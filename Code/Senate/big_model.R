# simulate some observations from the "big model" 
library(RSiena)
bigModEsts <- read_csv("Data/senate/bigModFitsSenate.csv")

bigModEsts %>% summarise_all(mean) %>% select(Rate:simXTransTrip) %>% as.numeric -> bigModMeans
load("Data/senate/senateSienaNoHRC.rda")
SenBasic <- getEffects(senateSiena)
Sen_all_nojttp <- includeEffects(SenBasic, "jumpXTransTrip", include = TRUE, type = "eval", interaction1 = "sex", character = TRUE)
Sen_all_nojttp <- includeEffects(Sen_all_nojttp, "simXTransTrip", include = TRUE, type = "eval", interaction1 = "bills", character = TRUE)
Sen_all_nojttp <- includeEffects(Sen_all_nojttp, "sameXTransTrip", include = TRUE, type = "eval", interaction1 = "party", character = TRUE)

saom_simulate2 <- function(dat, struct, parms, N) {
  require(RSiena)
  struct$initialValue[struct$include] <- parms
  myalgorithm <- sienaAlgorithmCreate(projname = Sys.time(), 
                                      useStdInits = FALSE, cond = TRUE, nsub = 0, simOnly = TRUE, 
                                      n3 = N)
  getsims <- siena07(myalgorithm, data = dat, returnDeps = TRUE, 
                     effects = struct, batch = TRUE, verbose = FALSE, silent = TRUE)
  return(getsims$sims)
}
# turn simulation lists into dfs 
sims_to_df <- function(sims, dat) {
  Nact <- length(dat$nodeSets$Actors)
  N <- length(sims)
  waves <- length(sims[[1]][[1]][[1]])
  simsdf <- NULL
  counter <- 1
  for (i in 1:N) {
    for (j in 1:waves) {
      dat <- as.data.frame(sims[[i]][[1]][[1]][[j]])
      names(dat) <- c("from", "to", "dep.var.id")
      ids <- 1:Nact
      nodes <- data.frame(id = ids)
      dat2 <- merge(dat, nodes, by.x = "from", by.y = "id", 
                    all = T)
      dat2$wave = j
      dat2$sim = i
      simsdf[[counter]] <- dat2
      counter <- counter + 1
    }
  }
  mydf <- dplyr::bind_rows(simsdf)
  return(mydf)
}

SimsBigMod <- saom_simulate2(dat = senateSiena,
                           struct = Sen_all_nojttp,
                           parms = bigModMeans,
                           N = 100)
SimsBigModdf <- sims_to_df(SimsBigMod, senateSiena)
SimsBigModdf <- SimsBigModdf %>% filter(wave == 1)
SimsBigModdf$model <- "bigmod"
ids <- 1:length(senateSiena$nodeSets$Actors)
nodes <- data.frame(id = rep(ids, max(SimsBigModdf$sim)), 
                    sim = rep(unique(SimsBigModdf$sim), each = max(ids)),
                    wave = 1, model = "bigmod")
SimsBigModdf <- merge(SimsBigModdf, nodes, by.x = c("from", "wave", "sim", "model"), 
                 by.y = c("id", "wave", "sim", "model"), all = T)
SimsBigModdf <- select(SimsBigModdf, from, to, dep.var.id, wave, sim, model) %>% arrange(sim, from, to)

idx <- sample(1:max(SimsBigModdf$sim), 15)
idx
idx1 <- idx[1:5]
idx2 <- idx[6:10]
idx3 <- idx[11:15]

load("Data/senate/se112adjmat.RDS")
se112adj
wave2 <- data.frame(se112adj)
senators <- data.frame(id = colnames(wave2), number = 1:155)
wave2$from <- colnames(wave2)
wave2 <- wave2 %>% gather(to, val, Alan.Stuart.Franken:William.Cowan) %>% filter(val > 0)
wave2 <- merge(wave2, senators, by.x = "from", by.y = "id", all = T)
wave2$from <- senators$number[match(wave2$from, senators$id)]
wave2$to <- senators$number[match(wave2$to, senators$id)]
wave2$from <- paste0("V", wave2$from)
wave2$to <- ifelse(is.na(wave2$to), NA, paste0("V", wave2$to))
wave2 <- wave2 %>% mutate(sim = 1001, model = "data", wave = 1) %>% select(-c(val, number))


SimsBigModdf %>% filter(sim %in% idx1) %>%
  mutate(from = paste0("V", from), to = ifelse(is.na(to), NA, paste0("V", to))) %>% 
  bind_rows(wave2) -> gof_bigmod_1
gof_bigmod_1$ord <- rep(sample(6,6), as.numeric(table(gof_bigmod_1$sim)))
SimsBigModdf %>% filter(sim %in% idx2) %>%
  mutate(from = paste0("V", from), to = ifelse(is.na(to), NA, paste0("V", to))) %>% 
  bind_rows(wave2) -> gof_bigmod_2
gof_bigmod_2$ord <- rep(sample(6,6), as.numeric(table(gof_bigmod_2$sim)))
SimsBigModdf %>% filter(sim %in% idx3) %>%
  mutate(from = paste0("V", from), to = ifelse(is.na(to), NA, paste0("V", to))) %>% 
  bind_rows(wave2) -> gof_bigmod_3
gof_bigmod_3$ord <- rep(sample(6,6), as.numeric(table(gof_bigmod_3$sim)))



ggplot(data = gof_bigmod_1) + 
  geom_net(aes(from_id = from, to_id = to), 
           arrow = arrow(type = 'open', length = unit(2, "points") ), 
           linewidth = .25, singletons = T, fiteach = T, directed = T, 
           color = 'black', arrowgap = .015, arrowsize = .3, size = .3) + 
  theme_net() + 
  theme(panel.background = element_rect(color = 'black')) +
  facet_wrap(~ord)

ggplot(data = gof_bigmod_2) + 
  geom_net(aes(from_id = from, to_id = to), 
           arrow = arrow(type = 'open', length = unit(2, "points") ), 
           linewidth = .25, singletons = T, fiteach = T, directed = T, 
           color = 'black', arrowgap = .015, arrowsize = .3, size = .3) + 
  theme_net() + 
  theme(panel.background = element_rect(color = 'black')) +
  facet_wrap(~ord)

ggplot(data = gof_bigmod_3) + 
  geom_net(aes(from_id = from, to_id = to), 
           arrow = arrow(type = 'open', length = unit(2, "points") ), 
           linewidth = .25, singletons = T, fiteach = T, directed = T, 
           color = 'black', arrowgap = .015, arrowsize = .3, size = .3) + 
  theme_net() + 
  theme(panel.background = element_rect(color = 'black')) +
  facet_wrap(~ord)

write_csv(gof_bigmod_1, "SenateLineupData/GoF/bigmod_gof_9_1.csv")
write_csv(gof_bigmod_2, "SenateLineupData/GoF/bigmod_gof_9_2.csv")
write_csv(gof_bigmod_3, "SenateLineupData/GoF/bigmod_gof_9_3.csv")



