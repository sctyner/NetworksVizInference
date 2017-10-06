# Simulating "data" from the models
setwd("~/Desktop/Dissertation/NetworksVizInference/")
library(RSiena)
library(netvizinf)
library(geomnet)

load("Data/senate/senateSienaNoHRC.rda")
# basic effects structure
SenBasic <- getEffects(senateSiena)
# basic algorithm structure
myalg <- sienaAlgorithmCreate( projname = Sys.time() , n3 = 1000)
# additional effects structures
# jumpXTransTrip party
Senjtt_p <- includeEffects(SenBasic, "jumpXTransTrip", include = TRUE, type = "eval", interaction1 = "party", character = TRUE)
# jumpXTransTrip sex
Senjtt_s <- includeEffects(SenBasic, "jumpXTransTrip", include = TRUE, type = "eval", interaction1 = "sex", character = TRUE) 
# indegree isolates
# SeninIsD <- includeEffects(SenBasic, "inIsDegree", include = TRUE, type = "eval", interaction1 = "", character = TRUE)
# sameX party
Sensame_p <- includeEffects(SenBasic, "sameX", include = TRUE, type = "eval", interaction1 = "party", character = TRUE)
# simXTransTrip bills
Senstt_b <- includeEffects(SenBasic, "simXTransTrip", include = TRUE, type = "eval", interaction1 = "bills", character = TRUE)
# sim recip party
#Sensimre_p <- includeEffects(SenBasic, "simRecipX", include = TRUE, type = "eval", interaction1 = "party", character = TRUE)
Sentt_p <- includeEffects(SenBasic, "transTrip", include = TRUE, type = "eval", interaction1 = "", character = TRUE)
Senstt_p <- includeEffects(SenBasic, "sameXTransTrip", include = TRUE, type = "eval", interaction1 = "party", character = TRUE)

# means of parameters to use as starting values
M1senateEsts <- read_csv("Data/senate/basicModelFitsSenate1000.csv")
#inisdSenateEsts <- read_csv("Data/senate/inisdModelFitsSenate1000.csv")
jttpSenateEsts <- read_csv("Data/senate/jttpModelFitsSenateAll1000.csv")
jttsSenateEsts <- read_csv("Data/senate/jttsModelFitsSenateAll1000.csv")
samepSenateEsts <- read_csv("Data/senate/samepModelFitsSenateall1000.csv")
simttbSenateEsts <- read_csv("Data/senate/simttbModelFitsSenateAll1000.csv")
samettpSenateEsts <- read_csv("Data/senate/samettpModelFitsSenateAll1000.csv")


# get means 
M1SenMeans <- M1senateEsts %>% filter(maxConv <=0.25) %>% summarise_at(.cols = 1:5, .funs = mean) %>% as.numeric()
M1SenNconv <-  M1senateEsts %>% filter(maxConv <=0.25) %>% count() %>% as.numeric
jttpSenMeans <- jttpSenateEsts %>% filter(maxConv <=0.25) %>% summarise_at(.cols = 2:7, .funs = mean) %>% as.numeric()
jttpSenNconv <- jttpSenateEsts %>% filter(maxConv <=0.25) %>% count() %>% as.numeric
jttsSenMeans <- jttsSenateEsts %>% filter(maxConv <=0.25) %>% summarise_at(.cols = 2:7, .funs = mean) %>% as.numeric()
jttsSenNconv <- jttsSenateEsts %>% filter(maxConv <=0.25) %>% count() %>% as.numeric
samepSenMeans <- samepSenateEsts %>% filter(maxConv <=0.25) %>% summarise_at(.cols = 2:7, .funs = mean) %>% as.numeric()
samePSenNconv <- samepSenateEsts %>% filter(maxConv <=0.25) %>% count() %>% as.numeric
simttbSenMeans <- simttbSenateEsts %>% filter(maxConv <= 0.25) %>% summarise_at(.cols = 2:7, .funs = mean) %>% as.numeric()
simttbSenNconv <- simttbSenateEsts %>% filter(maxConv <=0.25) %>% count() %>% as.numeric
samettpSenMeans <- samettpSenateEsts %>% filter(maxConv <= 0.25) %>% summarise_at(.cols = 2:7, .funs = mean) %>% as.numeric()
samettpSenNconv <- samettpSenateEsts %>% filter(maxConv <=0.25) %>% count() %>% as.numeric


# 
# check conditional v non-conditional simulation
# myalgorithmCond <- sienaAlgorithmCreate(projname = Sys.time(), 
#                                     useStdInits = FALSE, cond = TRUE, nsub = 0, simOnly = TRUE, 
#                                     n3 = 100)
# myalgorithmNonCond <- sienaAlgorithmCreate(projname = Sys.time(), 
#                                         useStdInits = FALSE, cond = FALSE, nsub = 0, simOnly = TRUE, 
#                                         n3 = 100)
# SenBasic2 <- SenBasic
# SenBasic2$initialValue[SenBasic2$include] <- M1SenMeans
# set.seed(4573921)
# simsCond <- siena07(myalgorithmCond, data = senateSiena, returnDeps = TRUE, 
#                    effects = SenBasic2, batch = TRUE, verbose = FALSE, silent = TRUE)
# simsCond <- simsCond$sims
# set.seed(4573921)
# simsNonCond <- siena07(myalgorithmNonCond, data = senateSiena, returnDeps = TRUE, 
#                     effects = SenBasic2, batch = TRUE, verbose = FALSE, silent = TRUE)
# simsNonCond <- simsNonCond$sims
# simsCond <- sims_to_df(simsCond)
# simsNonCond <- sims_to_df(simsNonCond)
# 
# simsCond %>% filter(sim == 2) %>% 
#   mutate(from = paste0("V", from), to = ifelse(is.na(to), to, paste0("V", to))) %>% 
#   ggplot() + geom_net(aes(from_id = from, to_id = to), fiteach = T, size = 1) + 
#   theme_net() + facet_grid(.~wave)
# simsNonCond %>% filter(sim == 2) %>% 
#   mutate(from = paste0("V", from), to = ifelse(is.na(to), to, paste0("V", to))) %>% 
#   ggplot() + geom_net(aes(from_id = from, to_id = to), fiteach = T, size = 1) + 
#   theme_net() + facet_grid(.~wave)
### MORAL: Do conditional simulation. Redo sims 10-1-17.

######################################################################################################
# Dont need to do again! (after 10-1-17) 
# simulate "data" 
# set a seed
saom_simulate2 <- function (dat, struct, parms, N) {
  require(RSiena)
  struct$initialValue[struct$include] <- parms
  myalgorithm <- sienaAlgorithmCreate(projname = Sys.time(), 
                                      useStdInits = FALSE, cond = TRUE, nsub = 0, simOnly = TRUE, 
                                      n3 = N)
  getsims <- siena07(myalgorithm, data = dat, returnDeps = TRUE, 
                     effects = struct, batch = TRUE, verbose = FALSE, silent = TRUE)
  return(getsims$sims)
}
set.seed(271498)
M1dat <- saom_simulate2(dat = senateSiena, struct = SenBasic, parms = M1SenMeans, N = 100)
jttpdat <- saom_simulate2(dat = senateSiena, struct = Senjtt_p, parms = jttpSenMeans, N = 100)
jttsdat <- saom_simulate2(dat = senateSiena, struct = Senjtt_s, parms = jttsSenMeans, N = 100)
samepdat <- saom_simulate2(dat = senateSiena, struct = Sensame_p, parms = samepSenMeans, N=100)
simttbdat <- saom_simulate2(dat = senateSiena, struct = Senstt_b, parms = simttbSenMeans, N=100)
samettpdat <- saom_simulate(dat = senateSiena, struct = Senstt_p, parms = samettpSenMeans, N = 100)

M1datdf <- sims_to_df(M1dat)
jttpdatdf <- sims_to_df(jttpdat)
jttsdatdf <- sims_to_df(jttsdat)
samepdatdf <- sims_to_df(samepdat)
simttbdatdf <- sims_to_df(simttbdat)
samettpdatdf <- sims_to_df(samettpdat)

write_csv(M1datdf, 'Data/senate/FauxTrueData/M1senSimDat.csv')
write_csv(jttpdatdf, 'Data/senate/FauxTrueData/jttpSenSimDat.csv')
write_csv(jttsdatdf, 'Data/senate/FauxTrueData//jttsSenSimDat.csv')
write_csv(samepdatdf, 'Data/senate/FauxTrueData/samepSenSimDat.csv')
write_csv(simttbdatdf, "Data/senate/FauxTrueData/simttbSenSimDat.csv")
write_csv(samettpdatdf, "Data/senate/FauxTrueData/samettpSenSimDat.csv")
######################################################################################################



M1datdf %>% group_by(wave, from, to) %>% summarise(count = n()) %>% ungroup() %>% 
  mutate(from = paste0("sen", from), to = paste0("sen", to)) %>% 
  filter(count >= 200) 

jttpdatdf %>% group_by(wave, from, to) %>% summarise(count = n()) %>% ungroup() %>% 
  mutate(from = paste0("sen", from), to = ifelse(is.na(to), NA, paste0("sen", to))) %>% 
  filter(count >=200) %>% data.frame()
ggplot() + 
  geom_net(layout.alg = "kamadakawai", aes(from_id = from, to_id = to, linewidth = count/858),  fiteach = T) + 
  theme_net() + 
  facet_wrap(~wave)
dummyorder <- sample(5)
M1datdf %>% filter(wave==1, sim %in% 1:4) %>% 
  mutate(from = paste0("V", from), to = ifelse(is.na(to), to, paste0("V", to))) %>%
  add_row(from = sen111$from, to = sen111$to, sim = 1001) %>% 
  mutate(order = rep(dummyorder, c(146,122,134,124, 130))) %>% 
  ggplot() + 
  geom_net(layout.alg = "fruchtermanreingold",directed = T,arrowsize = .2,
           aes(from_id = from, to_id = to),  fiteach = T, size = 1, linewidth = .25, color = 'navy') + 
  theme_net() + 
  facet_wrap(~order)

sen111 <- geomnet:::fortify.adjmat(geomnet::as.adjmat(senateData[,,1]))
sen111$from <- paste0("V", sen111$from)

ggplot(sen111) + 
  geom_net(layout.alg = "fruchtermanreingold",directed = T,arrowsize = .2,
           aes(from_id = from, to_id = to), fiteach = T, size = 1, linewidth = .25, color = 'navy') + 
  theme_net()
    
