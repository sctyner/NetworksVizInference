effects_dist <- read.csv("Data/simulation-1000-M1-M2-M3.csv")
effects_dist %>% group_by(model, effectname) %>% 
  dplyr::summarise(bhat = mean(estimate)) %>%
  filter(effectname !="alpha2") -> starting_values

lineups <- expand.grid(lineupname = c("smallfriends", "smallfriends-rev", "smallfriends-eff2", "smallfriends-eff2-rev" ),
                       M = c(3,6,9,12,16), 
                       rep = 1:20)
friend.data.w1 <- as.matrix(read.table("Data/s50_data/s50-network1.dat"))
friend.data.w3 <- as.matrix(read.table("Data/s50_data/s50-network3.dat"))
fd2.w1 <- friend.data.w1[20:35,20:35]
fd2.w3 <- friend.data.w3[20:35,20:35]

as.numeric(data.frame(starting_values %>% filter(model == "M1") %>% ungroup() %>%
  select_("bhat"))[,1]) -> sv_M1
as.numeric(data.frame(starting_values %>% filter(model == "M2") %>% ungroup() %>%
                        select_("bhat"))[,1]) -> sv_M2
as.numeric(data.frame(starting_values %>% filter(model == "M3") %>% ungroup() %>%
                        select_("bhat"))[,1]) -> sv_M3

estimates_from_lineup2 <- function(dat){
  lineupname <- dat$lineupname
  M <- dat$M
  rep <- dat$rep
  null_model_name <- "M1"
  null_model_eff_names <- c('alpha1', 'beta1', 'beta2')
  if (length(grep("eff2", lineupname)) == 0){
    covariate <- "alcohol2"
    alt_eff_name <- "jumpXTransTrip"
    inits <- sv_M2
    alt_model_name <- "M2"
    alt_model_eff_names <- c("alpha1", 'beta1', 'beta2', 'beta3')
  } else{
    covariate <- ""
    alt_eff_name <- "nbrDist2twice"
    inits <- sv_M3
    alt_model_name <- "M3"
    alt_model_eff_names <- c("alpha1", 'beta1', 'beta2', 'beta4')
  }
  require(RSiena)
  require(dplyr)
  
  # model setup
  myalgorithm <- sienaAlgorithmCreate( projname = 's50null' , n3 = 5000)
  filename <- paste0("Data/lineupdata/", lineupname, "-m-", M, "-rep-",rep, ".csv")
  lu_dat <- read.csv(filename)
  lu_dat_list <- NULL
  friendData <- NULL
  friendSiena <- NULL
  fullSienaData <- NULL
  for (m in 1:M){
    # create the data for each of the panels of the lineups.
    # the "alt" data is the Mth element of the list 
    lu_dat_list[[m]] <- as.matrix.network.adjacency(
      as.network(
        na.omit(dplyr::filter(lu_dat, count == m)[,c("X1","X2")]), 
        matrix.type = 'edgelist')
    )
    friendData[[m]] <- array( c( fd2.w1, lu_dat_list[[m]]),
                              dim = c( 16, 16, 2 ) )
    friendSiena[[m]] <- sienaDependent(friendData[[m]])
    fullSienaData[[m]] <- sienaDataCreate( friendSiena[[m]], alcohol2)
  }
  null_model_effects <- NULL
  alt_model_effects <- NULL
  fittedNullModels <- NULL
  fittedAltModels <- NULL
    for(m in 1:M){
      null_model_effects[[m]] <- getEffects(fullSienaData[[m]])
      null_model_effects[[m]]$initialValue[null_model_effects[[m]]$include] <- sv_M1
      fittedNullModels[[m]] <- siena07( myalgorithm, data = fullSienaData[[m]], 
                                    returnDeps = TRUE, effects = null_model_effects[[m]],
                                    batch=TRUE, verbose = FALSE, silent = TRUE)
      alt_model_effects[[m]] <- includeEffects(null_model_effects[[m]], alt_eff_name, 
                                               interaction1 = covariate, character = TRUE)
      alt_model_effects[[m]]$initialValue[alt_model_effects[[m]]$include] <- inits
      fittedAltModels[[m]] <- siena07( myalgorithm, data = fullSienaData[[m]],
                                  returnDeps = TRUE, effects = alt_model_effects[[m]],
                                  batch=TRUE, verbose = FALSE, silent = TRUE)
    }
  
    res <- data.frame(plot = c(rep(1:M, each = 3), rep(1:M, each = 4)), 
                      model = c(rep("M1", 3*M), rep(alt_model_name, 4*M)),
                      effectname = c(rep(null_model_eff_names, M), rep(alt_model_eff_names, M)),
                      estimate = 9999)
    for (j in 1:M){
      res$estimate[which(res$plot == j & res$model == "M1" )] <- c(fittedNullModels[[j]]$rate, 
                                                                  fittedNullModels[[j]]$theta)
      res$estimate[which(res$plot == j & res$model == alt_model_name )] <- c(fittedAltModels[[j]]$rate, 
                                                                            fittedAltModels[[j]]$theta)
      }
  return(res)
}

testing2 <- estimates_from_lineup2(dat = data.frame(lineupname = 'smallfriends', M = 3, rep = 1))
lineups$lineupid <- paste(lineups$lineupname, lineups$M, lineups$rep,sep = '-')
lineups$lineupname <- as.character(lineups$lineupname)


lineups %>% arrange(lineupname, M, rep) %>% nest(-lineupid) %>%
  mutate(fits = map(data, safely(estimates_from_lineup2))) -> lu_ests2

#readRDS("Data/estimates_from_lineups2.RDS")

#find out where there were errors
estVect <- rep(NA, nrow(lu_ests2))
for (i in 1:nrow(lu_ests2)){
  estVect[i] <- !is.null(lu_ests2$fits[[i]]$error)
}

# subset data so the runs that didn't finish don't get included in the estimate
lu_ests3 <- lu_ests2[-which(estVect == TRUE),]

lu_ests3$fits2 <- lapply(lu_ests3$fits, `[[`, 1)

lu_ests3 %>% unnest(data) %>% unnest(fits2) -> lu_ests4

lineups$filename <- paste0(lineups$lineupname, "-m-", lineups$M, "-rep-", lineups$rep, ".csv")

#setwd("~/Desktop/NetworksResearch/NetworksVizInference/Data/lineupdata/")
derp <- read.csv(lineups$filename[1])

lineups$altplot <- "hello"
for (i in 1:nrow(lineups)){
  derp <- read.csv(lineups$filename[[i]])
  lineups$altplot[i] <- derp$plot_order[which.max(derp$count)]
}

head(lineups)
lu_ans <- lineups[,c("lineupid", "altplot")]

names(lu_ests4)
head(lu_ests4)

merge(lu_ests4, lu_ans, by = "lineupid") -> lu_ests5

lu_ests5$isalt <- lu_ests5$plot == lu_ests5$altplot

lu_setup <- data.frame(lineupname = unique(lu_ests5$lineupname), 
                    altmodel = c("M2", "M3", "M1", "M1"),
                    nullmodel = c("M1","M1", "M3", "M2"))

merge(lu_ests5, lu_setup, by = "lineupname") -> lu_ests5

lu_ests5$matchnull <- lu_ests5$model == lu_ests5$nullmodel
lu_ests5$matchalt <- lu_ests5$model == lu_ests5$altmodel

ggplot(data = lu_ests5) + 
  geom_density(aes(x = estimate, fill = altmodel), alpha = .4) +
  xlim(c(-30,30)) + 
  facet_grid(isalt~effectname, scales = 'free')

lu_ests5 %>% group_by(model, nullmodel, matchnull, effectname) %>%
  dplyr::summarise(meanest = mean(estimate))

lu_ests5 %>% select(-c(altplot, isalt, matchnull, matchalt)) %>% 
       spread(model, estimate) %>% mutate(diff12 = ifelse(is.na(M1), M2, M1 - M2))%>% 
       select(-c(M1,M2)) %>% spread(effectname, diff12) -> thing

dim(thing)

for (name in unique(lu_ests5$lineupid)){
  filename <- paste0(name, "pcp")
  savepdf(file = filename, width = 8*2.54, height = 8*2.54)
  if (length(grep("eff2", name)) == 0){
    lu_ests5 %>% 
      select(-c(altplot, matchnull, matchalt)) %>% 
      spread(model, estimate) %>% 
      mutate(diff12 = ifelse(is.na(M1), M2, M1 - M2)) %>% 
      filter(lineupid == name) %>% 
      select(-c(M1,M2)) %>% spread(effectname, diff12) %>%
      ggparcoord(columns = 10:13) + aes(color = isalt) + geom_label(aes(label = plot, color = isalt)) + 
      scale_color_manual(values = c("black", "red")) -> plott
  } else {
  lu_ests5 %>% 
  select(-c(altplot, matchnull, matchalt)) %>% 
  spread(model, estimate) %>% 
  mutate(diff13 = ifelse(is.na(M1), M3, M1 - M3)) %>% 
  filter(lineupid == name) %>% 
  select(-c(M1,M3)) %>% spread(effectname, diff13) %>%
  ggparcoord(columns = 10:13) + aes(color = isalt) + geom_label(aes(label = plot, color = isalt)) + 
      scale_color_manual(values = c("black", "red")) -> plott
  }
  print(plott)
  dev.off()
}

savepdf <- function(file, width=16, height=10)
{
  fname <- paste(file,".pdf",sep="")
  pdf(fname, width=width/2.54, height=height/2.54,
      pointsize=10)
  par(mgp=c(2.2,0.45,0), tcl=-0.4, mar=c(3.3,3.5,1.1,1))
}