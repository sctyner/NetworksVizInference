# write a function to fit simulation to data. just save the object in the data
# object using purrr to pull out all information later that might be of interest.

effects_dist <- read.csv("Data/simulation-1000-M1-M2-M3.csv")
effects_dist %>% group_by(model, effectname) %>% 
  dplyr::summarise(bhat = mean(estimate)) %>%
  filter(effectname !="alpha2") -> starting_values

sv_m1 <- data.frame(starting_values %>% filter(model == "M1") %>% dplyr::select(bhat))[,2] %>% as.numeric()
sv_m2 <- data.frame(starting_values %>% filter(model == "M2") %>% dplyr::select(bhat))[,2] %>% as.numeric()
sv_m3 <- data.frame(starting_values %>% filter(model == "M3") %>% dplyr::select(bhat))[,2] %>% as.numeric()

lineups <- expand.grid(lineupname = c("smallfriends", "smallfriends-rev", "smallfriends-eff2", "smallfriends-eff2-rev" ),
                       M = c(3,6,9,12,16), 
                       rep = 1:20)
lineups$lineupid <- paste(lineups$lineupname, lineups$M, lineups$rep,sep = '-')
friend.data.w1 <- as.matrix(read.table("Data/s50_data/s50-network1.dat"))
fd2.w1 <- friend.data.w1[20:35,20:35]

fit_all_models <- function(dat){
  require(RSiena)
  require(network)
  lineupname <- dat$lineupname
  M <- dat$M
  rep <- dat$rep
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
  fittedNullModels <- NULL
  m2_model_effects <- NULL
  fittedM2Models <- NULL
  m3_model_effects <- NULL
  fittedM3Models <- NULL
  
  for(m in 1:M){
    null_model_effects[[m]] <- getEffects(fullSienaData[[m]])
    null_model_effects[[m]]$initialValue[null_model_effects[[m]]$include] <- sv_m1
    fittedNullModels[[m]] <- siena07( myalgorithm, data = fullSienaData[[m]], 
                                      returnDeps = TRUE, effects = null_model_effects[[m]],
                                      batch=TRUE, verbose = FALSE, silent = TRUE)
    m2_model_effects[[m]] <- includeEffects(null_model_effects[[m]], "jumpXTransTrip", 
                                            interaction1 = "alcohol2", character = TRUE)
    m2_model_effects[[m]]$initialValue[m2_model_effects[[m]]$include] <- sv_m2
    fittedM2Models[[m]] <- siena07( myalgorithm, data = fullSienaData[[m]],
                                    returnDeps = TRUE, effects = m2_model_effects[[m]],
                                    batch=TRUE, verbose = FALSE, silent = TRUE)
    m3_model_effects[[m]] <- includeEffects(null_model_effects[[m]], "nbrDist2twice", character = TRUE)
    m3_model_effects[[m]]$initialValue[m3_model_effects[[m]]$include] <- sv_m3
    fittedM3Models[[m]] <- siena07( myalgorithm, data = fullSienaData[[m]],
                                    returnDeps = TRUE, effects = m3_model_effects[[m]],
                                    batch=TRUE, verbose = FALSE, silent = TRUE)
  }
  return(list(M1 = fittedNullModels, M2 = fittedM2Models, M3 = fittedM3Models))
}


library(tidyr)
library(purrr)
lineups %>% arrange(lineupname, M, rep) %>% nest(-lineupid) -> nested_lus 

fit_all_models_safely <- safely(fit_all_models)

fitted_models_from_lineups <- nested_lus$data %>% map(fit_all_models_safely)

saveRDS(fitted_models_from_lineups, file = "Data/lineupdata/fittedM123_from_lineups.RDS")