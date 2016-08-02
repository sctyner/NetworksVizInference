# look at the ones with high jtt, low beta hat.
library(dplyr)
library(tidyr)
library(purrr)
weirdones <- subset(lus_summary_2, param_name=="transitive triplets jumping alcohol2" & jtt > 20)
weirdones2 <- data.frame(lineupid = weirdones$lineupid)
weirdones2 %>% separate(lineupid, into = c("lineupname", "M", "rep"), sep = "-") -> weirdones2
weirdones2$lineupid <- weirdones$lineupid
weirdones2$M[10:12] <- 6 
weirdones2$rep[10:12] <- c(20, 5, 9)

weirdones2 %>% nest(-lineupid) -> weirdones3

effects_dist <- read.csv("Data/simulation-1000-M1-M2-M3.csv")
m1ests <- readr::read_csv("Data/distribution_null_model.csv")
head(m1ests)
effects_dist %>% group_by(Model, parameter) %>% 
  dplyr::summarise(bhat = mean(estimate)) %>%
  filter(parameter !="alpha2") -> starting_values

sv_m1 <- data.frame(starting_values %>% filter(Model == "M1") %>% dplyr::select(bhat))[,2] %>% as.numeric()
sv_m2 <- data.frame(starting_values %>% filter(Model == "M2") %>% dplyr::select(bhat))[,2] %>% as.numeric()
sv_m3 <- data.frame(starting_values %>% filter(Model == "M3") %>% dplyr::select(bhat))[,2] %>% as.numeric()

lineups <- expand.grid(lineupname = c("smallfriends", "smallfriends-rev", "smallfriends-eff2", "smallfriends-eff2-rev" ),
                       M = c(3,6,9,12,16), 
                       rep = 1:20)
lineups$lineupid <- paste(lineups$lineupname, lineups$M, lineups$rep,sep = '-')
drink <- as.matrix(read.table("Data/s50_data/s50-alcohol.dat"))
drink2 <- drink[20:35,]
# siena data
alcohol2 <- coCovar( as.matrix(drink2)[,2] )
# create siena data object

# refit the ones that are weird

fit_all_models_redo <- function(dat){
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
                                      returnDeps = FALSE, effects = null_model_effects[[m]],
                                      batch=TRUE, verbose = FALSE, silent = TRUE)
    m2_model_effects[[m]] <- includeEffects(null_model_effects[[m]], "jumpXTransTrip", 
                                            interaction1 = "alcohol2", character = TRUE)
    m2_model_effects[[m]]$initialValue[m2_model_effects[[m]]$include] <- sv_m2
    fittedM2Models[[m]] <- siena07( myalgorithm, data = fullSienaData[[m]],
                                    returnDeps = FALSE, effects = m2_model_effects[[m]],
                                    batch=TRUE, verbose = FALSE, silent = TRUE)
    m3_model_effects[[m]] <- includeEffects(null_model_effects[[m]], "nbrDist2twice", character = TRUE)
    m3_model_effects[[m]]$initialValue[m3_model_effects[[m]]$include] <- sv_m3
    fittedM3Models[[m]] <- siena07( myalgorithm, data = fullSienaData[[m]],
                                    returnDeps = FALSE, effects = m3_model_effects[[m]],
                                    batch=TRUE, verbose = FALSE, silent = TRUE)
  }
  filename2 <- paste0("Data/WeirdModelRefits/",lineupname, "-", M, "-", rep, ".RDA")
  res <- list(M1 = fittedNullModels, M2 = fittedM2Models, M3 = fittedM3Models)
  save(res, file = filename2)
  return(res)
}
weirdones3$data[[1]] -> dat

for(i in 1:nrow(weirdones3)){
  fit_all_models_redo(dat = weirdones3$data[[i]])
}

get_fit_info <- function(dat){
  lineupname <- dat$lineupname
  M <- dat$M
  rep <- dat$rep
  filename <- paste0("Data/ModelFitstoLineupData/",lineupname,"-",M, "-",rep,".RDA")
  load(filename)
  
  model_num <- length(res)
  panel_num <- length(res[[1]])
  
  # return a data frame with 11 rows for each panel: 3 rows for M1 and 4 rows for M2, M3.
  vals <- data.frame(matrix(NA, nrow = 11*panel_num, ncol = 8))
  names(vals) <- c("model", "panel_num", "param_name", "param_est", "param_est_se", 
                   "converg_stat", "time", "termination")
  
  for(i in 1:model_num){
    model_name <- paste0("M", i)
    
    for(j in 1:panel_num){
      dat <- res[[i]][[j]]
      idx_start <- length(which(!is.na(vals$model))) + 1
      idx_end <- idx_start + length(c("rate", dat$requestedEffects$effectName)) - 1
      subvals <- data.frame(model = model_name,
                            panel_num = j,
                            param_name = c("rate", dat$requestedEffects$effectName),
                            param_est = c(dat$rate, dat$theta), 
                            param_est_se = c(dat$vrate, sqrt(diag(dat$covtheta))),
                            converg_stat = c(dat$tconv.max, dat$tconv),
                            time = dat$ctime, termination = dat$termination,
                            stringsAsFactors = FALSE, 
                            row.names = idx_start:idx_end)
      
      vals[idx_start:idx_end,] <- subvals
    }
  }
  
  return(vals)
}

weirdones3 %>% mutate(fit_info = map(data, get_fit_info)) -> weirdones_refit
weirdones_refit[weirdones_refit$lineupid == "smallfriends-6-17","fit_info"][[1]]
lus_ests_truth[lus_ests_truth$lineupid == "smallfriends-6-17",]
# okay run the code from HH on the weird ones refit. 
# 

library(geomnet)
# redo jtts 
dframe <- data.frame(dframe)
dframe$filename <- lineupdata[as.numeric(dframe$filename)]


weirdones_refit %>% mutate(jtts = map(data, get_jtts)) -> weirdones_refit
