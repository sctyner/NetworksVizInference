# extracting all the information from the rsiena models fitted to the 
# 400 lineups of all types!!! 

# lineups - located in Data/lineupdata
# fitted models to lineups - located in Data/ModelFitstoLineupData

# M1, M2, M3 were fit to every panel of every lineup.

# load a fitted object
load("Data/ModelFitstoLineupData/smallfriends-6-1.RDA")
#it's named res.

length(res)
# there are 3 objects in res. res[[i]] is the result of fitting model i
# to all M panels. 

length(res[[1]])
# there are M objects in each res object. res[[i]][[j]] is the Rsiena
# object resulting from fitting model i to panel j in the given lineup

# pull all the information out of each of the fitted models. what info
# do i want? parameter estimates, standard errors, convergence statistics 

# repeat for each model!!! 
test <- res[[1]]
# and for each lineup! 
test2 <- res[[1]][[1]]

# non-rate parameter estimates.
test2$theta

# std errors of non-rate parameter estimates
sqrt(diag(test2$covtheta))

# rate parameter estimates
test2$rate

# std error of rate parameter estimates
test2$vrate

# time elapsed
test2$ctime

# termination
test2$termination

# convergence for non-rate parameters
test2$tconv

# overall convergence
test2$tconv.max

# name of beta effects in model
test2$requestedEffects$effectName

lineups <- expand.grid(lineupname = c("smallfriends", "smallfriends-rev", "smallfriends-eff2", "smallfriends-eff2-rev" ),
                       M = c(3,6,9,12,16), 
                       rep = 1:20)
lineups$lineupid <- paste(lineups$lineupname, lineups$M, lineups$rep,sep = '-')

# column names to return: 
# effect name, effect estimate, se effect estimate, converge stat, time, termination

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

library(dplyr)
library(tidyr)
library(purrr)

nested_lus <- lineups %>% nest(-lineupid) 

testfn <- nested_lus[1:6, ]

# note : convergence stat for rate is the overall max convergence ratio since
# there is no measure of convergence for rate parameters
testfn %>% mutate(fit_info = map(data, get_fit_info)) -> testfnres
# it workss!!!!!

nested_lus %>% mutate(fit_info = map(data, get_fit_info)) -> nested_lus_res

# weird. "smallfriends-eff2-rev-6-20.RDA" is missing. what others are missing?

try_loading <- function(dat){
  lineupname <- dat$lineupname
  M <- dat$M
  rep <- dat$rep
  filename <- paste0("Data/ModelFitstoLineupData/",lineupname,"-",M, "-",rep,".RDA")
  testing <- try(expr = load(filename))
  return(testing)
}  

nested_lus %>% mutate(tryload = map(data, try_loading)) -> try_loadeds 
which(try_loadeds$tryload != 'res')

# nothing else missing. redo that one.

# okay, it won't write it. ignore for now.


nested_lus[-388,] %>% mutate(fit_info = map(data, get_fit_info)) -> nested_lus_res

#######################################################
#######################################################
########### Begin Analyses ###########################
#######################################################
#######################################################

summary(nested_lus_res)

unnest(nested_lus_res, data) %>% filter(M == 3) %>% unnest() -> lus_res_m3
unnest(nested_lus_res, data) %>% filter(M == 6) %>% unnest() -> lus_res_m6
unnest(nested_lus_res, data) %>% filter(M == 9) %>% unnest() -> lus_res_m9
unnest(nested_lus_res, data) %>% filter(M == 12) %>% unnest() -> lus_res_m12
unnest(nested_lus_res, data) %>% filter(M == 16) %>% unnest() -> lus_res_m16


lus_res_long <- rbind(lus_res_m3, lus_res_m6, lus_res_m9,
                      lus_res_m12, lus_res_m16)

# plots 


