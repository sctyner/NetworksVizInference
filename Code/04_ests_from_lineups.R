# Read in lineup data and re-fit the model and store the estimates
# in function: null_mod_eff is the effects structure w/4 effects and 
# alt_mod_eff is the effects structure w/5 effects (the 4 + additional)
# give lineupname either "smallfriends", "smallfriends-rev", "smallfriends-eff2" or "smallfriends-eff2-rev" 

#TO DO:
# 1. get original first wave of data + alcohol covariate
# 2. create siena data object for each lineup data plot (M data objs)
# 3. fit appropriate siena model to each of the M data objects
# 4. store effects in a purrr data frame

lineups <- expand.grid(lineupname = c("smallfriends", "smallfriends-rev", "smallfriends-eff2", "smallfriends-eff2-rev" ),
                       M = c(3,6,9,12,16), 
                       rep = 1:20)
friend.data.w1 <- as.matrix(read.table("Data/s50_data/s50-network1.dat"))
friend.data.w3 <- as.matrix(read.table("Data/s50_data/s50-network3.dat"))
fd2.w1 <- friend.data.w1[20:35,20:35]
fd2.w3 <- friend.data.w3[20:35,20:35]

drink <- as.matrix(read.table("Data/s50_data/s50-alcohol.dat"))
drink2 <- drink[20:35,1]
alcohol2 <- coCovar( as.vector(drink2) )
mysmalldata <- sienaDataCreate( friend2, alcohol2)

#get starting values for the simulations from the distributions
effects_dist <- read.csv("Data/simulation-1000-M1-M2-M3.csv")
effects_dist %>% group_by(model, effectname) %>% dplyr::summarise(bhat = mean(estimate))

estimates_from_lineup <- function(dat, alt_eff_name, covariate = "alcohol2"){
  lineupname <- dat$lineupname
  M <- dat$M
  rep <- dat$rep
  
  require(RSiena)
  require(dplyr)
#   if (length(grep("rev", lineupname)) == 0){
#     null_model_effects <- null_mod_eff # which effects to use for estimation. 
#     # "null" means the model with M-1 plots in lineup, while "alt" is the "test plot" 
#     alt_model_effects <- alt_mod_eff
#   } else{
#     null_model_effects <- alt_mod_eff # for the "rev" lineups
#     alt_model_effects <- null_mod_eff
#   }
  
  # model setup
  myalgorithm_null <- sienaAlgorithmCreate( projname = 's50null' , n3 = 1000)
  myalgorithm_alt <- sienaAlgorithmCreate( projname = 's50null' , n3 = 20000)
  # null_net <- siena07( myalgorithm2, data = my_dat, returnDeps = TRUE, effects = null_eff_struct, batch=TRUE, verbose = FALSE)
  
  
#  for (m in M) {# M is a vector of lineup sizes
#    for(r in 1:reps){
      filename <- paste0("Data/lineupdata/", lineupname, "-m-", M, "-rep-",rep, ".csv")
      lu_dat <- read.csv(filename)
      lu_dat_list <- NULL
      friendData <- NULL
      friendSiena <- NULL
      fullSienaData <- NULL
      for (m in 1:M){
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
        
        #test_model_eff2 <- includeEffects( null_model_eff2, egoX, altX, egoXaltX, interaction1 = "alcohol2")
      }
      null_model_effects <- NULL
      alt_model_effects <- NULL
      fittedModels <- NULL
      # for M-1 M_1 plots, and 1 M_2/M_3 plot
      if (length(grep("rev", lineupname)) == 0){
        for(m in 1:(M-1)){
          null_model_effects[[m]] <- getEffects(fullSienaData[[m]])
          fittedModels[[m]] <- siena07( myalgorithm_null, data = fullSienaData[[m]], 
                                    returnDeps = TRUE, effects = null_model_effects[[m]],
                                    batch=TRUE, verbose = FALSE, silent = TRUE)
        }
        alt_model_effects <- getEffects(fullSienaData[[M]])
        alt_model_effects <- includeEffects(alt_model_effects, alt_eff_name, interaction1 = covariate, character = TRUE)
        fittedModels[[M]] <- siena07( myalgorithm_alt, data = fullSienaData[[M]],
                                    returnDeps = TRUE, effects = alt_model_effects,
                                    batch=TRUE, verbose = FALSE, silent = TRUE)
        res <- data.frame(plot = rep(1:M, c(rep(3,M-1), 4)), 
                          effects = rep(NA, sum(c(rep(3,M-1), 4)))
                          )
        for (j in 1:M){
          res$effects[which(res$plot == j)] <- c(fittedModels[[j]]$rate, fittedModels[[j]]$theta)
        }
      } else { #for the "rev" lineups
        for(m in 1:(M-1)){
          alt_model_effects[[m]] <- getEffects(fullSienaData[[m]])
          alt_model_effects[[m]] <- includeEffects(alt_model_effects[[m]], alt_eff_name, interaction1 = covariate, character = TRUE)
          fittedModels[[m]] <- siena07( myalgorithm_alt, data = fullSienaData[[m]],
                                      returnDeps = TRUE, effects = alt_model_effects[[m]],
                                      batch=TRUE, verbose = FALSE, silent = TRUE)
        }
        null_model_effects <- getEffects(fullSienaData[[M]])
        fittedModels[[M]] <- siena07( myalgorithm_null, data = fullSienaData[[M]], 
                                      returnDeps = TRUE, effects = null_model_effects,
                                      batch=TRUE, verbose = FALSE, silent = TRUE)
        res <- data.frame(plot = rep(1:M, c(rep(4,M-1), 3)), 
                          effects = rep(NA, sum(c(rep(4,M-1), 3)))
                          )
        for (j in 1:M){
          res$effects[which(res$plot == j)] <- c(fittedModels[[j]]$rate, fittedModels[[j]]$theta)
        }
      }
#       res <- NULL
#       for (j in 1:M){
#         res[[j]] <- c(fittedModels[[j]]$rate, fittedModels[[j]]$theta)
#       }
    return(res)
}

testing <- estimates_from_lineup(dat = nest_lu, alt_eff_name = "jumpXTransTrip")
# it works! :) 

# IDEA: set the starting values for the fitting to the original mean estimates
# from the 1000 iterations to make convergence faster.

# now do it for all! 

lineups$lineupid <- paste(lineups$lineupname, lineups$M, lineups$rep,sep = '-')
lineups$lineupname <- as.character(lineups$lineupname)

nest_lu <- lineups %>% arrange(lineupname, M, rep) %>% nest(-lineupid)


lineups %>% arrange(lineupname, M, rep) %>% nest(-lineupid) %>%
  mutate(fits = map(data, safely(estimates_from_lineup),alt_eff_name = "jumpXTransTrip")) -> lu_ests_eric

#lu_ests_eric <- readRDS("Data/ests_from_lineup.RDS")

lu_ests_eric %>% unnest(data) -> unnest_ests 
library(plyr)

lu_ests_eric %>% unnest(data) %>% 
  select(fits) %>% flatten() -> listRes 


erridx <- NULL
for (i in 1:length(listRes)){
  if(!is.null(listRes[[i]]$error)){
    erridx <- c(erridx, i)
  } else {erridx <- erridx}
}

erridx

listRes[-erridx,] 

allres <- NULL

for (i in (1:length(listRes))[-erridx]){
  resi <- listRes[[i]]$result
  resi$lineupid <- unnest_ests$lineupid[i]
  resi$lineupname <- unnest_ests$lineupname[i]
  resi$M <- unnest_ests$M[i]
  resi$rep <- unnest_ests$rep[i]
  resi$idx <- i
  allres <- rbind(allres, resi)
  print(i)
}

head(allres)

if (allres$M[i] == allres$plot[i]){
  
}

allres %>% group_by(lineupid, plot) %>% arrange(lineupname, M, rep)

allres$effectname <- 1:nrow(allres) 

for (i in unique(allres$idx)){ 
  idx2 <- which(allres$idx == i)
  if (unique(allres$lineupname[idx2]) == 'smallfriends'){
    allres[idx2,]$effectname <- c(rep(c("alpha1", "beta1", "beta2"), (unique(allres$M[idx2])-1)), 
                                  c("alpha1", "beta1", "beta2",'beta3'))
  } else if (unique(allres$lineupname[idx2]) == 'smallfriends-rev'){
    allres[idx2,]$effectname <- c(rep(c("alpha1", "beta1", "beta2", 'beta3'), (unique(allres$M[idx2])-1)), 
                                  c("alpha1", "beta1", "beta2"))
  } else if (unique(allres$lineupname[idx2]) == 'smallfriends-eff2'){
    allres[idx2,]$effectname <- c(rep(c("alpha1", "beta1", "beta2"), (unique(allres$M[idx2])-1)), 
                                c("alpha1", "beta1", "beta2",'beta4'))
  } else{
    allres[idx2,]$effectname <- c(rep(c("alpha1", "beta1", "beta2", 'beta4'), (unique(allres$M[idx2])-1)), 
                                  c("alpha1", "beta1", "beta2"))
  } 
  print(i)
}     

allres$model <- 1:nrow(allres)

for (i in unique(allres$idx)){ 
  idx2 <- which(allres$idx == i)
  if (unique(allres$lineupname[idx2]) == 'smallfriends'){
    allres[idx2,]$model <- c(rep("M1", 3*(unique(allres$M[idx2])-1)), rep("M2", 4))
  } else if (unique(allres$lineupname[idx2]) == 'smallfriends-rev'){
    allres[idx2,]$model <- c(rep("M2", 4*(unique(allres$M[idx2])-1)),rep("M1", 3))
  } else if (unique(allres$lineupname[idx2]) == 'smallfriends-eff2'){
    allres[idx2,]$model <- c(rep("M1", 3*(unique(allres$M[idx2])-1)), 
                                  rep("M3", 4))
  } else{
    allres[idx2,]$model <- c(rep("M3", 4*(unique(allres$M[idx2])-1)), 
                                  rep("M1",3))
  } 
  print(i)
} 
head(allres)

ggplot(data = allres, aes(x = idx, y = effects, color = plot)) +
  geom_point() + facet_wrap(~effectname, scales = 'free')

ggplot(data = allres, aes(x = M, y = effects, color = plot, fill = plot, group = M)) +
  geom_boxplot() + ylim(c(-15,15)) +  facet_wrap(~effectname, scales = 'free')

#very interesting
ggplot(data = allres, aes(x = effects)) +
  geom_density(aes(fill = model), alpha = .4) + xlim(c(-10,10)) + 
  facet_wrap(~effectname, scales = 'free')  

ggplot()+ 
geom_density(data = effects_dist, 
             aes(x = estimate, fill = model), alpha = .4) + 
  facet_wrap(~effectname, scales = 'free')  


effects_dist <- read.csv("Data/simulation-1000-M1-M2-M3.csv")
names(effects_dist)[3] <- "effectname"
names(effects_dist)[2] <- 'model'
ggplot() + 
  geom_density(data = effects_dist, aes(x = estimate, fill = Model), alpha = .4)+
  facet_wrap(~effectname, scales = 'free')
  