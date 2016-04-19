# estimate model effects

library(RSiena)
library(geomnet)
library(grid)
library(gridSVG)
##### LOAD IN DATA #####
#looks like 20:35 are pretty well connected, go with those for now
#setwd("Desktop/NetworksResearch/NetworksVizInference/")
friend.data.w1 <- as.matrix(read.table("../Data/s50_data/s50-network1.dat"))
friend.data.w2 <- as.matrix(read.table("../Data/s50_data/s50-network2.dat"))
friend.data.w3 <- as.matrix(read.table("../Data/s50_data/s50-network3.dat"))
fd2.w1 <- friend.data.w1[20:35,20:35]
fd2.w2 <- friend.data.w2[20:35,20:35]
fd2.w3 <- friend.data.w3[20:35,20:35]
# read in covariate data
drink <- as.matrix(read.table("../Data/s50_data/s50-alcohol.dat"))
drink2 <- drink[20:35,]
# siena data
friendData2 <- array( c( fd2.w1, fd2.w2, fd2.w3 ),
                      dim = c( 16, 16, 3 ) )
# and next give this the role of the dependent variable:
friend2 <- sienaDependent(friendData2)
alcohol2 <- varCovar( as.matrix(drink2) )
# create siena data object
mysmalldata <- sienaDataCreate( friend2, alcohol2)
null_model_eff2 <- getEffects(mysmalldata)
myalgorithm2 <- sienaAlgorithmCreate( projname = 's50' , n3 = 1000)
sig_eff_names <- read.csv("../Data/sig_eff_names_smfriends.csv")
runs_models_smallFriends <- readRDS("../Data/runs_models_smallFriends.RDS")
eff_models_smallFriends <- readRDS("../Data/eff_models_smallFriends.RDS")


# write a function to get dist of the parameter ests
get_eff_val_dist <- function(B, eff_struct, sd = 2017){
  require(RSiena)
  set.seed(sd)
  df.ests <- NULL
  for (i in 1:B){
    null_net <- siena07(myalgorithm2, data = mysmalldata, returnDeps = TRUE, effects = eff_struct, batch=TRUE, verbose = FALSE, silent = TRUE)
    ests <- c(null_net$rate,null_net$theta)
    df.ests <- rbind(df.ests, ests)
  }
  return(df.ests)
}

#null_model_dist <- get_eff_val_dist(B = 1000, eff_struct = null_model_eff2)
#write.csv(null_model_dist, "distribution_null_model.csv")
# use these!  4.660529  1.928300 -3.594301  4.145621
#alt_model_ests <- get_eff_val_dist(B = 1000, eff_struct = eff_models_smallFriends[[39]])
#write.csv(alt_model_ests, "distribution_jumpTT_model.csv")
# use these!  5.172778  2.015728 -4.101604  4.274210  3.207874
#alt_model_ests2 <- avg_eff_val(B = 10, eff_struct = eff_models_smallFriends[[39]])

# another significan effect: number pairs at double achieved dist 2. eff_models_smFriends[[18]]

alt_model2_ests <- get_eff_val_dist(B = 1000, eff_struct = eff_models_smallFriends[[18]])
#write.csv(alt_model2_ests, "Data/distribution_dblpairs_model.csv")

null_model_dist <- read.csv("../Data/distribution_null_model.csv")
alt_model_dist <- read.csv("../Data/distribution_jumpTT_model.csv")
alt_model2_dist <- read.csv("../Data/distribution_dblpairs_model.csv")

#null_model_dist <- as.data.frame(null_model_dist)
rownames(null_model_dist) <- 1:nrow(null_model_dist)
names(null_model_dist) <- c('alpha1', 'alpha2', 'beta1', 'beta2')

library(tidyr)
null_model_dist %>% gather(coefficient, value, alpha1:beta2) -> null_model_dist2

ggplot(data = null_model_dist2, aes(x = value)) + 
  geom_histogram(binwidth = .01) +
  facet_wrap(~coefficient, scales = "free_x")

#alt_model_dist <- as.data.frame(alt_model_ests)
rownames(alt_model_dist) <- 1:nrow(alt_model_dist)
names(alt_model_dist) <- c('alpha1', 'alpha2', 'beta1', 'beta2','beta3')

alt_model_dist %>% gather(coefficient, value, alpha1:beta3) -> alt_model_dist2

ggplot(data = alt_model_dist2, aes(x = value)) + 
  geom_histogram(binwidth = .01) +
  facet_wrap(~coefficient, scales = "free_x")

alt_model2_dist <- as.data.frame(alt_model2_ests)
rownames(alt_model2_dist) <- 1:nrow(alt_model2_dist)
names(alt_model2_dist) <- c('alpha1', 'alpha2', 'beta1', 'beta2','beta3')

alt_model2_dist %>% gather(coefficient, value, alpha1:beta3) -> alt_model2_dist2

ggplot(data = alt_model2_dist2, aes(x = value)) + 
  geom_histogram(binwidth = .01) +
  facet_wrap(~coefficient, scales = "free_x") # the spread is pretty big on beta3

library(GGally)
ggscatmat(null_model_dist) # :(
ggscatmat(alt_model_dist) # :(
ggscatmat(alt_model2_dist)
