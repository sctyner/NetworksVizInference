# Using the friends example in RSiena 

# Use the three effects presented as the alternative model, then used 
# the similarity effects presented in the "assignment" section of the code
# as the null model

setwd("s50_data")
library(RSiena) # or RSienaTest
# read in network data
friend.data.w1 <- as.matrix(read.table("Data/s50_data/s50-network1.dat"))
friend.data.w2 <- as.matrix(read.table("Data/s50_data/s50-network2.dat"))
friend.data.w3 <- as.matrix(read.table("Data/s50_data/s50-network3.dat"))
# read in covariate data
drink <- as.matrix(read.table("Data/s50_data/s50-alcohol.dat"))
smoke <- as.matrix(read.table("Data/s50_data/s50-smoke.dat"))
# First create a 50 * 50 * 3 array composed of the 3 adjacency matrices
friendshipData <- array( c( friend.data.w1, friend.data.w2, friend.data.w3 ),
                         dim = c( 50, 50, 3 ) )
# and next give this the role of the dependent variable:
friendship <- sienaDependent(friendshipData)
alcohol <- varCovar( as.matrix(drink) )
# create siena data object
mydata <- sienaDataCreate( friendship, alcohol)

null_model_eff <- getEffects(mydata)
test_model_eff <- includeEffects( null_model_eff, egoXaltX, interaction1 = "alcohol")

# Create object with algorithm settings
myalgorithm <- sienaAlgorithmCreate( projname = 's50' , n3 = 1500)
#Estimate parameters
ests_null <- siena07( myalgorithm, data = mydata, returnDeps = TRUE, effects = null_model_eff, batch=TRUE, verbose = FALSE)
ests_test <- siena07( myalgorithm, data = mydata, returnDeps = TRUE, effects = test_model_eff, batch=TRUE, verbose = FALSE)

# How do the simulations look compared to each other? 
# i.e. how much does the network evolve in simulation

# pull out the last 10 networks for periods 1 (wave 1->2) and 2 (wave 2->3)
period1 <- NULL
for (i in 1191:1200){
  getnet <- merge(data.frame(ests_null$sims[[i]][[1]][[1]][[1]])[,-3], 
                  data.frame(id = 1:50), by.x = "X1", by.y = "id",
                  all = T)
  getnet$count <- paste(i, "wave2") 
  period1 <- rbind(period1, getnet)
}

period2 <- NULL
for (i in 1191:1200){
  getnet <- merge(data.frame(ests_null$sims[[i]][[1]][[1]][[2]])[,-3], 
                  data.frame(id = 1:50), by.x = "X1", by.y = "id",
                  all = T)
  getnet$count <- paste(i, "wave3") 
  period2 <- rbind(period2, getnet)
}

library(geomnet)

# scroll down for actual2, actual3
period1 <- rbind(period1, actual2)
ggplot(data = period1, aes(from_id = X1, to_id = X2)) +
  geom_net(fiteach = TRUE) + theme_net() +
  facet_wrap(~count)

period2 <- rbind(period2, actual3)
ggplot(data = period2, aes(from_id = X1, to_id = X2)) +
  geom_net(fiteach = TRUE) + theme_net() +
  facet_wrap(~count)

ggplot(data = rbind(period1[1011:1388,],period2[1013:1380,]), aes(from_id = X1, to_id = X2)) +
  geom_net(fiteach = FALSE) + theme_net() +
  facet_wrap(~count, ncol = 2) 

library(network)
actual2 <- merge(data.frame(as.edgelist(as.network(friend.data.w2))), 
                 data.frame(id = 1:50), 
                 by.x = "X1", by.y = "id", all = T)
actual2$count <- "true_wave2"
actual3 <- merge(data.frame(as.edgelist(as.network(friend.data.w3))), data.frame(id = 1:50), 
                 by.x = "X1", by.y = "id", all = T)
actual3$count <- "true_wave3"

head(actual2)
# view estimates
ests_null$rate
ests_null$theta

ests_test$rate
ests_test$theta

ests_test$oneStep

# significance testing

A1.1 <- c(1, rep(0,4))
A1.2 <- c(0,1, rep(0,3))
A1.3 <- c(0,0, 1,0,0)
A1.4 <- c(rep(0,3), 1, 0)
A1.5 <- c(rep(0,4), 1)

library(RSienaTest)
Wald.RSiena(A1.1, ans1) # density significant
Wald.RSiena(A1.2, ans1) # reciprocity significant
Wald.RSiena(A1.3, ans1) # alcohol alter not significant
Wald.RSiena(A1.4, ans1) # alcohol ego not significan
Wald.RSiena(A1.5, ans1) # alcohol ego x alchohol alter IS significant
# test 

Wald.RSiena(rbind(A1.1,A1.2), ans1)
Wald.RSiena(rbind(A1.1,A1.3), ans1)
Wald.RSiena(rbind(A1.3,A1.4), ans1)
Wald.RSiena(rbind(A1.3,A1.5), ans1)

get_ests_RSiena <- function(RSobj){
  rates <- RSobj$rate
  evals <- RSobj$theta
  effnames <- c(rep("Rate", length(rates)), RSobj$effects$effectName)
  periods <- RSobj$periodNos

}

rate.params <- data.frame(rate.param = c("Rate", "Rate"),
           rate.param.val = ans1$rate)
eval.params <- data.frame(eval.param = c("density", "recip"), eval.param.val = ans1$theta[1:2])
eval.int.params <- data.frame(eval.int.param = c("altX", "egoX", "egoXaltX"), 
                              eval.int.param.val = ans1$theta[3:5])

sim1 <- SimulateNextWave(init.waves = friendshipData, V0 = drink, 
                 rate.params.names = rate.params$rate.param, 
                 rate.params.vals = rate.params$rate.param.val,
                 eval.params.names = eval.params$eval.param,
                 eval.params.val = eval.params$eval.param.val,
                 eval.int.params.names = eval.int.params$eval.int.param,
                 eval.int.params.vals = eval.int.params$eval.int.param.val)

sim1[[1]][[1]][[1]][[1]]

plot_net_RSiena <- function(sims){ 
  N <- length(sims)
  i <- 1
  for (i in 1:N){
    edges <- data.frame(sims[[i]][[1]][[1]]) 
    vertices <- data.frame(id = unique(c(edges[,1], edges[,2])))
    net <- merge(edges, vertices, by.x = "X1", by.y="id", all = T)
    print(ggplot(net, aes(from_id = X1, to_id = X2)) +
      geom_net(directed = T, label = T, size = .5, ealpha = .5, labelcolour = 'blue') +
      theme_net())
  }  
}
sims <- sim1[[1000]]
plot_net_RSiena(sims)

sim1_net <- merge(sim1, data.frame(id =1:50), by.x = "X1", by.y = "id", all = T)

ggplot(sim1_net, aes(from_id = X1, to_id = X2)) + 
  geom_net(label = T, size = .1)

sim2 <- SimulateNextWave(init.waves = friendshipData, V0 = drink, 
                         rate.params.names = rate.params$rate.param, 
                         rate.params.vals = rate.params$rate.param.val,
                         eval.params.names = eval.params$eval.param,
                         eval.params.val = eval.params$eval.param.val,
                         eval.int.params.names = eval.int.params$eval.int.param,
                         eval.int.params.vals = eval.int.params$eval.int.param.val)
sim2_net <- merge(sim2, data.frame(id =1:50), by.x = "X1", by.y = "id", all = T)
ggplot(sim2_net, aes(from_id = X1, to_id = X2)) + 
  geom_net(label = T, size = .1)

#### Using the different model (ans2) #######
rate.params2 <- data.frame(rate.param = c("Rate", "Rate"),
                          rate.param.val = ans2$rate)
eval.params2 <- data.frame(eval.param = c("density", "recip"), eval.param.val = ans2$theta[1:2])
eval.int.params2 <- data.frame(eval.int.param = "simX", 
                              eval.int.param.val = ans2$theta[3])

sim1.2 <- SimulateNextWave(init.waves = friendshipData, V0 = drink, 
                         rate.params.names = rate.params2$rate.param, 
                         rate.params.vals = rate.params2$rate.param.val,
                         eval.params.names = eval.params2$eval.param,
                         eval.params.val = eval.params2$eval.param.val,
                         eval.int.params.names = eval.int.params2$eval.int.param,
                         eval.int.params.vals = eval.int.params2$eval.int.param.val)
sim1.2_net <- merge(sim1.2, data.frame(id =1:50), by.x = "X1", by.y = "id", all = T)

ggplot(sim1.2_net, aes(from_id = X1, to_id = X2)) + 
  geom_net(label = T, size = .1)

sim2.2 <- SimulateNextWave(init.waves = friendshipData, V0 = drink, 
                           rate.params.names = rate.params2$rate.param, 
                           rate.params.vals = rate.params2$rate.param.val,
                           eval.params.names = eval.params2$eval.param,
                           eval.params.val = eval.params2$eval.param.val,
                           eval.int.params.names = eval.int.params2$eval.int.param,
                           eval.int.params.vals = eval.int.params2$eval.int.param.val)
sim2.2_net <- merge(sim2.2, data.frame(id =1:50), by.x = "X1", by.y = "id", all = T)

ggplot(sim2.2_net, aes(from_id = X1, to_id = X2)) + 
  geom_net(label = T, size = .1)

# create lineup with m=3 plots

# null is smaller model
small_lu <- rbind(sim1_net, sim1.2_net, sim2.2_net)
small_lu$group <- rep(sample.int(3), c(nrow(sim1_net), nrow(sim1.2_net), nrow(sim2.2_net)))

ggplot(small_lu, aes(from_id = X1, to_id = X2)) + 
  geom_net(ealpha = .7, fiteach = T) + 
  theme_net() + 
  facet_wrap(~group, nrow=1)
