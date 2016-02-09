# Using the friends example in RSiena 

# Use the three effects presented as the alternative model, then used 
# the similarity effects presented in the "assignment" section of the code
# as the null model

setwd("s50_data")
library(RSiena) # or RSienaTest
# read in network data
friend.data.w1 <- as.matrix(read.table("s50-network1.dat"))
friend.data.w2 <- as.matrix(read.table("s50-network2.dat"))
friend.data.w3 <- as.matrix(read.table("s50-network3.dat"))
# read in covariate data
drink <- as.matrix(read.table("s50-alcohol.dat"))
smoke <- as.matrix(read.table("s50-smoke.dat"))
# First create a 50 * 50 * 3 array composed of the 3 adjacency matrices
friendshipData <- array( c( friend.data.w1, friend.data.w2, friend.data.w3 ),
                         dim = c( 50, 50, 3 ) )
# and next give this the role of the dependent variable:
friendship <- sienaDependent(friendshipData)
alcohol <- varCovar( as.matrix(drink[,1:2]) )
# create siena data object
mydata <- sienaDataCreate( friendship, alcohol)

myeff <- getEffects( mydata )
myeff1 <- includeEffects( myeff, egoX, altX, egoXaltX, interaction1 = "alcohol" )
myeff2 <- includeEffects( myeff, simX, interaction1 = "alcohol")
# Create object with algorithm settings
myalgorithm <- sienaAlgorithmCreate( projname = 's50' )
#Estimate parameters
ans1 <- siena07( myalgorithm, data = mydata, effects = myeff1)
ans2 <- siena07( myalgorithm, data = mydata, effects = myeff2)
# view estimates
ans1$rate
ans1$theta

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
