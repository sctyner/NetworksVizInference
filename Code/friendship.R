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
  