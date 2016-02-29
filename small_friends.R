# New model with a smaller collection of girls from the friendship data
#deciding how to subset the girls:
library(ggplot2)
library(tidyr)
library(dplyr)
val1 <- data.frame(friend.data.w1) %>% gather(x,y, V1:V50) %>% select(y)
val2 <- data.frame(friend.data.w2) %>% gather(x,y, V1:V50) %>% select(y)
val3 <- data.frame(friend.data.w3) %>% gather(x,y, V1:V50) %>% select(y)

all_girls <- expand.grid(x = 1:50, y= 1:50)

view_rs <- data.frame(all_girls, W1 = val1, W2 = val2, W3 = val3)
names(view_rs)[3:5] <- c('w1', 'w2','w3')

view_rs %>% gather(wave, value, w1:w3) -> view_rs2

ggplot(data= view_rs2, aes(x = x, y=y)) + 
  geom_tile(aes(fill = as.factor(value))) + facet_wrap(~wave) +
  theme(aspect.ratio=1)

#looks like 20:35 are pretty well connected, go with those for now
fd2.w1 <- friend.data.w1[20:35,20:35]
fd2.w2 <- friend.data.w2[20:35,20:35]
fd2.w3 <- friend.data.w3[20:35,20:35]
drink2 <- drink[20:35,]

# siena data
library(RSiena)
friendData2 <- array( c( fd2.w1, fd2.w2, fd2.w3 ),
                         dim = c( 16, 16, 3 ) )
# and next give this the role of the dependent variable:
friend2 <- sienaDependent(friendData2)
alcohol2 <- varCovar( as.matrix(drink2) )
# create siena data object
mysmalldata <- sienaDataCreate( friend2, alcohol2)
null_model_eff2 <- getEffects(mysmalldata)
test_model_eff2 <- includeEffects( null_model_eff2, egoXaltX, interaction1 = "alcohol2")
myalgorithm2 <- sienaAlgorithmCreate( projname = 's50' , n3 = 1000)
ests_null <- siena07( myalgorithm2, data = mysmalldata, returnDeps = TRUE, effects = null_model_eff2, batch=TRUE, verbose = FALSE)
ests_test <- siena07( myalgorithm2, data = mysmalldata, returnDeps = TRUE, effects = test_model_eff2, batch=TRUE, verbose = FALSE)

# pull out the last 10 networks for periods 1 (wave 1->2) and 2 (wave 2->3)
period1.null <- NULL
for (i in 991:1000){
  getnet <- merge(data.frame(ests_null$sims[[i]][[1]][[1]][[1]])[,-3], 
                  data.frame(id = 1:16), by.x = "X1", by.y = "id",
                  all = T)
  getnet$count <- paste(i, "wave2") 
  period1.null <- rbind(period1.null, getnet)
}

period2.null <- NULL
for (i in  991:1000){
  getnet <- merge(data.frame(ests_null$sims[[i]][[1]][[1]][[2]])[,-3], 
                  data.frame(id = 1:16), by.x = "X1", by.y = "id",
                  all = T)
  getnet$count <- paste(i, "wave3") 
  period2.null <- rbind(period2.null, getnet)
}

library(geomnet)
library(network)
actual1 <- merge(data.frame(as.edgelist(as.network(fd2.w1))), 
                 data.frame(id = 1:16), 
                 by.x = "X1", by.y = "id", all = T)
actual2 <- merge(data.frame(as.edgelist(as.network(fd2.w2))), 
                 data.frame(id = 1:16), 
                 by.x = "X1", by.y = "id", all = T)
actual2$count <- "true_wave2"
actual3 <- merge(data.frame(as.edgelist(as.network(fd2.w3))), 
                 data.frame(id = 1:16), 
                 by.x = "X1", by.y = "id", all = T)
actual3$count <- "true_wave3"
period1.null <- rbind(period1.null, actual2)
period1.null$cat <- 'null'
ggplot(data = period1.null, aes(from_id = X1, to_id = X2)) +
  geom_net(fiteach = FALSE) + theme_net() +
  facet_wrap(~count)

period2.null <- rbind(period2.null, actual3)
period2.null$cat <- "null"
ggplot(data = period2.null, aes(from_id = X1, to_id = X2)) +
  geom_net(fiteach = FALSE) + theme_net() +
  facet_wrap(~count)


# looking at the test networks
period1.test <- NULL
for (i in 991:1000){
  getnet <- merge(data.frame(ests_test$sims[[i]][[1]][[1]][[1]])[,-3], 
                  data.frame(id = 1:16), by.x = "X1", by.y = "id",
                  all = T)
  getnet$count <- paste(i, "wave2") 
  period1.test <- rbind(period1.test, getnet)
}

period2.test <- NULL
for (i in  991:1000){
  getnet <- merge(data.frame(ests_test$sims[[i]][[1]][[1]][[2]])[,-3], 
                  data.frame(id = 1:16), by.x = "X1", by.y = "id",
                  all = T)
  getnet$count <- paste(i, "wave3") 
  period2.test <- rbind(period2.test, getnet)
}

period1.test <- rbind(period1.test, actual2)
period1.test$cat <- 'test'
ggplot(data = period1.test, aes(from_id = X1, to_id = X2)) +
  geom_net(fiteach = FALSE) + theme_net() +
  facet_wrap(~count)

period2.test <- rbind(period2.test, actual3)
period2.test$cat <- "test"
ggplot(data = period2.test, aes(from_id = X1, to_id = X2)) +
  geom_net(fiteach = FALSE) + theme_net() +
  facet_wrap(~count)

grep(996, x = period2.test$count)
grep(996, x = period2.null$count)

ggplot(data = data.frame(rbind(period2.test[102:nrow(period2.test),], period2.null[113:nrow(period2.null),])), aes(from_id = X1, to_id = X2)) +
  geom_net(fiteach = FALSE) + theme_net() +
  facet_grid(cat~count)
