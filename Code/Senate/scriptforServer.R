library(RSiena)
load("senateSienaNoHRC.rda")
# basic effects structure
SenBasic <- getEffects(senateSiena)
# basic algorithm structure
myalg <- sienaAlgorithmCreate( projname = Sys.time() , n3 = 1000)
# additional effects structures
#Senjtt_p <- includeEffects(SenBasic, "jumpXTransTrip", include = TRUE, type = "eval", interaction1 = "party", character = TRUE)
#Senjtt_s <- includeEffects(SenBasic, "jumpXTransTrip", include = TRUE, type = "eval", interaction1 = "sex", character = TRUE)
# SeninIsD <- includeEffects(SenBasic, "inIsDegree", include = TRUE, type = "eval", interaction1 = "", character = TRUE)
# Sensame_p <- includeEffects(SenBasic, "sameX", include = TRUE, type = "eval", interaction1 = "party", character = TRUE)
 Senstt_b <- includeEffects(SenBasic, "simXTransTrip", include = TRUE, type = "eval", interaction1 = "bills", character = TRUE)
# Sensimre_p <- includeEffects(SenBasic, "simRecipX", include = TRUE, type = "eval", interaction1 = "party", character = TRUE)
# Sentt_p <- includeEffects(SenBasic, "transTrip", include = TRUE, type = "eval", interaction1 = "", character = TRUE)
# Senstt_p <- includeEffects(SenBasic, "sameXTransTrip", include = TRUE, type = "eval", interaction1 = "party", character = TRUE)

get_effects_dist <- function(dat, struct, N){
  require(RSiena)
  myalg <- sienaAlgorithmCreate( projname = Sys.time() , n3 = 1000)
  nparm <- sum(struct$include)
  Parmnames <- struct$shortName[struct$include]
  Nnr <- sum(Parmnames != "Rate")
  df.ests <- data.frame(matrix(0, nrow = N, ncol = nparm + Nnr + 1))
  
  names(df.ests) <- c(Parmnames,
                      paste("se", Parmnames[which(Parmnames!="Rate")], sep = "_"),
                      "maxConv")
  for (i in 1:N){
    fits <- siena07(myalg, data = dat, effects = struct, returnDeps = FALSE,
                    batch=TRUE, verbose = FALSE, silent = TRUE, useCluster = TRUE, nbrNodes = 3, 
                    initC = TRUE, clusterIter = FALSE)
    df.ests[i,] <- c(fits$rate, fits$theta, sqrt(diag(fits$covtheta)), fits$tconv.max)
  }
  return(df.ests)
}

simttbFitsSenate <- get_effects_dist(dat = senateSiena, struct = Senstt_b, N = 100)
write.csv(simttbFitsSenate, "Data/senate/simttbModelFitsSenate100.csv")
simttbFitsSenate <- get_effects_dist(dat = senateSiena, struct = Senstt_b, N = 100)
write.csv(simttbFitsSenate, "Data/senate/simttbModelFitsSenate200.csv")
simttbFitsSenate <- get_effects_dist(dat = senateSiena, struct = Senstt_b, N = 100)
write.csv(simttbFitsSenate, "Data/senate/simttbModelFitsSenate300.csv")
simttbFitsSenate <- get_effects_dist(dat = senateSiena, struct = Senstt_b, N = 100)
write.csv(simttbFitsSenate, "Data/senate/simttbModelFitsSenate400.csv")
simttbFitsSenate <- get_effects_dist(dat = senateSiena, struct = Senstt_b, N = 100)
write.csv(simttbFitsSenate, "Data/senate/simttbModelFitsSenate500.csv")
simttbFitsSenate <- get_effects_dist(dat = senateSiena, struct = Senstt_b, N = 100)
write.csv(simttbFitsSenate, "Data/senate/simttbModelFitsSenate600.csv")
simttbFitsSenate <- get_effects_dist(dat = senateSiena, struct = Senstt_b, N = 100)
write.csv(simttbFitsSenate, "Data/senate/simttbModelFitsSenate700.csv")
simttbFitsSenate <- get_effects_dist(dat = senateSiena, struct = Senstt_b, N = 100)
write.csv(simttbFitsSenate, "Data/senate/simttbModelFitsSenate800.csv")
simttbFitsSenate <- get_effects_dist(dat = senateSiena, struct = Senstt_b, N = 100)
write.csv(simttbFitsSenate, "Data/senate/simttbModelFitsSenate900.csv")
simttbFitsSenate <- get_effects_dist(dat = senateSiena, struct = Senstt_b, N = 100)
write.csv(simttbFitsSenate, "Data/senate/simttbModelFitsSenate1000.csv")
# samepFitsSenate <- get_effects_dist(dat = senateSiena, struct = Sensame_p, N = 100)
# write.csv(samepFitsSenate, "Data/senate/samepModelFitsSenate100.csv")
# samepFitsSenate <- get_effects_dist(dat = senateSiena, struct = Sensame_p, N = 100)
# write.csv(samepFitsSenate, "Data/senate/samepModelFitsSenate200.csv")
# samepFitsSenate <- get_effects_dist(dat = senateSiena, struct = Sensame_p, N = 100)
# write.csv(samepFitsSenate, "Data/senate/samepModelFitsSenate300.csv")
# samepFitsSenate <- get_effects_dist(dat = senateSiena, struct = Sensame_p, N = 100)
# write.csv(samepFitsSenate, "Data/senate/samepModelFitsSenate400.csv")
# samepFitsSenate <- get_effects_dist(dat = senateSiena, struct = Sensame_p, N = 100)
# write.csv(samepFitsSenate, "Data/senate/samepModelFitsSenate500.csv")
# samepFitsSenate <- get_effects_dist(dat = senateSiena, struct = Sensame_p, N = 100)
# write.csv(samepFitsSenate, "Data/senate/samepModelFitsSenate600.csv")
# samepFitsSenate <- get_effects_dist(dat = senateSiena, struct = Sensame_p, N = 100)
# write.csv(samepFitsSenate, "Data/senate/samepModelFitsSenate700.csv")
# samepFitsSenate <- get_effects_dist(dat = senateSiena, struct = Sensame_p, N = 100)
# write.csv(samepFitsSenate, "Data/senate/samepModelFitsSenate800.csv")
# samepFitsSenate <- get_effects_dist(dat = senateSiena, struct = Sensame_p, N = 100)
# write.csv(samepFitsSenate, "Data/senate/samepModelFitsSenate900.csv")
# samepFitsSenate <- get_effects_dist(dat = senateSiena, struct = Sensame_p, N = 100)
# write.csv(samepFitsSenate, "Data/senate/samepModelFitsSenate1000.csv")




# # for loop to parallelize
# cl <- makeCluster(3)  
# registerDoParallel(cl)  
# N <- 3
# results <- foreach(i=1:N, .packages='RSiena', .errorhandling = 'pass') %dopar% {  
#   fits <- siena07(myalg, data = dat, effects = struct, returnDeps = TRUE,
#                   batch=TRUE, verbose = FALSE, silent = TRUE)
#   return(c(fits$rate, fits$theta, sqrt(diag(fits$covtheta)), fits$tconv.max))
# }
# stopCluster(cl)  
# 
# stopCluster(cl)  

