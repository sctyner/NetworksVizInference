library(RSiena)
dat <- load("senateSienaNoHRC.rda")
# basic effects structure
SenBasic <- getEffects(senateSiena)
# basic algorithm structure
myalg <- sienaAlgorithmCreate( projname = Sys.time() , n3 = 1000)
# additional effects structures
Senjtt_p <- includeEffects(SenBasic, "jumpXTransTrip", include = TRUE, type = "eval", interaction1 = "party", character = TRUE)
# Senjtt_s <- includeEffects(SenBasic, "jumpXTransTrip", include = TRUE, type = "eval", interaction1 = "sex", character = TRUE)
# SeninIsD <- includeEffects(SenBasic, "inIsDegree", include = TRUE, type = "eval", interaction1 = "", character = TRUE)
# Sensame_p <- includeEffects(SenBasic, "sameX", include = TRUE, type = "eval", interaction1 = "party", character = TRUE)
# Senstt_b <- includeEffects(SenBasic, "simXTransTrip", include = TRUE, type = "eval", interaction1 = "bills", character = TRUE)
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
    fits <- siena07(myalg, data = dat, effects = struct, returnDeps = TRUE,
                    batch=TRUE, verbose = FALSE, silent = TRUE)
    df.ests[i,] <- c(fits$rate, fits$theta, sqrt(diag(fits$covtheta)), fits$tconv.max)
  }
  return(df.ests)
}

jtt_pFitsSenate <- get_effects_dist(dat = senateSiena, struct = Senjtt_p, N = 1000)
write.csv(jtt_pFitsSenate, "jttpModelFitsSenate.csv")


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

