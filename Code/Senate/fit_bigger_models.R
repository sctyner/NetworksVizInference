setwd("~/Desktop/Dissertation/NetworksVizInference")

load("Data/senate/senateSienaNoHRC.rda")
library(RSiena)
SenBasic <- getEffects(senateSiena)
Sen_all <- includeEffects(SenBasic, "jumpXTransTrip", include = TRUE, type = "eval", interaction1 = "party", character = TRUE)
Sen_all <- includeEffects(Sen_all, "jumpXTransTrip", include = TRUE, type = "eval", interaction1 = "sex", character = TRUE)
Sen_all <- includeEffects(Sen_all, "simXTransTrip", include = TRUE, type = "eval", interaction1 = "bills", character = TRUE)
Sen_all <- includeEffects(Sen_all, "sameXTransTrip", include = TRUE, type = "eval", interaction1 = "party", character = TRUE)
Sen_all_nojttp <- includeEffects(SenBasic, "jumpXTransTrip", include = TRUE, type = "eval", interaction1 = "sex", character = TRUE)
Sen_all_nojttp <- includeEffects(Sen_all_nojttp, "simXTransTrip", include = TRUE, type = "eval", interaction1 = "bills", character = TRUE)
Sen_all_nojttp <- includeEffects(Sen_all_nojttp, "sameXTransTrip", include = TRUE, type = "eval", interaction1 = "party", character = TRUE)

myalg <- sienaAlgorithmCreate( projname = Sys.time() , n3 = 1000)

p = Sys.time()
big_model <- siena07(myalg, data = senateSiena, effects = Sen_all, returnDeps = TRUE,
                    batch=TRUE, verbose = TRUE, silent = FALSE)
Sys.time() - p       

p = Sys.time()
big_model2 <- siena07(myalg, data = senateSiena, effects = Sen_all, prevAns = big_model, returnDeps = TRUE,
                    batch=TRUE, verbose = TRUE, silent = FALSE)
Sys.time() - p  

p = Sys.time()
big_model3 <- siena07(myalg, data = senateSiena, effects = Sen_all, prevAns = big_model2, returnDeps = TRUE,
                    batch=TRUE, verbose = TRUE, silent = FALSE)
Sys.time() - p  

p = Sys.time()
big_model4 <- siena07(myalg, data = senateSiena, effects = Sen_all, prevAns = big_model3, returnDeps = TRUE,
                    batch=TRUE, verbose = TRUE, silent = FALSE)
Sys.time() - p  

p = Sys.time()
big_modela <- siena07(myalg, data = senateSiena, effects = Sen_all_nojttp, returnDeps = TRUE,batch=TRUE, verbose = TRUE, silent = FALSE)
Sys.time() - p 

p = Sys.time()
big_modela2 <- siena07(myalg, data = senateSiena, effects = Sen_all_nojttp, returnDeps = TRUE,batch=TRUE, verbose = TRUE, silent = FALSE)
Sys.time() - p 


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


bigModSenate <- get_effects_dist(senateSiena, struct = Sen_all_nojttp, N = 10)
write_csv(bigModSenate, path = "Data/senate/bigModFitsSenate.csv")
