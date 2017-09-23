# repeatedly fit models to data
library(tidyverse)
library(RSienaTest)
library(RSiena)
SenateEffects <- read_csv("Data/senate/sigEffsSenate912.csv")
sigSenEffects <- SenateEffects %>% 
  filter(overallConverge < 0.25 & Waldpval < 0.10 & !is.na(shortName)) %>% 
  arrange(Waldpval) %>% mutate(fullEffect = ifelse(!is.na(inter1), paste(shortName, inter1), shortName))

fullEffectNames <- c("jumpXTransTrip party", "jumpXTransTrip sex", "inIsDegree", "sameX party", 
                "simXTransTrip bills", "simRecipX party", "transTrip", "sameXTransTrip party")

sigSenEffects %>% filter(fullEffect %in% fullEffectNames) -> effectsIWant

load("Data/senate/senateSienaNoHRC.rda")

SenBasic <- getEffects(senateSiena)
Senjtt_p <- includeEffects(SenBasic, "jumpXTransTrip", include = TRUE, type = "eval", interaction1 = "party", character = TRUE)
Senjtt_s <- includeEffects(SenBasic, "jumpXTransTrip", include = TRUE, type = "eval", interaction1 = "sex", character = TRUE)
SeninIsD <- includeEffects(SenBasic, "inIsDegree", include = TRUE, type = "eval", interaction1 = "", character = TRUE)
Sensame_p <- includeEffects(SenBasic, "sameX", include = TRUE, type = "eval", interaction1 = "party", character = TRUE)
Senstt_b <- includeEffects(SenBasic, "simXTransTrip", include = TRUE, type = "eval", interaction1 = "bills", character = TRUE)
Sensimre_p <- includeEffects(SenBasic, "simRecipX", include = TRUE, type = "eval", interaction1 = "party", character = TRUE)
Sentt_p <- includeEffects(SenBasic, "transTrip", include = TRUE, type = "eval", interaction1 = "", character = TRUE)
Senstt_p <- includeEffects(SenBasic, "sameXTransTrip", include = TRUE, type = "eval", interaction1 = "party", character = TRUE)

require(RSiena)
struct <- Senjtt_p
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


M1Senate <- get_effects_dist(senateSiena, struct = SenBasic, N = 100)
write_csv(M1Senate, path = "Data/senate/basicModelFitsSenate2.csv")
M1Senate <- get_effects_dist(senateSiena, struct = SenBasic, N = 100)
write_csv(M1Senate, path = "Data/senate/basicModelFitsSenate3.csv")
M1Senate <- get_effects_dist(senateSiena, struct = SenBasic, N = 100)
write_csv(M1Senate, path = "Data/senate/basicModelFitsSenate9.csv")


jtt_pSenate <- get_effects_dist(senateSiena, struct = Senjtt_p, N = 1000)
write_csv(jtt_pSenate, path = "Data/senate/jttpModelFitsSenate.csv")
