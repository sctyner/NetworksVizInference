# fit every model to the simulated data

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

# fitBasicSaom <- function(sienaDat){
#   SenBasic <- getEffects(sienaDat)
#   myalg <- sienaAlgorithmCreate( projname = Sys.time() , n3 = 1000)
#   fits <- siena07(myalg, data = sienaDat, effects = SenBasic, returnDeps = TRUE,
#                   batch=TRUE, verbose = FALSE, silent = TRUE)
#   return(fits)
# }
# 
# fitjttpSaom <- function(sienaDat){
#   SenBasic <- getEffects(sienaDat)
#   myalg <- sienaAlgorithmCreate( projname = Sys.time() , n3 = 1000)
#   fits <- siena07(myalg, data = sienaDat, effects = Senjtt_p, returnDeps = TRUE,
#                   batch=TRUE, verbose = FALSE, silent = TRUE)
#   return(fits)
# }
# 
# fitjttsSaom <- function(sienaDat){
#   SenBasic <- getEffects(sienaDat)
#   myalg <- sienaAlgorithmCreate( projname = Sys.time() , n3 = 1000)
#   fits <- siena07(myalg, data = sienaDat, effects = Senjtt_s, returnDeps = TRUE,
#                   batch=TRUE, verbose = FALSE, silent = TRUE)
#   return(fits)
# }
# 
# fitsamepSaom <- function(sienaDat){
#   SenBasic <- getEffects(sienaDat)
#   myalg <- sienaAlgorithmCreate( projname = Sys.time() , n3 = 1000)
#   fits <- siena07(myalg, data = sienaDat, effects = Sensame_p, returnDeps = TRUE,
#                   batch=TRUE, verbose = FALSE, silent = TRUE)
#   return(fits)
# }
# 
# fitsimttbSaom <- function(sienaDat){
#   SenBasic <- getEffects(sienaDat)
#   myalg <- sienaAlgorithmCreate( projname = Sys.time() , n3 = 1000)
#   fits <- siena07(myalg, data = sienaDat, effects = Senstt_b, returnDeps = TRUE,
#                   batch=TRUE, verbose = FALSE, silent = TRUE)
#   return(fits)
# }
# 
# fitsamepttSaom <- function(sienaDat){
#   SenBasic <- getEffects(sienaDat)
#   myalg <- sienaAlgorithmCreate( projname = Sys.time() , n3 = 1000)
#   fits <- siena07(myalg, data = sienaDat, effects = Senstt_p, returnDeps = TRUE,
#                   batch=TRUE, verbose = FALSE, silent = TRUE)
#   return(fits)
# }

load("Data/senate/FauxTrueData/allModelsNested2SienaDat.RDS")

allSimsNested2 <- allSimsNested2 %>% mutate(basicFit = map(sienaDat, fitBasicSaom))
save(allSimsNested2, "Data/senate/FauxTrueData/allSimsNestedM1.RDS")
