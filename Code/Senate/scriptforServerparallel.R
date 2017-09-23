library(parallel)
library(RSiena)
load("Data/senate/senateSienaNoHRC.rda")
dat <- senateSiena
SenBasic <- getEffects(senateSiena)
struct <- SenBasic

get_effects_dist <- function(dat, struct, N){
  myalg <- sienaAlgorithmCreate( projname = Sys.time() , n3 = 1000)
  no_cores <- detectCores()
  cl <- makeCluster(no_cores, outfile = "", renice = 0)
  clusterExport(cl, varlist = c("myalg", "dat", "struct"))

  mycoefs <- parLapply(cl, 1:N, function(i, dat, struct, myalg) {
      library(RSiena)
    
    fits <- siena07(myalg, data = dat, effects = struct, returnDeps = TRUE,
              batch=TRUE, verbose = FALSE, silent = TRUE)

    return(c(fits$rate, fits$theta, sqrt(diag(fits$covtheta)), fits$tconv.max))
  })

  df.ests <- as.data.frame(do.call(rbind, mycoefs))

  nparm <- sum(struct$include)
  Parmnames <- struct$shortName[struct$include] 
  
  names(df.ests) <- c(Parmnames,
                      paste("se", Parmnames[which(Parmnames!="Rate")], sep = "_"),
                      "maxConv")
  #for (i in 1:N){
  #  fits <- siena07(myalg, data = dat, effects = struct, returnDeps = TRUE,
  #                  batch=TRUE, verbose = FALSE, silent = TRUE)
  #  df.ests[i,] <- c(fits$rate, fits$theta, sqrt(diag(fits$covtheta)), fits$tconv.max)
  #}
  return(df.ests)
}

M1Senate <- get_effects_dist(dat = senateSiena, struct = SenBasic, N = 3)
write.csv(M1Senate, "basicModelFitsSenate5.csv")
