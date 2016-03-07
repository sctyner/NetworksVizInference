###############################################################################
###                                                                         ###
### ---- NetworkSimulation.R: a script for simulation with RSiena --------- ###
###                                                                         ###
###                         version November 19, 2014                       ###
###############################################################################
#
# This is a script with an example of how to simulate multiple network waves
# according to a stochastic actor-oriented model.
# Written by Tom Snijders.
# First a function is presented for network dynamics,
# then for network and behavior dynamics.
#
# It implements one particular model specification
# (with parameter values and numbers of actors and waves up to the user)
# and can easily be adapted to other model specifications:
# other effects, behavioral dependent variables, etc.
#
# The main issue that has to be solved here is that, in the case of
# 3 or more waves, the straightforward simulation for wave m+1 will start
# with the observed network at wave m in the data set,
# not with the simulated network for wave m.
# For simulations of 3 or more waves, it is desired to let the simulations
# for wave m+1 start at the simulated network for wave m.
# This is achieved by the following script,
# which implements a sequence of two-wave (one-period) simulations.
# The script uses a basic specification.
# It can be modified straightforwardly for other specifications.
# For example, the whole definition of InitEff and InitEff0
# could be done outside of the function,
# and these model specification objects could then be used
# as parameters for the function.
#
# Important note:
# ===============
# The function SimulateNetworks will create a file Init.out.
# This may become large after many simulations,
# and should be deleted afterwards.
# It is not deleted automatically within the function
# because it might serve diagnostic purposes.
#
# For setting parameter values, it is recommended to use values
# rec .. Vbsim that are in line with other experience,
# and to tune the value of dens such that the average degrees
# (which are reported when calling SimulateNetworks)
# are in a reasonable desired range, e.g., between 3 and 6.
# Larger values for rate will lead to larger running times of estimations.
# rate values are recommended to be between 3 and 5.
#
# A more detailed and precise way of tuning some of the parameters is by using
# the Robbins-Monro procedure that is further used for parameter estimation,
# as in Tom A.B. Snijders and Christian E.G. Steglich (2013),
# Representing Micro-Macro Linkages by Actor-Based Dynamic Network Models.
# Sociological Methods & Research, in press.
# DOI: http://dx.doi.org/10.1177/0049124113494573
#

library(RSiena) # or RSienaTest

###############################################################################
###                                                                         ###
###         First main function: SimulateNetworks                           ###
###                                                                         ###
###############################################################################


SimulateNetworks <- function(n, M, rate, dens, rec, tt, c3,
                             Vaego, Vaalt, Vasim, Vbego, Vbalt, Vbsim){
  # Simulates M consecutive network waves, with n actors,
  # according to a stochastic actor-oriented model
  # with parameter values rate for rate,
  # dens for outdegree, rec for reciprocity,
  # tt for transitive triplets, c3 for 3-cycles,
  # an actor covariate Va with values alternating between 0 and 1,
  # with parameter values Vaego, Vaalt, Vasim
  # for egoX, altX, and simX with respect to Va,
  # and an actor covariate Vb with a standard normal distribution,
  # with parameter values Vbego, Vbalt, Vbsim
  # for egoX, altX, and simX with respect to Vb.
  ##
  # Create actor covariates
  V0 <- rep(0, n)
  V0[2*(1:(n %/% 2))] <- 1 # equal binary
  V1 <- rnorm(n, 0, 1)
  # Create initial 2-wave data to get a suitable data structure.
  # arbitrarily, this initial network has an expected average degree of 3
  X0 <- matrix(rbinom(n*n,1,3/(n-1)),n,n)
  diag(X0) <- 0
  X1 <- X0
  # but X0 and X1 should not be identical for use in sienaDependent
  X0[1,2] <- 0
  X0[2,1] <- 1
  X1[1,2] <- 1
  X1[2,1] <- 0
  XX <- array(NA,c(n,n,2))
  XX[,,1] <- X0
  XX[,,2] <- X1
  # With this data structure, we now can create the data.
  Va <- coCovar(V0)
  Vb <- coCovar(V1)
  X   <- sienaDependent(XX, allowOnly = FALSE)
  InitData <- sienaDataCreate(X, Va, Vb)
  InitEff0 <- getEffects(InitData)
  # sink to avoid printing to the screen
  sink("eff.txt")
  # Specify the parameters.
  # The rate parameter is first multiplied by 10,
  # which will be used only to get from the totally random network XX[,,1] = X0
  # to the network that will be the simulated first wave.
  InitEff0 <- setEffect(InitEff0, Rate, type="rate", initialValue = 10*rate)
  InitEff0 <- setEffect(InitEff0, density, initialValue = dens)
  InitEff0 <- setEffect(InitEff0, recip, initialValue = rec)
  InitEff0 <- setEffect(InitEff0, transTrip, initialValue = tt)
  InitEff0 <- setEffect(InitEff0, cycle3, initialValue = c3)
  InitEff0 <- setEffect(InitEff0, egoX, interaction1="Va", initialValue = Vaego)
  InitEff0 <- setEffect(InitEff0, altX, interaction1="Va", initialValue = Vaalt)
  InitEff0 <- setEffect(InitEff0, simX, interaction1="Va", initialValue = Vasim)
  InitEff0 <- setEffect(InitEff0, egoX, interaction1="Vb", initialValue = Vbego)
  InitEff0 <- setEffect(InitEff0, altX, interaction1="Vb", initialValue = Vbalt)
  InitEff0 <- setEffect(InitEff0, simX, interaction1="Vb", initialValue = Vbsim)
  # The parameter given for n3 should be larger than sum(InitEff0$include)
  nthree <- sum(InitEff0$include)	+ 5
  InitAlg <- sienaAlgorithmCreate(projname="Init", useStdInits=FALSE,
                                  cond=FALSE, nsub=0, n3=nthree, simOnly=TRUE)
  # Simulate the first wave.
  InitSim   <- siena07(InitAlg, data=InitData, eff=InitEff0,
                       returnDeps=TRUE, batch=TRUE, silent=TRUE)
  # Now prepare for simulating waves 2 to M.
  # Create empty result network.
  Xs <- array(0, dim=c(n,n,M))
  # The rate parameter value from the function call is reinstated in InitEff.
  InitEff <- InitEff0
  InitEff <- setEffect(InitEff, Rate, type="rate", initialValue = rate)
  sink()
  for (m in 1:M){
    # Note that we start this loop with a previously simulated network.
    # Transform the previously simulated network
    # from edge list into adjacency matrix
    XXsim <- matrix(0,n,n)
    nsim  <- InitAlg$n3
    XXsim[InitSim$sims[[nsim]][[1]]$X[[1]][,1:2]]  <-
      InitSim$sims[[nsim]][[1]]$X[[1]][,3]
    # Put simulated network into the result matrix.
    Xs[,,m] <- XXsim
    # Put simulated network in desired places for the next simulation
    XX[,,2] <- XX[,,1] # used only to get the data structure
    XX[,,1] <- XXsim
    if (m < M){
      # The following is only to prevent the error that would occur
      # in the very unlikely event XX[,,1] == XX[,,2].
      if (identical(XX[,,1], XX[,,2])){XX[1,2,1] <- 1 - XX[1,2,2]}
      # Specify the two-wave network data set starting with XX[,,1].
      X <- sienaDependent(XX, allowOnly = FALSE)
      # Simulate wave m+1 starting at XX[,,1] which is the previous XXsim
      InitData  <- sienaDataCreate(X, Va, Vb)
      InitSim <- siena07(InitAlg, data=InitData, eff=InitEff,
                         returnDeps=TRUE, batch=TRUE, silent=TRUE)
    }
  }
  # Present the average degrees to facilitate tuning the outdegree parameter
  # to achieve a desired average value for the average degrees.
  cat("Average degrees ", round(colSums(Xs,dims=2)/n, digits=2), "\n")
  # Result: simulated data set; covara and covarb are vectors of length n;
  # networks is an array of dimension nxnxM
  list(covara = V0, covarb = V1, networks = Xs)
}


###############################################################################
###                                                                         ###
###         Examples                                                        ###
###                                                                         ###
###############################################################################


# Trial values:
n <- 20
M <- 4
rate <- 2
dens <- -1.9
rec <- 2
tt <- 0.3
c3 <- -0.3
Vaego <- 0
Vaalt <- 0
Vasim <- 0.6
Vbego <- 0.5
Vbalt <- -0.5
Vbsim <- 0.5

# Example call:
SN <- SimulateNetworks(n, M, rate, dens, rec, tt, c3, Vaego, Vaalt, Vasim,
                       Vbego, Vbalt, Vbsim)
# You can repeat this call a few times, and then see the varying values
# reported for the average degrees.
# You can also experiment this with other values for dens,
# keeping everything else the same,
# varying dens by values of +/- 0.05 to +/- 0.2, for example.

# For larger n, slightly lower values of dens are required
# to achieve roughly the same average degrees. For example:
n <- 30
dens <- -2.0
SN <- SimulateNetworks(n, M, rate, dens, rec, tt, c3, Vaego, Vaalt, Vasim,
                       Vbego, Vbalt, Vbsim)

# Results:
SN[[1]]
# the same as
SN$covara

SN[[2]]
# the same as
SN$covarb

SN[[3]]
# the same as
SN$networks

###############################################################################
###                                                                         ###
###         Second main function: SimulateNetworksBehavior                  ###
###                                                                         ###
###############################################################################


SimulateNetworksBehavior <- function(n, M, c, rate, dens, rec, tt, c3,
                                     Vaego, Vaalt, Vasim, Vbego, Vbalt, Vbsim,
                                     rate.b, lin.b, qu.b, avalt.b){
  # Simulates M consecutive network and behavior waves, with n actors,
  # with c categories of the behavior variable,
  # according to a stochastic actor-oriented model
  # with parameter values rate for rate,
  # dens for outdegree, rec for reciprocity,
  # tt for transitive triplets, c3 for 3-cycles,
  # an actor covariate Va with values alternating between 0 and 1,
  # with parameter values Vaego, Vaalt, Vasim
  # for egoX, altX, and simX with respect to Va,
  # and an actor covariate Vb with a standard normal distribution,
  # with parameter values Vbego, Vbalt, Vbsim
  # for egoX, altX, and simX with respect to Vb;
  # and with for the behavioral dependent variable parameter values
  # rate.b for rate, lin.b for linear tendency,
  # qu.b for quadratic tendency, and avalt.b for average alter.
  ##
  # Create actor covariates
  V0 <- rep(0, n)
  V0[2*(1:(n %/% 2))] <- 1 # equal binary
  V1 <- rnorm(n, 0, 1)
  # Create initial 2-wave data to get a suitable data structure.
  # arbitrarily, this initial network has an expected average degree of 3
  X0 <- matrix(rbinom(n*n,1,3/(n-1)),n,n)
  diag(X0) <- 0
  X1 <- X0
  # but X0 and X1 should not be identical for use in sienaDependent
  X0[1,2] <- 0
  X0[2,1] <- 1
  X1[1,2] <- 1
  X1[2,1] <- 0
  XX <- array(NA,c(n,n,2))
  XX[,,1] <- X0
  XX[,,2] <- X1
  # Create behavior variable; initial distribution uniform on {1, ..., c}.
  ZZ <- pmin(matrix(trunc(c*runif(n*2))+1, n, 2), c)
  # With this data structure, we now can create the data.
  Va <- coCovar(V0)
  Vb <- coCovar(V1)
  X   <- sienaDependent(XX, allowOnly = FALSE)
  Z   <- sienaDependent(ZZ, type="behavior", allowOnly = FALSE)
  InitData <- sienaDataCreate(X, Z, Va, Vb)
  InitEff0 <- getEffects(InitData)
  # sink to avoid printing to the screen
  sink("eff.txt")
  # Specify the parameters.
  # The rate parameters are first multiplied by 10,
  # which will be used only to get from the totally random network XX[,,1] = X0
  # to the network that will be the simulated first wave.
  InitEff0 <- setEffect(InitEff0, Rate, type="rate", initialValue = 10*rate)
  InitEff0 <- setEffect(InitEff0, density, initialValue = dens)
  InitEff0 <- setEffect(InitEff0, recip, initialValue = rec)
  InitEff0 <- setEffect(InitEff0, transTrip, initialValue = tt)
  InitEff0 <- setEffect(InitEff0, cycle3, initialValue = c3)
  InitEff0 <- setEffect(InitEff0, egoX, interaction1="Va", initialValue = Vaego)
  InitEff0 <- setEffect(InitEff0, altX, interaction1="Va", initialValue = Vaalt)
  InitEff0 <- setEffect(InitEff0, simX, interaction1="Va", initialValue = Vasim)
  InitEff0 <- setEffect(InitEff0, egoX, interaction1="Vb", initialValue = Vbego)
  InitEff0 <- setEffect(InitEff0, altX, interaction1="Vb", initialValue = Vbalt)
  InitEff0 <- setEffect(InitEff0, simX, interaction1="Vb", initialValue = Vbsim)
  InitEff0 <- setEffect(InitEff0, name = "Z", Rate, type="rate",
                        initialValue = 10*rate.b)
  InitEff0 <- setEffect(InitEff0, name = "Z", linear, initialValue = lin.b)
  InitEff0 <- setEffect(InitEff0, name = "Z", quad, initialValue = qu.b)
  InitEff0 <- setEffect(InitEff0, name = "Z", avAlt, interaction1 = "X",
                        initialValue = avalt.b)
  ## The parameter given for n3 should be larger than sum(InitEff0$include)
  nthree <- sum(InitEff0$include)	+ 5
  InitAlg <- sienaAlgorithmCreate(projname="Init", useStdInits=FALSE,
                                  cond=FALSE, nsub=0, n3=nthree, simOnly=TRUE)
  # Simulate the first wave.
  InitSim   <- siena07(InitAlg, data=InitData, eff=InitEff0,
                       returnDeps=TRUE, batch=TRUE, silent=TRUE)
  # Now prepare for simulating waves 2 to M.
  # Create empty result network and behavior matrices
  Xs <- array(NA, dim=c(n,n,M))
  Zs <- array(NA, dim=c(n,M))
  # The rate parameter values from the function call are reinstated in InitEff.
  InitEff <- InitEff0
  InitEff <- setEffect(InitEff, Rate, type="rate", initialValue = rate)
  InitEff <- setEffect(InitEff, name = "Z", Rate, type="rate",
                       initialValue = rate.b)
  sink()
  for (m in 1:M){
    # Note that we start this loop with a previously simulated network.
    # Transform the previously simulated network
    # from edge list into adjacency matrix
    XXsim <- matrix(0,n,n)
    nsim  <- InitAlg$n3
    XXsim[InitSim$sims[[nsim]][[1]]$X[[1]][,1:2]]  <-
      InitSim$sims[[nsim]][[1]]$X[[1]][,3]
    Zsim <- InitSim$sims[[nsim]][[1]]$Z[[1]]
    # Put simulated network and behavior into the result matrix.
    Xs[,,m] <- XXsim
    Zs[,m] <- Zsim
    # Put simulated network in desired places for the next simulation
    XX[,,2] <- XX[,,1] # used only to get the data structure
    XX[,,1] <- XXsim
    ZZ[,2] <- ZZ[,1]
    ZZ[,1] <- Zsim
    if (m < M){
      # The following is only to prevent the error that would occur
      # in the very unlikely event XX[,,1] == XX[,,2] or ZZ[,1] == ZZ[,2].
      if (identical(XX[,,1], XX[,,2])){XX[1,2,1] <- 1 - XX[1,2,2]}
      if (identical(ZZ[,1], ZZ[,2])){
        ZZ[1,1] <- ifelse((ZZ[1,1] == 1), 2, 1)}
      # Specify the two-wave network data set starting with XX[,,1].
      X <- sienaDependent(XX, allowOnly = FALSE)
      Z <- sienaDependent(ZZ, type = 'behavior', allowOnly = FALSE)
      # Simulate wave m+1 starting at XX[,,1] which is the previous XXsim
      InitData  <- sienaDataCreate(X, Z, Va, Vb)
      InitSim <- siena07(InitAlg, data=InitData, eff=InitEff,
                         returnDeps=TRUE, batch=TRUE, silent=TRUE)
    }
  }
  # Present the average degrees to facilitate tuning the outdegree parameter
  # to achieve a desired average value for the average degrees.
  cat("Average degrees ", round(colSums(Xs,dims=2)/n, digits=2), "\n")
  cat("Average behavior ", round(colSums(Zs)/n, digits=2), "\n")
  # Result: simulated data set; covara and covarb are vectors of length n;
  # networks is an array of dimension nxnxM; 
  # behaviors is a matrix of dimension nxM
  list(covara = V0, covarb = V1, networks = Xs, behaviors = Zs)
}


###############################################################################
###                                                                         ###
###         Example                                                         ###
###                                                                         ###
###############################################################################

# Trial values:
n <- 20
M <- 4
rate <- 2
dens <- -1.9
rec <- 2
tt <- 0.3
c3 <- -0.3
Vaego <- 0
Vaalt <- 0
Vasim <- 0.6
Vbego <- 0.5
Vbalt <- -0.5
Vbsim <- 0.5
c <- 4
rate.b <- 1
lin.b <- 0.1
qu.b <- -0.3
avalt.b <- 0.5

# Example call:
SN <- SimulateNetworksBehavior(n, M, c, rate, dens, rec, tt, c3, Vaego, Vaalt, Vasim,
                               Vbego, Vbalt, Vbsim, rate.b, lin.b, qu.b, avalt.b)
# Results:
SN[[1]]
# the same as
SN$covara

SN[[2]]
# the same as
SN$covarb

SN[[3]]
# the same as
SN$networks

SN[[4]]
# the same as
SN$behaviors
