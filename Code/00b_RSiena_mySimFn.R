# simulate the networks 

# M = number of waves to simulate
# init.waves = initial array of network observations
# V0 = categorical actor variable
# 

SimulateNextWave <- function(init.waves, V0, 
                                     rate.params.names, rate.params.vals,
                                     eval.params.names, eval.params.vals,
                                     eval.int.params.names, eval.int.params.vals){
  if (ncol(init.waves[,,1]) != nrow(init.waves[,,1])){
    return("Error: Non-square adjacency matrix provided")
  } 
  if (length(eval.params.vals) != length(eval.params.names)){
    return("Error: Number of eval parameters to include in model does not
           match number of eval parameter values provided.")
  }
  if (length(rate.params.vals) != length(rate.params.names)){
    return("Error: Number of rate parameters to include in model does not
           match number of rate parameter values provided.")
  }
  if (length(eval.int.params.vals) != length(eval.int.params.names)){
    return("Error: Number of interaction eval parameters to include in model does not
           match number of interaction eval parameter values provided.")
  }
  init.waves <- friendshipData
  n <- ncol(init.waves[,,1])
  V0 <- drink
  c <- length(unique(V0[,1]))
  p.e <- length(eval.params.names)
  p.r <- length(rate.params.names)
  p.e.i <- length(eval.int.params.names)
  # Simulates M consecutive network and behavior waves, with n actors,
  # with c categories of the behavior variable, from initial network init.net.adj
  # according to a stochastic actor-oriented model with actor covariate V0
  # with parameter values params.values for the parameters of interest params.names
  Va <- varCovar(V0)
  X   <- sienaDependent(init.waves)
  InitData <- sienaDataCreate(X, Va)
  InitEff0 <- getEffects(InitData)
  # sink to avoid printing to the screen
  #sink("eff.txt")
  # Specify the parameters.
  # The rate parameters are first multiplied by 10,
  # which will be used only to get from the totally random network XX[,,1] = X0
  # to the network that will be the simulated first wave.
  
  for (i in 1:p.r){
    InitEff0 <- setEffect(InitEff0, rate.params.names[i], type = 'rate', initialValue = rate.params.vals[i], period = i, character = T)
  }
  for (i in 1:p.e){
    InitEff0 <- setEffect(InitEff0, eval.params.names[i], type = 'eval', initialValue = eval.params.vals[i], character = T)
  }
  for (i in 1:p.e.i){
    InitEff0 <- setEffect(InitEff0, eval.int.params.names[i], type = 'eval', interaction1 = "Va", initialValue = eval.int.params.vals[i], character = T)
  }
  
  ## The parameter given for n3 should be larger than sum(InitEff0$include)
  InitAlg <- sienaAlgorithmCreate(projname="Init", nsub=0,
                                  cond=FALSE, simOnly=TRUE)
  # Simulate the first wave.
  InitSim   <- siena07(InitAlg, data=InitData, eff=InitEff0,
                       returnDeps=TRUE, batch=TRUE, silent=TRUE)
  return(InitSim$sims)
}   
