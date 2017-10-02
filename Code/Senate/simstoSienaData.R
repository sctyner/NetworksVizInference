# write function to take simulated data and turn it into siena data. 
library(tidyverse)
library(RSiena)
load("Data/senate/se111adjmat.RDS")
load("Data/senate/allSensAdjMatEmpty.RDS")
senateCovars <- read_csv('Data/senate/senateCovars.csv')

# turn a wave df into an adjmat 
make_se_adjmat2 <- function(adjmat=allSensAMempty, wave){
  N <- nrow(wave)
  for(i in 1:N){
    if(!(is.na(wave[i,c("to")]))){
    k <- which(rownames(adjmat) == wave$from[i])
    l <- which(colnames(adjmat) == wave$to[i])
    adjmat[k, l] <- 1
    }
  }
  return(adjmat)
}
# turn 3 waves into a 4-wave sienaData object
dat_to_SienaDat <- function(wave1 = se111adj, Sim, covars = senateCovars){
  wave2 <- filter(Sim, wave == 1)
  wave2$from <- replace(wave2$from, 1:length(wave2$from), covars$source[wave2$from])
  wave2$to <- replace(wave2$to, 1:length(wave2$to), covars$source[wave2$to])
  wave2 <- make_se_adjmat2(wave = wave2)
  wave3 <- filter(Sim, wave == 2)
  wave3$from <- replace(wave3$from, 1:length(wave3$from), covars$source[wave3$from])
  wave3$to <- replace(wave3$to, 1:length(wave3$to), covars$source[wave3$to])
  wave3 <- make_se_adjmat2(wave = wave3)
  wave4 <- filter(Sim, wave == 3)
  wave4$from <- replace(wave4$from, 1:length(wave4$from), covars$source[wave4$from])
  wave4$to <- replace(wave4$to, 1:length(wave4$to), covars$source[wave4$to])
  wave4 <- make_se_adjmat2(wave = wave4)
  
  senateData <- array( c( wave1, wave2, wave3, wave4 ),
                       dim = c( 155, 155, 4 ) )
  senateData2 <- sienaDependent(senateData)
  party <- coCovar(val = as.numeric(as.factor(covars$party)))
  # party 1 is dem 2 is ind 3 is rep
  sex <- coCovar(val = as.numeric(as.factor(covars$party)))
  # sex 1 is F 2 is M
  nbills_au <- senateCovars[,4:7]
  names(nbills_au) <- paste0("Sen",111:114) 
  nbills_au <- as.matrix(nbills_au)
  nbills_au[which(is.na(nbills_au))] <- 0
  rownames(nbills_au) <- senateCovars$source
  bills <- varCovar(nbills_au)
  sienaData <- sienaDataCreate( senateData2, bills, sex, party )
  return(sienaData)
}

# use purrr to make a list df with models, sims, df, and sienaDat columns

M1sims <- read_csv("Data/senate/FauxTrueData/M1senSimDat.csv")
jttssims <- read_csv("Data/senate/FauxTrueData/jttsSenSimDat.csv")
jttpsims <- read_csv("Data/senate/FauxTrueData/jttpSenSimDat.csv")
samepsims <- read_csv("Data/senate/FauxTrueData/samepSenSimDat.csv")
simttbsims <- read_csv("Data/senate/FauxTrueData/simttbSenSimDat.csv")
samettpsims <- read_csv("Data/senate/FauxTrueData/samettpSenSimDat.csv")

M1sims <- M1sims %>% select(from, to, wave, sim) %>% add_column(model = 'basic')
jttssims <- jttssims %>% select(from, to, wave, sim) %>% add_column(model = 'jtts')
jttpsims <- jttpsims %>% select(from, to, wave, sim) %>% add_column(model = 'jttp')
samepsims <- samepsims %>% select(from, to, wave, sim) %>% add_column(model = 'samep')
simttbsims <- simttbsims %>% select(from, to, wave, sim) %>% add_column(model = 'simttb')
samettpsims <- samettpsims %>% select(from, to, wave, sim) %>% add_column(model = 'samettp')

allSims <- do.call(rbind, list(M1sims, jttssims, jttpsims, samepsims, simttbsims, samettpsims))
allSims %>% group_by(model, sim) %>% nest() -> allSimsNested


allSimsNested %>% 
  mutate(sienaDat = map(data, dat_to_SienaDat, wave1 = se111adj, covars=senateCovars)) -> allSimsNested2
allSimsNested2 %>% filter(sim %in% 1:100) -> allSimsNested3
sienaDatSimsOrigParams <- allSimsNested3
save(sienaDatSimsOrigParams, file = "Data/senate/FauxTrueData/SimsfrommodelsOriginalvalues100.RDS")
# do another set of same p sims because they're so much larger than the others....test
## samepdat2 <- saom_simulate(dat = senateSiena, struct = Sensame_p, parms = samepSenMeans, N=1000)
## samepdat2df <- netvizinf::sims_to_df(samepdat2)
# about 109,000 rows. it's fine i guess.

