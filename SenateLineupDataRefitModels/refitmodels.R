# refit models to data for lineup experiment.
library(RSiena)
library(tidyverse)
setwd("../SenateLineupDataRefitModels/")
# senate data
load("../Data/senate/se111adjmat.RDS")
load("../Data/senate/se112adjmat.RDS")
load("../Data/senate/se113adjmat.RDS")
load("../Data/senate/se114adjmat.RDS")
senateCovars <- read_csv('../Data/senate/senateCovars.csv')
party <- coCovar(val = as.numeric(as.factor(senateCovars$party)))
# party 1 is dem 2 is ind 3 is rep
sex <- coCovar(val = as.numeric(as.factor(senateCovars$party)))
# sex 1 is F 2 is M
nbills_au <- senateCovars[,4:7]
names(nbills_au) <- paste0("Sen",111:114) 
nbills_au <- as.matrix(nbills_au)
nbills_au[which(is.na(nbills_au))] <- 0
rownames(nbills_au) <- senateCovars$source
bills <- varCovar(nbills_au)

# initial mean values to use as starting vals 
load("../shinyApp/dat/allModelMeans.RDS")
modelMeanEsts <- modelMeanEsts %>% add_row(model = "bigmod", ests = NA)
modelMeanEsts$ests <- list(modelMeanEsts$ests[[1]], modelMeanEsts$ests[[2]],
                           modelMeanEsts$ests[[3]], modelMeanEsts$ests[[4]], 
                           modelMeanEsts$ests[[5]],modelMeanEsts$ests[[6]],
                           c(2.4405048,2.4594403,2.2098176,-4.9232775,4.8916183,2.3743720,0.2047038,6.9661589))
modelMeanEsts

## necessary functions
# take a df and make an adjmat
make_adjmat <- function(d) {
  d %>% right_join(df) %>% 
    mutate(from = parse_number(from), to = parse_number(to)) %>% 
    xtabs(!is.na(dep.var.id)~from+to, data =.) %>% as.matrix()
}
# take a lineup and make all data adjmats
make_all_adjmats <- function(dat){
  dat %>% nest(-sim) %>% mutate(adjmats = map(data, make_adjmat))
}
# take a wave and put it in senate siena data
make_new_senate_siena <- function(wave2){
  wave1 <- se111adj
  wave3 <- se113adj
  wave4 <- se114adj
  rownames(wave2) <- rownames(se111adj)
  colnames(wave2) <- colnames(se111adj)
  senateData <- array( c( wave1, wave2, wave3, wave4 ),
                       dim = c( 155, 155, 4 ) )
  senateData2 <- sienaDependent(senateData)
  sienaData <- sienaDataCreate( senateData2, bills, sex, party )
  return(sienaData)
}
# make an empty full edgelist
df <- data.frame(expand.grid(
  from=paste0("V", 1:155), to = paste0("V", 1:155)), stringsAsFactors = FALSE)

library(tidyverse)
library(RSiena)

nongof_files <- list.files("Non-GoF/")
gof_files <- list.files("GoF/")

lineup_data <- data_frame(GOF = rep(c(F, T), c(length(nongof_files), length(gof_files))),
                          data_file = c(nongof_files, gof_files))

lineup_data <- mutate(lineup_data, data_name = data_file) %>% 
  separate(data_file, c("model", "type", "difficulty", "rep"), sep = "_") %>% 
  select(GOF, model, data_name)
lineup_data$model <- as.factor(lineup_data$model)
levels(lineup_data$model) <- c("bigmod","basic","jttp","jtts","basic","samettp","simttb")
lineup_data$model <- as.character(lineup_data$model)
lineup_data

lineup_data <- lineup_data %>% mutate(filePath = ifelse(GOF, paste0("GoF/", data_name), paste0("Non-GoF/",data_name)))

lineup_data <- lineup_data %>% mutate(data = map(filePath, read_csv))                                      
lineup_data <- lineup_data %>% mutate(AllAMs = map(data, make_all_adjmats))

#save(lineup_data, file = "lineupdataAdjMats.RDA")
#load("SenateLineupDataRefitModels/lineupdataAdjMats.RDA")

lineup_data <- lineup_data %>% unnest(AllAMs)
lineup_data <- lineup_data %>% mutate(newSienaDat = map(adjmats, make_new_senate_siena))

# refit models to data 
refit_models <- function(dat, mod){
  SenBasic <- getEffects(dat)
  myalg <- sienaAlgorithmCreate( projname = Sys.time() , n3 = 1000, firstg = .02)
  if (mod == "basic"){
    myeffects <- SenBasic
    myeffects$initialValue[myeffects$include] <- modelMeanEsts[modelMeanEsts$model == "basic","ests"][[1]][[1]]
  } else if (mod == "jttp"){
    myeffects <- includeEffects(SenBasic, "jumpXTransTrip", include = TRUE, type = "eval", interaction1 = "party", character = TRUE)
    myeffects$initialValue[myeffects$include] <- modelMeanEsts[modelMeanEsts$model == "jttp","ests"][[1]][[1]]
  } else if (mod == "jtts"){
    myeffects <- includeEffects(SenBasic, "jumpXTransTrip", include = TRUE, type = "eval", interaction1 = "sex", character = TRUE)
    myeffects$initialValue[myeffects$include] <- modelMeanEsts[modelMeanEsts$model == "jtts","ests"][[1]][[1]]
  } else if (mod == "simttb"){
    myeffects <- includeEffects(SenBasic, "simXTransTrip", include = TRUE, type = "eval", interaction1 = "bills", character = TRUE)
    myeffects$initialValue[myeffects$include] <- modelMeanEsts[modelMeanEsts$model == "simttb","ests"][[1]][[1]]
  } else if (mod == "samettp"){   
    myeffects <- includeEffects(SenBasic, "sameXTransTrip", include = TRUE, type = "eval", interaction1 = "party", character = TRUE)
    myeffects$initialValue[myeffects$include] <- modelMeanEsts[modelMeanEsts$model == "samettp","ests"][[1]][[1]]
  } else { # if mod == "bigmod"
    myeffects <- includeEffects(SenBasic, "jumpXTransTrip", include = TRUE, type = "eval", interaction1 = "sex", character = TRUE)
    myeffects <- includeEffects(myeffects, "simXTransTrip", include = TRUE, type = "eval", interaction1 = "bills", character = TRUE)
    myeffects <- includeEffects(myeffects, "sameXTransTrip", include = TRUE, type = "eval", interaction1 = "party", character = TRUE)
    myeffects$initialValue[myeffects$include] <- modelMeanEsts[modelMeanEsts$model == "bigmod","ests"][[1]][[1]]
  }
  
  fits <- siena07(myalg, data = dat, effects = myeffects, returnDeps = FALSE,
                  batch=TRUE, verbose = FALSE, silent = TRUE)
  fits
}

lineup_data_jttp <- lineup_data %>% filter(model == "jttp")
lineup_data$sienaFits <- list()
for (i in 1:nrow(lineup_data)){
  print(i)
  lineup_data$sienaFits[[i]] <- refit_models(dat = lineup_data$newSienaDat[[i]], mod = lineup_data$model[i])
}
#sienaFits = map2(newSienaDat, model, refit_models)
# [1] 55
# Error in x$FRAN(zsmall, xsmall) : 
#   Unlikely to terminate this epoch:  more than 1000000 steps
# In addition: Warning message:
#   Unknown or uninitialised column: 'sienaFits'. 
for (i in 56:nrow(lineup_data)){
  print(i)
  lineup_data$sienaFits[[i]] <- refit_models(dat = lineup_data$newSienaDat[[i]], mod = lineup_data$model[i])
}
# [1] 61
# Error in x$FRAN(zsmall, xsmall) : 
#   Unlikely to terminate this epoch:  more than 1000000 steps
for (i in 62:nrow(lineup_data)){
  print(i)
  lineup_data$sienaFits[[i]] <- refit_models(dat = lineup_data$newSienaDat[[i]], mod = lineup_data$model[i])
}
# [1] 67
# Error in x$FRAN(zsmall, xsmall) : 
#   Unlikely to terminate this epoch:  more than 1000000 steps
# Called from: x$FRAN(zsmall, xsmall)
for (i in 68:nrow(lineup_data)){
  print(i)
  lineup_data$sienaFits[[i]] <- refit_models(dat = lineup_data$newSienaDat[[i]], mod = lineup_data$model[i])
}
# [1] 73
# Error in x$FRAN(zsmall, xsmall) : 
#   Unlikely to terminate this epoch:  more than 1000000 steps
# Called from: x$FRAN(zsmall, xsmall)
for (i in 74:nrow(lineup_data)){
  print(i)
  lineup_data$sienaFits[[i]] <- refit_models(dat = lineup_data$newSienaDat[[i]], mod = lineup_data$model[i])
}
# [1] 79
# Error in x$FRAN(zsmall, xsmall) : 
#   Unlikely to terminate this epoch:  more than 1000000 steps
# Called from: x$FRAN(zsmall, xsmall)
for (i in 1:nrow(lineup_data)){
  print(i)
  lineup_data$sienaFits[[i]] <- try(refit_models(dat = lineup_data$newSienaDat[[i]], mod = lineup_data$model[i]))
  save(lineup_data, file = "lineup_data_refits.RDA")
} 
save(lineup_data, file = "Senate_lineup_data_Siena.RDA")


