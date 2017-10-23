# refit-models csafe-01 server
library(RSiena)
library(dplyr)
library(tibble)
library(purrr)

load("Senate_lineup_data_Siena.RDA")
load("allModelMeans.RDS")
modelMeanEsts <- modelMeanEsts %>% add_row(model = "bigmod", ests = NA)
modelMeanEsts$ests <- list(modelMeanEsts$ests[[1]], modelMeanEsts$ests[[2]],
                           modelMeanEsts$ests[[3]], modelMeanEsts$ests[[4]], 
                           modelMeanEsts$ests[[5]],modelMeanEsts$ests[[6]],
                           c(2.4405048,2.4594403,2.2098176,-4.9232775,4.8916183,2.3743720,0.2047038,6.9661589))


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

lineup_data$sienaFits <- list(0)

lineup_data_basic <- lineup_data %>% filter(model == "basic")
lineup_data_jttp <- lineup_data %>% filter(model == "jttp")
lineup_data_jtts <- lineup_data %>% filter(model == "jtts")
lineup_data_samettp <- lineup_data %>% filter(model == "samettp")
lineup_data_simttb <- lineup_data %>% filter(model == "simttb")
lineup_data_bigmod <- lineup_data %>% filter(model == "bigmod")

for (i in 1:nrow(lineup_data_basic)){
  lineup_data_basic$sienaFits[[i]] <- try(refit_models(dat = lineup_data_basic$newSienaDat[[i]], mod = lineup_data_basic$model[i]))
  save(lineup_data_basic, file = "lineup_data_refits_basic.RDA")
} 
# for (i in 1:nrow(lineup_data_jttp)){
#   lineup_data_jttp$sienaFits[[i]] <- try(refit_models(dat = lineup_data_jttp$newSienaDat[[i]], mod = lineup_data_jttp$model[i]))
#   save(lineup_data_jttp, file = "lineup_data_refits_jttp.RDA")
# }
for (i in 1:nrow(lineup_data_jtts)){
  lineup_data_jtts$sienaFits[[i]] <- try(refit_models(dat = lineup_data_jtts$newSienaDat[[i]], mod = lineup_data_jtts$model[i]))
  save(lineup_data_jtts, file = "lineup_data_refits_jtts.RDA")
} 
for (i in 1:nrow(lineup_data_samettp)){
  lineup_data_samettp$sienaFits[[i]] <- try(refit_models(dat = lineup_data_samettp$newSienaDat[[i]], mod = lineup_data_samettp$model[i]))
  save(lineup_data_samettp, file = "lineup_data_refits_samettp.RDA")
} 
# for (i in 1:nrow(lineup_data_simttb)){
#   lineup_data_simttb$sienaFits[[i]] <- try(refit_models(dat = lineup_data_simttb$newSienaDat[[i]], mod = lineup_data_simttb$model[i]))
#   save(lineup_data_simttb, file = "lineup_data_refits_simttb.RDA")
# }
# for (i in 1:nrow(lineup_data_bigmod)){
#   lineup_data_bigmod$sienaFits[[i]] <- try(refit_models(dat = lineup_data_bigmod$newSienaDat[[i]], mod = lineup_data_bigmod$model[i]))
#   save(lineup_data_bigmod, file = "lineup_data_refits_bigmod.RDA")
# }
