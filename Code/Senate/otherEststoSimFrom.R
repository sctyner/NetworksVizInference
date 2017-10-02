# simulate from different parameter values 

M1SenMeans <- M1senateEsts %>% filter(maxConv <=0.25) %>% summarise_at(.cols = 1:5, .funs = mean) %>% as.numeric()
M1SenNconv <-  M1senateEsts %>% filter(maxConv <=0.25) %>% count() %>% as.numeric
jttpSenMeans <- jttpSenateEsts %>% filter(maxConv <=0.25) %>% summarise_at(.cols = 2:7, .funs = mean) %>% as.numeric()
jttpSenNconv <- jttpSenateEsts %>% filter(maxConv <=0.25) %>% count() %>% as.numeric
jttsSenMeans <- jttsSenateEsts %>% filter(maxConv <=0.25) %>% summarise_at(.cols = 2:7, .funs = mean) %>% as.numeric()
jttsSenNconv <- jttsSenateEsts %>% filter(maxConv <=0.25) %>% count() %>% as.numeric
samepSenMeans <- samepSenateEsts %>% filter(maxConv <=0.25) %>% summarise_at(.cols = 2:7, .funs = mean) %>% as.numeric()
samePSenNconv <- samepSenateEsts %>% filter(maxConv <=0.25) %>% count() %>% as.numeric
simttbSenMeans <- simttbSenateEsts %>% filter(maxConv <= 0.25) %>% summarise_at(.cols = 2:7, .funs = mean) %>% as.numeric()
simttbSenNconv <- simttbSenateEsts %>% filter(maxConv <=0.25) %>% count() %>% as.numeric
samettpSenMeans <- samettpSenateEsts %>% filter(maxConv <= 0.25) %>% summarise_at(.cols = 2:7, .funs = mean) %>% as.numeric()
samettpSenNconv <- samettpSenateEsts %>% filter(maxConv <=0.25) %>% count() %>% as.numeric


basicParms <- (M1SenEstsLong %>% select(parameter) %>% unique())[[1]] 
jttpParms <- (jttpSenLong %>% select(parameter) %>% unique())[[1]] 
jttsParms <- (jttsSenLong %>% select(parameter) %>% unique())[[1]] 
samepParms <- (samepSenLong %>% select(parameter) %>% unique())[[1]] 
simttbParms <- (simttbSenLong %>% select(parameter) %>% unique())[[1]] 
samettpParms <- (samettpSenLong %>% select(parameter) %>% unique())[[1]] 

SAOMparmVals <- data.frame(model = rep(c("basic", "jttp", "jtts", "samep", "simttb", "samettp"), c(5,6,6,6,6,6)),
           parameter = c(basicParms, jttpParms, jttsParms, samepParms, simttbParms, samettpParms),
           origEsts = c(M1SenMeans, jttpSenMeans, jttsSenMeans, samepSenMeans, simttbSenMeans, samettpSenMeans))
SAOMparmVals %>% mutate(double = ifelse(parameter %in% c("Rate", "Rate_1", "Rate_2", "density", "recip"), origEsts, 2*origEsts), 
                        quint = ifelse(parameter %in% c("Rate", "Rate_1", "Rate_2", "density", "recip"), origEsts, 5*origEsts), 
                        #oppositeD = ifelse(parameter == "density", -origEsts, origEsts), 
                        #oppositeR = ifelse(parameter == "recip", -origEsts, origEsts),
                        oppositeDR = ifelse(parameter %in% c("density", "recip"), -origEsts, origEsts), 
                        opposite = ifelse(parameter %in% c("Rate", "Rate_1", "Rate_2", "density", "recip"), origEsts, -origEsts), 
                        halve = ifelse(parameter %in% c("Rate", "Rate_1", "Rate_2", "density", "recip"), origEsts, origEsts/2),
                        tenx = ifelse(parameter %in% c("Rate", "Rate_1", "Rate_2", "density", "recip"), origEsts, origEsts*10) 
                        ) -> SAOMparmVals

head(SAOMparmVals)

SAOMparmVals %>% select(-parameter) %>% 
  gather(condition, paramVals, origEsts:tenx) %>% 
  nest(paramVals, .key = inits) %>% 
  mutate(inits = map(inits, function(x){as.numeric(x[[1]])})) -> SAOMparmValsNest
idxchg <- which(SAOMparmValsNest$model =='basic' & SAOMparmValsNest$condition != "oppositeDR")
SAOMparmValsNest$model[idxchg[-6]] <- levels(SAOMparmValsNest$model)[-1]
SAOMparmValsNest$condition[idxchg[-6]] <- "compBasic"
SAOMparmValsNest <- SAOMparmValsNest[-idxchg[6],]

SAOMparmValsNest %>% 
  filter(!(condition == 'oppositeDR' & model !='basic')) %>% 
  arrange(model, condition) -> simstoget


# combine siena structures and data
SenBasic <- getEffects(senateSiena)
# additional effects structures
# jumpXTransTrip party
Senjtt_p <- includeEffects(SenBasic, "jumpXTransTrip", include = TRUE, type = "eval", interaction1 = "party", character = TRUE)
# jumpXTransTrip sex
Senjtt_s <- includeEffects(SenBasic, "jumpXTransTrip", include = TRUE, type = "eval", interaction1 = "sex", character = TRUE) 
# sameX party
Sensame_p <- includeEffects(SenBasic, "sameX", include = TRUE, type = "eval", interaction1 = "party", character = TRUE)
# simXTransTrip bills
Senstt_b <- includeEffects(SenBasic, "simXTransTrip", include = TRUE, type = "eval", interaction1 = "bills", character = TRUE)
# sim recip party
Senstt_p <- includeEffects(SenBasic, "sameXTransTrip", include = TRUE, type = "eval", interaction1 = "party", character = TRUE)

structstib <- tibble(model = unique(SAOMparmVals$model), 
                     structs = list(SenBasic, Senjtt_p, Senjtt_s, Sensame_p, Senstt_b, Senstt_p))

left_join(simstoget, structstib) -> simstoget

simstoget[simstoget$condition == 'compBasic',][[4]] <- list(SenBasic, SenBasic, SenBasic, SenBasic, SenBasic)
 
simstoget[c(1,2,9,16, 23, 30),] -> testgetsims

testgetsims$dat <- list(0L)
for(i in 1:nrow(testgetsims)){
  testgetsims$dat[[i]] <- senateSiena
}
testgetsims$N <- 110

testgotsims <- testgetsims %>%   
  mutate(sims = pmap(.f = saom_simulate2, 
                                   .l = list(dat = dat, struct = structs, parms = inits, N = N)))
## IT WORKSSSSSSS 

# do it for all of themssssss!!!! 

simstoget$dat <- list(0L)
for(i in 1:nrow(simstoget)){
  simstoget$dat[[i]] <- senateSiena
}
simstoget$N <- (20-1)*10

gotsims <- simstoget %>%   
  mutate(sims = pmap(.f = saom_simulate2, 
                     .l = list(dat = dat, struct = structs, parms = inits, N = N)))
save(gotsims, file = "Data/senate/FauxTrueData/M_1_sims_for_fdata.RDS")

