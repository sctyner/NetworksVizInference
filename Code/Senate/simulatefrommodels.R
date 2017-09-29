# Simulating "data" from the models
setwd("~/Desktop/Dissertation/NetworksVizInference/")
library(RSiena)
library(netvizinf)
library(geomnet)

load("Data/senate/senateSienaNoHRC.rda")
# basic effects structure
SenBasic <- getEffects(senateSiena)
# basic algorithm structure
myalg <- sienaAlgorithmCreate( projname = Sys.time() , n3 = 1000)
# additional effects structures
# jumpXTransTrip party
Senjtt_p <- includeEffects(SenBasic, "jumpXTransTrip", include = TRUE, type = "eval", interaction1 = "party", character = TRUE)
# jumpXTransTrip sex
Senjtt_s <- includeEffects(SenBasic, "jumpXTransTrip", include = TRUE, type = "eval", interaction1 = "sex", character = TRUE) 
# indegree isolates
# SeninIsD <- includeEffects(SenBasic, "inIsDegree", include = TRUE, type = "eval", interaction1 = "", character = TRUE)
# sameX party
Sensame_p <- includeEffects(SenBasic, "sameX", include = TRUE, type = "eval", interaction1 = "party", character = TRUE)
# simXTransTrip bills
Senstt_b <- includeEffects(SenBasic, "simXTransTrip", include = TRUE, type = "eval", interaction1 = "bills", character = TRUE)
# sim recip party
#Sensimre_p <- includeEffects(SenBasic, "simRecipX", include = TRUE, type = "eval", interaction1 = "party", character = TRUE)
Sentt_p <- includeEffects(SenBasic, "transTrip", include = TRUE, type = "eval", interaction1 = "", character = TRUE)
# Senstt_p <- includeEffects(SenBasic, "sameXTransTrip", include = TRUE, type = "eval", interaction1 = "party", character = TRUE)

# means of parameters to use as starting values
M1senateEsts <- read_csv("Data/senate/basicModelFitsSenate1000.csv")
#inisdSenateEsts <- read_csv("Data/senate/inisdModelFitsSenate1000.csv")
jttpSenateEsts <- read_csv("Data/senate/jttpModelFitsSenateAll1000.csv")
jttsSenateEsts <- read_csv("Data/senate/jttsModelFitsSenateAll1000.csv")
samepSenateEsts <- read_csv("Data/senate/samepModelFitsSenateall1000.csv")
simttbSenateEsts <- read_csv("Data/senate/simttbModelFitsSenateAll1000.csv")

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



######################################################################################################
# Dont need to do again!  
# simulate "data" 
# set a seed
#set.seed(271498)
#M1dat <- saom_simulate(dat = senateSiena, struct = SenBasic, parms = M1SenMeans, N = 1000)
#jttpdat <- saom_simulate(dat = senateSiena, struct = Senjtt_p, parms = jttpSenMeans, N = 1000)
#jttsdat <- saom_simulate(dat = senateSiena, struct = Senjtt_s, parms = jttsSenMeans, N = 1000)
#samepdat <- saom_simulate(dat = senateSiena, struct = Sensame_p, parms = samepSenMeans, N=1000)
simttbdat <- saom_simulate(dat = senateSiena, struct = Senstt_b, parms = simttbSenMeans, N=1000)


#M1datavgSen2 <- net_avg(sims = M1dat, wave = 1)
#M1datavgSen3 <- net_avg(sims = M1dat, wave = 2)
#M1datavgSen4 <- net_avg(sims = M1dat, wave = 3)

#M1datdf <- sims_to_df(M1dat)
#jttpdatdf <- sims_to_df(jttpdat)
#jttsdatdf <- sims_to_df(jttsdat)
#samepdatdf <- sims_to_df(samepdat) 
simttbdatdf <- sims_to_df(simttbdat)

#write_csv(M1datdf, 'Data/senate/“TrueData"/M1senSimDat.csv')
#write_csv(jttpdatdf, 'Data/senate/“TrueData"/jttpSenSimDat.csv')
#write_csv(jttsdatdf, 'Data/senate/“TrueData"/jttsSenSimDat.csv')
#write_csv(samepdatdf, 'Data/senate/FauxTrueData/samepSenSimDat.csv')
#write_csv(simttbdatdf, "Data/senate/FauxTrueData/simttbSenSimDat.csv")
######################################################################################################



M1datdf %>% group_by(wave, from, to) %>% summarise(count = n()) %>% ungroup() %>% 
  mutate(from = paste0("sen", from), to = paste0("sen", to)) %>% 
  filter(count >= 200) 

jttpdatdf %>% group_by(wave, from, to) %>% summarise(count = n()) %>% ungroup() %>% 
  mutate(from = paste0("sen", from), to = ifelse(is.na(to), NA, paste0("sen", to))) %>% 
  filter(count >=200) %>% data.frame()
ggplot() + 
  geom_net(layout.alg = "kamadakawai", aes(from_id = from, to_id = to, linewidth = count/858),  fiteach = T) + 
  theme_net() + 
  facet_wrap(~wave)
dummyorder <- sample(5)
M1datdf %>% filter(wave==1, sim %in% 1:4) %>% 
  mutate(from = paste0("V", from), to = ifelse(is.na(to), to, paste0("V", to))) %>%
  add_row(from = sen111$from, to = sen111$to, sim = 1001) %>% 
  mutate(order = rep(dummyorder, c(146,122,134,124, 130))) %>% 
  ggplot() + 
  geom_net(layout.alg = "fruchtermanreingold",directed = T,arrowsize = .2,
           aes(from_id = from, to_id = to),  fiteach = T, size = 1, linewidth = .25, color = 'navy') + 
  theme_net() + 
  facet_wrap(~order)

sen111 <- geomnet:::fortify.adjmat(geomnet::as.adjmat(senateData[,,1]))
sen111$from <- paste0("V", sen111$from)

ggplot(sen111) + 
  geom_net(layout.alg = "fruchtermanreingold",directed = T,arrowsize = .2,
           aes(from_id = from, to_id = to), fiteach = T, size = 1, linewidth = .25, color = 'navy') + 
  theme_net()
    
