# Exploratory Data analyses
library(tidyverse)
M1senateEsts <- read_csv("Data/senate/basicModelFitsSenate1000.csv")
#inisdSenateEsts <- read_csv("../Data/senate/inisdModelFitsSenate1000.csv")
jttpSenateEsts <- read_csv("Data/senate/jttpModelFitsSenateAll1000.csv")
jttsSenateEsts <- read_csv("Data/senate/jttsModelFitsSenateAll1000.csv")
samepSenateEsts <- read_csv("Data/senate/samepModelFitsSenateall1000.csv")
simttbSenateEsts <- read_csv("Data/senate/simttbModelFitsSenateAll1000.csv")
samettpSenateEsts <- read_csv("Data/senate/samettpModelFitsSenateAll1000.csv")

# ggpairs(M1senateEsts, columns = 1:8)
# M1senateEsts %>% mutate(converged = (maxConv <= 0.25)) %>% 
#   ggplot() + 
#   geom_density(aes(x = density, color = converged), adjust = .5) 
# M1senateEsts %>% mutate(converged = (maxConv <= 0.25)) %>% 
#   ggplot() + 
#   geom_histogram(aes(x = density, color = converged), binwidth = .005, fill = NA) + facet_wrap(~converged)

# make ests long - M1
M1senateEsts %>% 
  select(Rate:recip, maxConv, sim) %>% 
  gather(parameter, estimate, Rate:recip) -> melt1 
M1senateEsts %>% 
  select(se_density:sim) %>% 
  gather(parameter, se, se_density:se_recip) %>% 
  mutate(parameter = str_replace(parameter, "se_", "")) -> melt2 
left_join(melt1, melt2) %>% arrange(sim) -> M1SenEstsLong 

# make ests long - inisd
# inisdSenateEsts %>% 
#   select(Rate:inIsDegree, maxConv, sim) %>% 
#   gather(parameter, estimate, Rate:inIsDegree) -> melt1 
# inisdSenateEsts %>% 
#   select(se_density:sim) %>% 
#   gather(parameter, se, se_density:se_inIsDegree) %>% 
#   mutate(parameter = str_replace(parameter, "se_", "")) -> melt2 
# left_join(melt1, melt2) %>% arrange(sim) -> inisdSenEstsLong

# make ests long - jttp
jttpSenateEsts %>% 
  select(Rate:jumpXTransTrip, maxConv, sim) %>% 
  gather(parameter, estimate, Rate:jumpXTransTrip) -> melt1 
jttpSenateEsts %>% 
  select(se_density:maxConv, sim) %>% 
  gather(parameter, se, se_density:se_jumpXTransTrip) %>% 
  mutate(parameter = str_replace(parameter, "se_", "")) -> melt2 
left_join(melt1, melt2) %>% arrange(sim) -> jttpSenLong

# make ests long - jtts
jttsSenateEsts %>% 
  select(Rate:jumpXTransTrip, maxConv, sim) %>% 
  gather(parameter, estimate, Rate:jumpXTransTrip) -> melt1 
jttsSenateEsts %>% 
  select(se_density:maxConv, sim) %>% 
  gather(parameter, se, se_density:se_jumpXTransTrip) %>% 
  mutate(parameter = str_replace(parameter, "se_", "")) -> melt2 
left_join(melt1, melt2) %>% arrange(sim) -> jttsSenLong

# make ests long - samep
samepSenateEsts %>% 
  select(Rate:sameX, maxConv, sim) %>% 
  gather(parameter, estimate, Rate:sameX) -> melt1 
samepSenateEsts %>% 
  select(se_density:sim) %>% 
  gather(parameter, se, se_density:se_sameX) %>% 
  mutate(parameter = str_replace(parameter, "se_", "")) -> melt2 
left_join(melt1, melt2) %>% arrange(sim) -> samepSenLong

# make ests long - simttb
simttbSenateEsts %>% 
  select(Rate:simXTransTrip, maxConv, sim) %>% 
  gather(parameter, estimate, Rate:simXTransTrip) -> melt1 
simttbSenateEsts %>% 
  select(se_density:maxConv, sim) %>% 
  gather(parameter, se, se_density:se_simXTransTrip) %>% 
  mutate(parameter = str_replace(parameter, "se_", "")) -> melt2 
left_join(melt1, melt2) %>% arrange(sim) -> simttbSenLong

# make ests long - samettp
samettpSenateEsts %>% 
  select(Rate:sameXTransTrip, maxConv, sim) %>% 
  gather(parameter, estimate, Rate:sameXTransTrip) -> melt1 
samettpSenateEsts %>% 
  select(se_density:maxConv, sim) %>% 
  gather(parameter, se, se_density:se_sameXTransTrip) %>% 
  mutate(parameter = str_replace(parameter, "se_", "")) -> melt2 
left_join(melt1, melt2) %>% arrange(sim) -> samettpSenLong

# get means 
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
# data exploration 
names(M1SenEstsLong)
names(jttpSenLong)
names(jttsSenLong)
names(samepSenLong)  
names(simttbSenLong)
names(samettpSenLong)
M1SenEstsLong <- M1SenEstsLong %>% add_column(model = "basic") 
jttpSenLong <- jttpSenLong %>% add_column(model = "jttp") 
jttsSenLong <- jttsSenLong %>% add_column(model = "jtts") 
samepSenLong <- samepSenLong %>% add_column(model = "samep") 
simttbSenLong <- simttbSenLong %>% add_column(model = "simttb")
samettpSenLong <- samettpSenLong %>% add_column(model = "samettp")
allModelsLong <- do.call(rbind, list(M1SenEstsLong, jttpSenLong, jttsSenLong,
                                     samepSenLong, simttbSenLong, samettpSenLong))

ggplot(data = allModelsLong) + 
  geom_density(aes(x = estimate, fill = model), alpha = .45) + 
  facet_wrap(~parameter, scales = 'free', nrow = 3) + 
  theme(legend.position = 'bottom')
# ggplot(data = allModelsLong) + 
#   geom_density(aes(x = estimate, fill =  parameter), alpha = .45) + 
#   facet_wrap(~model, scales = 'free')
# non-rate parameter ests. converge only
ggplot(data = allModelsLong %>% filter(!str_detect(parameter, "Rate") & maxConv <=0.25)) + 
  geom_point(aes(x = se, y = estimate, color = parameter)) + 
  facet_wrap(~model, scales = 'free')



