# make lineups
library(tidyverse)
library(netvizinf)
library(geomnet)
load("Data/senate/FauxTrueData/M_1_sims_for_fdata.RDS")
basicsims <- read_csv("Data/senate/FauxTrueData/M1senSimDat.csv")

head(basicsims)
basicsims %>% filter(wave == 1, sim == 1) %>% mutate(sim = 1001)

gotsims %>% filter(model == "basic") %>%
  mutate(simsdf = map(sims, sims_to_df)) -> testsims
(testsims %>% select(simsdf))[[1]][[1]] %>% filter(wave == 1, sim %in% 1:5) -> for_lu

bind_rows(for_lu, basicsims %>% filter(wave == 1, sim == 1) %>% mutate(sim = 1001)) %>% 
  mutate(from = paste0("V", from), to = ifelse(is.na(to), to, paste0("V", to))) -> for_lu
for_lu$ord <- rep(sample(6), as.numeric(table(for_lu$sim)))
  
ggplot(data = for_lu) + 
  geom_net(aes(from_id = from, to_id = to), fiteach = T, directed = T, color = 'black', arrowgap = .01, arrowsize = .3, size = 2) + 
  theme_net() + 
  facet_wrap(~ord)
