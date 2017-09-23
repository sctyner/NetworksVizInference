# looking at sig effects Senate no hrc
library(tidyverse)
library(lubridate)
library(geomnet)
library(RSienaTest)
library(RSiena)
library(rvest)
library(netvizinf)

test_res <- read_csv("Data/senate/sigEffsSenate912.csv")

test_res <- test_res[-which(test_res$estimate == 0),]

test_res %>% arrange(Waldpval)

# pvalues vs convergence. want params that converged relatively quickly and that have very small p-values
ggplot(data = test_res, aes(x = log(Waldpval), y = overallConverge)) + 
  geom_line() + 
  geom_point(aes(color = as.factor(refits))) + 
  scale_color_brewer(palette = "YlOrRd") + 
  geom_hline(yintercept = .25, color = 'red') + 
  geom_vline(xintercept = c(log(.01),log(.05), log(.10)), color = c('blue', "green", "magenta"))

test_res %>% filter(Waldpval < 0.1 & overallConverge <= .25) %>% 
  arrange(Waldpval) %>% 
  mutate(overallConverge = round(overallConverge,4),
         Waldpval = ifelse(Waldpval < 0.001, "<0.001", round(Waldpval, 4))) -> sigEffsConverged 
# write_csv(sigEffsConverged, "Data/senate/sigEffsSenateConverged.csv")

ggplot(data = sigEffsConverged, aes(x = readr::parse_number(Waldpval), y = overallConverge)) + 
  geom_line() + 
  geom_point(aes(color = as.factor(refits))) + 
  scale_color_brewer(palette = "YlOrRd") + 
  geom_hline(yintercept = .25, color = 'red') + 
  geom_vline(xintercept = c(.01,.05, .10), color = c('blue', "green", "magenta"))




