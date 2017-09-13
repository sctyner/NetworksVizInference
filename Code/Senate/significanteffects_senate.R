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
