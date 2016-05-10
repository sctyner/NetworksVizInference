# analyzing results of mini-experiment 

#setwd("GGExperimentApr28")

library(readxl)
library(dplyr)
library(magrittr)
response <- read_excel("responses_GGExpApr28.xlsx")
response %<>% mutate(correct = as.numeric(Answer == ChosenLU))
response %>% group_by(Respondent) %>% 
  dplyr::summarise(perc_right = sum(na.omit(correct))/20)

library(ggplot2)

print(ggplot(data = response, aes(x = as.factor(ChosenLU), fill = as.factor(correct))) + 
  geom_bar() +
  facet_wrap(~Lineup, scales = 'free'))

print(response %>% group_by(Lineup) %>%
  dplyr::summarise(perc_guessed = sum(na.omit(correct))/11, tot_resp = length(unique(ChosenLU)))
)
