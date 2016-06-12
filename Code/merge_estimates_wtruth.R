# analysis of results. 

lus_res_long <- read.csv("Data/June_estimates_from_lineups.csv", stringsAsFactors = FALSE)
library(ggplot2)
ggplot(data = lus_res_long, aes(x = param_est)) + 
  geom_density(aes(fill = model), alpha = .7) + 
  facet_wrap(~param_name, scales='free')

ggplot(data = lus_res_long, aes(x = param_est_se)) + 
  geom_density(aes(fill = model), alpha = .7) +
  xlim(c(0,10)) + 
  facet_wrap(~param_name, scales='free')

ggplot(data = lus_res_long, aes(x = converg_stat)) + 
  geom_density(aes(fill = model), alpha = .7) +
  facet_wrap(~param_name, scales='free')

# there are some NAs

summary(lus_res_long$param_est)
summary(lus_res_long$param_est_se)
summary(lus_res_long$converg_stat)
summary(lus_res_long$time)
table(lus_res_long$termination, useNA ="ifany")

# overall convergence measured by the rate convergence valu
ggplot() + 
  geom_density(data = lus_res_long %>% filter(param_name == 'rate'), aes(x = converg_stat)) +
  geom_vline(xintercept = c(.2, .3), color = c('green', 'blue')) + 
  geom_text(data = NULL, inherit.aes = FALSE, aes(x = 2, y = 1.5, label = "Excellent convergence left of green line,\nreasonable convergence left of blue line"))

ggplot() + 
  geom_density(data = lus_res_long %>% filter(param_name != 'rate'), aes(x = converg_stat)) + 
  geom_vline(xintercept = c(-.1, .1), color = 'red') +
  facet_wrap(~param_name) +
  labs(title = "between red lines is good convergence")

# interesting. explore jtt more

ggplot() + 
  geom_density(data = lus_res_long %>% filter(param_name == 'transitive triplets jumping alcohol2'), aes(x = converg_stat, fill = lineupname)) + 
  geom_vline(xintercept = c(-.1, .1), color = 'red') + 
  facet_wrap(~lineupname)

# whoa. very cool  


#### match the lineup ids and plots to the true model they came from

get_truth <- function(dat){
  lineupname <- dat$lineupname
  M <- dat$M
  rep <- dat$rep
  filename <- paste0("Data/lineupdata/",lineupname,"-m-",M, "-rep-",rep,".csv")
  dat2 <- read.csv(filename, stringsAsFactors = F)
  altplotid <- dat2[which(dat2$count == M)[1],'plot_order']
  
  if (lineupname == 'smallfriends'){
      res <- data.frame(lineupid = paste0(lineupname,'-',M,'-',rep), 
                 panel = 1:M, 
                 true_model = ifelse(1:M == altplotid, "M2", "M1"),
                 stringsAsFactors = F)
      
  } else if (lineupname == 'smallfriends-rev'){
    res <- data.frame(lineupid = paste0(lineupname,'-',M,'-',rep), 
                      panel = 1:M, 
                      true_model = ifelse(1:M == altplotid, "M1", "M2"),
                      stringsAsFactors = F)
  } else if (lineupname == 'smallfriends-eff2'){
    res <- data.frame(lineupid = paste0(lineupname,'-',M,'-',rep), 
                      panel = 1:M, 
                      true_model = ifelse(1:M == altplotid, "M3", "M1"),
                      stringsAsFactors = F)
  } else {
    res <- data.frame(lineupid = paste0(lineupname,'-',M,'-',rep), 
                      panel = 1:M, 
                      true_model = ifelse(1:M == altplotid, "M1", "M3"),
                      stringsAsFactors = F)
  }
  return(res)
}

library(tidyr)
library(dplyr)
library(purrr)
library(magrittr)

lineups$lineupname %<>% as.character()
lineups %>% nest(-lineupid) %>% mutate(matching = map(data, get_truth)) -> matching_lineups 

head(matching_lineups)

matching_lineups %>% unnest(data) %>% unnest(matching) -> match_lu_long
match_lu_long <- match_lu_long[,-5]


effects_dist <- read.csv("Data/simulation-1000-M1-M2-M3.csv")
effects_dist %>% group_by(Model, parameter) %>% 
  dplyr::summarise(bhat = mean(estimate)) %>%
  filter(parameter !="alpha2") -> starting_values

true_params <- data.frame(model = rep(c("M1", "M2", "M3"), c(3,4,4)),
                          param_name = c("rate","outdegree (density)","reciprocity", 
                                         "rate","outdegree (density)","reciprocity", "transitive triplets jumping alcohol2",
                                         "rate","outdegree (density)","reciprocity", "number pairs at doubly achieved distance 2"), 
                          true_value = starting_values[[3]])

panels_and_truth <- merge(match_lu_long, true_params)

lus_ests_truth <- merge(lus_res_long, panels_and_truth)
head(lus_ests_truth)

lus_ests_truth %<>% arrange(lineupname, M, rep, model, panel_num, param_name)
