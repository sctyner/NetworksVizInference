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

lineups <- expand.grid(lineupname = c("smallfriends", "smallfriends-rev", "smallfriends-eff2", "smallfriends-eff2-rev" ),
                       M = c(3,6,9,12,16), 
                       rep = 1:20)

lineups$lineupid <- paste(lineups$lineupname, lineups$M, lineups$rep,sep = '-')
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
names(true_params)[1] <- "true_model"
true_params <- rbind(true_params, data.frame(true_model = c("M1", "M1", "M2", "M3"), 
                                             param_name = c("transitive triplets jumping alcohol2", "number pairs at doubly achieved distance 2", "number pairs at doubly achieved distance 2", "transitive triplets jumping alcohol2"), 
                                             true_value = rep(0,4)))
panels_and_truth <- merge(match_lu_long, true_params)
names(panels_and_truth)[6] <- "panel_num"
names(lus_res_long)
lus_ests_truth <- merge(lus_res_long, panels_and_truth, by = intersect(names(lus_res_long), names(panels_and_truth)))
head(lus_ests_truth)
lus_ests_truth <-lus_ests_truth[, -which(names(lus_ests_truth) == "X")]
lus_ests_truth %<>% arrange(lineupname, M, rep, model, panel_num, param_name)
write.csv(lus_ests_truth, file="Data/lus_ests_truth.csv", row.names=FALSE)

library(ggplot2)
ggplot(data = lus_ests_truth) + geom_density(alpha = .6, aes(x = param_est, fill = true_model)) + 
  facet_grid(param_name~model, scales = 'free')

#previous pic better 
ggplot(data = lus_ests_truth) + geom_density(alpha = .8, aes(x = param_est, fill = param_name)) + 
  facet_grid(model~true_model, scales = 'free', labeller = "label_both") + xlim(c(-25,25))

# params in all models 
  # "outdegree (density)" "rate" "reciprocity"   

allmodels <- dplyr::filter(lus_ests_truth, param_name %in% c("outdegree (density)","rate","reciprocity"))

ggplot(data = allmodels) + 
  geom_density(alpha = .6, aes(x = param_est, fill = true_model)) + xlim(c(-25,25)) + 
  facet_grid(param_name~model, scales = 'free')

# params in only 2, 3 : "transitive triplets jumping alcohol2" "number pairs at doubly achieved distance 2"

others <- dplyr::filter(lus_ests_truth, param_name %in% c( "transitive triplets jumping alcohol2", "number pairs at doubly achieved distance 2"))

ggplot(data = dplyr::filter(others, model == "M2")) + 
  geom_density(alpha = .6, aes(x = param_est, fill = true_model))

ggplot(data = dplyr::filter(others, model == "M3")) + 
  geom_density(alpha = .6, aes(x = param_est, fill = true_model))

# create column for convergence.

# good convergence for params c(-.1,.1)
# good convergence overall <.3, great convergence <.2

names(lus_ests_truth2)
lus_ests_truth <- lus_ests_truth2
lus_ests_truth$convergence <- NA

for (i in 1:nrow(lus_ests_truth)){
  if (is.na(lus_ests_truth$converg_stat[i])){
    lus_ests_truth$convergence[i] <- NA
  } else if (lus_ests_truth$param_name[i] != "rate"){
    if (lus_ests_truth$converg_stat[i] <= 0.1 & lus_ests_truth$converg_stat[i] >= -.1){
      lus_ests_truth$convergence[i] <- "Converged"
    } else{  lus_ests_truth$convergence[i] <- "Did not converge"} 
  } else {
    if (lus_ests_truth$converg_stat[i] <= 0.3){
      lus_ests_truth$convergence[i] <- "Converged"
    } else { lus_ests_truth$convergence[i] <- "Did not converge" }
  }
}

save(lus_ests_truth, file = "~/Desktop/NetworksResearch/NetworksVizInference/Data/lus_ests_truth.RDA")
table(lus_ests_truth$convergence, useNA = 'ifany')

ggplot(data = lus_ests_truth) + geom_density(alpha = .8, aes(x = param_est, fill = true_model)) + 
  facet_grid(convergence~param_name, scales = 'free', labeller = "label_both") 

# create zeros for the parameters that don't exist. 
head(data.frame(lus_ests_truth %>% spread(model, param_est)), 100)

head(data.frame(lus_ests_truth %>% filter(model == "M1" & true_model == "M2")))
