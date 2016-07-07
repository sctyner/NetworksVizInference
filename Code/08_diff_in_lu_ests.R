# computing differences between the model estimates in m1 v m2 situations

load("Data/lus_ests_truth.rda")
head(lus_ests_truth)
m1m2 <- unique(lus_ests_truth$lineupname)[1:2]
onlym1m2 <- lus_ests_truth %>% filter(lineupname %in% m1m2) %>% filter(model != "M3")

onlym1m2wide <- spread(onlym1m2[,c("lineupid","model", "true_model", "panel_num", "param_name", "param_est", "convergence")], model, param_est)
onlym1m2wide %>% mutate(diff = M2 - ifelse(is.na(M1), 0, M1)) -> onlym1m2wide

ggplot(data = filter(onlym1m2wide, param_name == "transitive triplets jumping alcohol2")) + 
  geom_density(aes(x = diff, fill = true_model), alpha = .7) + 
  scale_fill_brewer(palette = "Set3") + 
  facet_wrap(~convergence, scales = 'free') 

# okay. jtt panel shows that when m2 is true model there is a mode around 3 for jtt coef but when true mod
# is m1 mode is at zero for jtt. woot. 
