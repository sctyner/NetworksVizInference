load("Data/lus_ests_truth.rda")

lus_ests_truth$true_panel <- lus_ests_truth$true_model == lus_ests_truth$model
lus_spread <- tidyr::spread(lus_ests_truth[,c("lineupid","model", "true_model", "panel_num", "param_name", "param_est")], model, param_est)

# parameters should be most different between data plot and null plots

# merge in true values:
truth <- unique(subset(lus_ests_truth, true_panel==TRUE)[,c("lineupid", "true_model","param_name","true_value")])
dim(lus_spread)
lus_spread <- merge(lus_spread, truth, by=c("lineupid","true_model", "param_name"), all.x=TRUE)
lus_spread$true_value[is.na(lus_spread$true_value)] <- 0
qplot(M2, true_value, facets=~param_name, data=lus_spread, colour=true_model)
qplot(M3, true_value, facets=~param_name, data=lus_spread, colour=true_model)

# compute a distance between the data panel and the closest null panel
lus_spread$part <- gsub("(.*)-[0-9]*-[0-9]*", '\\1', lus_spread$lineupid)

lus_summary_2 <- lus_spread %>% group_by(part, lineupid, param_name) %>%
  summarize(
    data_n = sum(true_model=="M2"),
    data_est = mean(M2[true_model=="M2"]),
    data_sd = sd(M2[true_model=="M2"], na.rm=TRUE),
    null_est = list(M2[true_model=="M1"]),
    data_dist = data_est - max(unlist(null_est)),
    upper_qu = sum(data_est > unlist(null_est))/(n()-1)
  )
lus_summary_2$m <- gsub(".*-([0-9]*)-.*","\\1",lus_summary$lineupid)

qplot(lineupid, upper_qu, data = subset(lus_summary_2, !is.na(upper_qu) & param_name=="transitive triplets jumping alcohol2")) +
  facet_grid(.~part, scale="free_x", space="free")

qplot(upper_qu, geom="histogram", data = subset(lus_summary_2, !is.na(upper_qu) & param_name=="transitive triplets jumping alcohol2")) +
  facet_grid(.~part)

qplot(data_dist, upper_qu, colour=m,  data = subset(lus_summary_2, !is.na(upper_qu) & param_name=="transitive triplets jumping alcohol2")) +
  facet_grid(m~part)


lus_summary_3 <- lus_spread %>% group_by(part, m, lineupid, param_name) %>%
  summarize(
    data_n = sum(true_model=="M3"),
    data_est = mean(M3[true_model=="M3"]),
    data_sd = sd(M3[true_model=="M3"], na.rm=TRUE),
    null_est = list(M3[true_model=="M1"]),
    data_dist = min(unlist(null_est)) - data_est,
    lower_qu = sum(data_est < unlist(null_est))/(n()-1)
  )

qplot(lineupid, lower_qu, data = subset(lus_summary_3, !is.na(lower_qu) & param_name=="number pairs at doubly achieved distance 2")) +
  facet_grid(.~part, scale="free_x", space="free")

qplot(data_dist, lower_qu,  data = subset(lus_summary_3, !is.na(lower_qu) & param_name=="number pairs at doubly achieved distance 2")) +
  facet_grid(.~part)
