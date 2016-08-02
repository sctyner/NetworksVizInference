# "easy" lineups
load("Data/lus_ests_truth.rda")

lus_ests_truth$true_panel <- lus_ests_truth$true_model == lus_ests_truth$model
lus_spread <- tidyr::spread(lus_ests_truth[,c("lineupid","model", "true_model", "panel_num", "param_name", "param_est")], model, param_est)

# parameters should be most different between data plot and null plots

# merge in true values:
truth <- unique(subset(lus_ests_truth, true_panel==TRUE)[,c("lineupid", "true_model","param_name","true_value")])

lus_spread <- merge(lus_spread, truth, by=c("lineupid","true_model", "param_name"), all.x=TRUE)
lus_spread$true_value[is.na(lus_spread$true_value)] <- 0

# compute a distance between the data panel and the closest null panel
lus_spread$part <- gsub("(.*)-[0-9]*-[0-9]*", '\\1', lus_spread$lineupid)

lus_summary_2 <- lus_spread %>% dplyr::group_by(part, lineupid, param_name) %>%
  dplyr::summarize(
    data_n = sum(true_model=="M2"),
    data_est = mean(M2[true_model=="M2"]),
    data_sd = sd(M2[true_model=="M2"], na.rm=TRUE),
    data_dist = data_est - max(M2[true_model=="M1"]),
    upper_qu = sum(data_est > M2[true_model=="M1"]) / (n()-1) 
  )

# smallfriends rev 

data.frame(lus_summary_2 %>% 
  filter(part == "smallfriends-rev", param_name == "transitive triplets jumping alcohol2", upper_qu == 0) %>% 
  dplyr::select(part, lineupid, param_name, data_dist, upper_qu))
data.frame(lus_summary_2 %>% 
             filter(part == "smallfriends-rev", param_name == "transitive triplets jumping alcohol2", upper_qu > 0, upper_qu<.25, data_dist < 1) %>% 
             dplyr::select(part, lineupid, param_name, data_dist, upper_qu))

data.frame(lus_summary_2 %>% 
             filter(part == "smallfriends-eff2", param_name == "number pairs at doubly achieved distance 2", upper_qu == 1) %>% 
             dplyr::select(part, lineupid, param_name, data_dist, upper_qu))
data.frame(lus_summary_2 %>% 
             filter(part == "smallfriends-eff2", param_name == "transitive triplets jumping alcohol2", upper_qu > 0, upper_qu<.25, data_dist < 1) %>% 
             dplyr::select(part, lineupid, param_name, data_dist, upper_qu))


p1 <- qplot(data_dist, upper_qu, data = subset(lus_summary_2, !is.na(upper_qu) & param_name=="transitive triplets jumping alcohol2"), color = upper_qu) +
  facet_grid(.~part) + xlab("Difference in M2 estimate of data panel and maximum of the null panels") +
  ylab("Ratio of null panels with smaller M2 estimate") +
  ggtitle("jumping transitive triplets")


lus_summary_2b <- lus_spread %>% dplyr::group_by(part, lineupid, param_name) %>%
  dplyr::summarize(
    data_n = sum(true_model=="M1"),
    data_est = mean(M2[true_model=="M1"]),
    data_sd = sd(M2[true_model=="M1"], na.rm=TRUE),
    data_dist = data_est - min(M2[true_model=="M2"]),
    lower_qu = sum(data_est < M2[true_model=="M2"]) / (n()-1) 
  )


# which are 1 ? should be easy! 
easy_ids <- filter(lus_summary_2, !is.na(upper_qu) & 
                   param_name=="transitive triplets jumping alcohol2" &  
                   upper_qu > .75)
easy_ids

easy_id_1 <- easy_ids[which(easy_ids$upper_qu == 1),]
easy_id_75 <- easy_ids[which(easy_ids$upper_qu > .75 & easy_ids$upper_qu < 1),]

easy_lineups <- plyr::ldply(strsplit(lus_summary_2$lineupid[easy_id], "-"), rbind)
easy_lineups2 <- plyr::ldply(strsplit(lus_summary_2$lineupid[easy_id2], "-"), rbind)
lus_summary_2[easy_id2,]
names(easy_lineups) <- c('lineupname', 'M', 'rep')
easy_lineups[easy_lineups$M == 12,"rep"]
names(easy_lineups2) <- c('lineupname', 'M', 'rep')
easy_lineups2[easy_lineups2$M == 12,"rep"]

easy_lineups
view_lu <- function(dat){
  lineupname <- dat$lineupname
  M <- dat$M
  rep <- dat$rep
  
  filename <- paste0("Data/lineupdata/",lineupname, "-m-", M, "-rep-", rep, ".csv")
  lu <- read.csv(filename)
  
  p <- ggplot(data = lu) + geom_net(aes(from_id = X1, to_id = X2), size = 2) + facet_wrap(~plot_order) + 
    ggtitle(filename) + theme_net()
  return(list(plot = p, panel = unique(lu$plot_order[which(lu$count == M)])))
}

# i tried a couple times, and only got 1/3 correct. :(