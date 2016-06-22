source("Code/complete_lineup_creation.R")
# function to reverse the models (from M-1 X (null) & 1 Y (alt) to  1 X & M-1 alt)
create_smfriend_lu_rev <- function(null_eff_struct, test_eff_struct, M, my_dat=mysmalldata, ...){
  # browser()
  alt_data <- NULL
  n <- M-1
  alt_net <- siena07( myalgorithm2, data = my_dat, returnDeps = TRUE, effects = test_eff_struct, batch=TRUE, verbose = FALSE)
  test_eff_struct2 <- test_eff_struct
  test_eff_struct2$initialValue[test_eff_struct2$include] <- c(alt_net$rate,alt_net$theta)
  sim_algorithm <- sienaAlgorithmCreate( projname = "sim_model", cond = FALSE,
                                         useStdInits = FALSE, nsub = 0 , simOnly = TRUE)
  sim_ans    <-  siena07( sim_algorithm, data = my_dat, effects = test_eff_struct2, returnDeps = TRUE)
  for (i in 1:n){
    net_df <- merge(data.frame(sim_ans$sims[[1000-(n-i)]][[1]][[1]][[1]])[,-3], 
                    data.frame(id = 1:16), by.x = "X1", by.y = "id",
                    all = T)
    for (j in 1:nrow(net_df)){
      if (!(net_df$X1[j] %in% net_df$X2) & is.na(net_df$X2[j])){
        net_df$X2[j] <- net_df$X1[j]
      } else {net_df$X2[j] <- net_df$X2[j]}
    }
    net_df$count <- i 
    alt_data <- rbind(alt_data, net_df)
  }
  null_net <- siena07( myalgorithm2, data = my_dat, returnDeps = TRUE, effects =null_eff_struct, batch=TRUE, verbose = FALSE)
  null_data <- merge(data.frame(null_net$sims[[1000]][[1]][[1]][[1]])[,-3], 
                     data.frame(id = 1:16), by.x = "X1", by.y = "id",
                     all = T)
  for (j in 1:nrow(null_data)){
    if (!(null_data$X1[j] %in% null_data$X2) & is.na(null_data$X2[j])){
      null_data$X2[j] <- null_data$X1[j]
    } else {null_data$X2[j] <- null_data$X2[j]}
  }
  null_data$count <- M  # test data is always indexed by M
  to_plot <- rbind(alt_data, null_data)
  to_plot$plot_order <- rep(sample(M), as.vector(table(to_plot$count)))
  to_plot$X1 <- as.factor(to_plot$X1)
  to_plot$X2 <- as.factor(to_plot$X2)
  plot <- ggplot(data = to_plot, aes(from_id = X1, to_id = X2)) + 
    geom_net(fiteach = TRUE, directed = F, size = 1, arrowsize = .5) +
    facet_wrap(~plot_order) + theme_net() + theme(panel.background = element_rect(fill = "white", color = 'black'))
  data_plot_id <- unique(to_plot$plot_order[which(to_plot$count == M)])
  return(list(data = to_plot, lineup = plot, test_id = data_plot_id, null_mod_ests = c(null_net$rate,null_net$theta), alt_mod_estimates = c(alt_net$rate,alt_net$theta)))
}

make_interactive_lineups_rev <- function(M, reps, model.effects, null.effects){
  for (r in 1:reps){
    lineup <- create_smfriend_lu_rev(null_eff_struct = null.effects, test_eff_struct = model.effects, M = M)
    filename <- paste0("Data/lineupdata/smallfriendsrev-m-", M, '-rep-', r, ".csv")
    write.csv(lineup$data, sprintf("Data/lineupdata/smallfriendsrev-m-%s-rep-%s.csv",M,r), row.names=FALSE)
    #ggsave(lineup$lineup, file=sprintf("Lineup-Images/pdfs/smallfriends-m-%s-rep-%s.pdf",M,r), width = 7.2, height = 4.5, units = "in")
    savepdf(file = sprintf("Lineup-Images/pdfs/smallfriendsrev-m-%s-rep-%s",M,r), width = 7.2*2.54, height = 4.5*2.54)
    print(lineup$lineup)
    dev.off()
    tmpfile <- sprintf("%s.svg",tempfile(tmpdir="Lineup-Images/svgs"))
    make_interactive(filename2 = tmpfile, script=scriptURL,  
                     high="#d5d5d5",  background="#ffffff")
  }
  return(list(lineup = lineup))
}

#make_interactive_lineups_rev(M = 3, reps=20, model.effects = null_model_eff2, null.effects = eff_models_smallFriends[[39]])

#make_interactive_lineups_rev(M = 6, reps=20, model.effects = null_model_eff2, null.effects = eff_models_smallFriends[[39]])

#make_interactive_lineups_rev(M = 9, reps=20, model.effects = null_model_eff2, null.effects = eff_models_smallFriends[[39]])

#make_interactive_lineups_rev(M = 12, reps=20, model.effects = null_model_eff2, null.effects = eff_models_smallFriends[[39]])

make_interactive_lineups_rev(M = 16, reps=20, model.effects = null_model_eff2, null.effects = eff_models_smallFriends[[39]])
