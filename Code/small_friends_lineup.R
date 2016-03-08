# function to create full lineups 
# notes : this is for the small friends data, wave 2 (aka period 1).
# many aspects of this function are catered specifically to that data and 
# the function will break if used on other models

library(RSiena)
# run line to get the effects structures and data. 
# source("Code/find_sig_eff_small_friends.R")

create_smfriend_lu <- function(null_eff_struct, test_eff_struct, M, my_dat=mysmalldata, ...){
  null_data <- NULL
  n <- M-1
  for (i in 1:n){
    null_net <- siena07( myalgorithm2, data = my_dat, returnDeps = TRUE, effects = null_eff_struct, batch=TRUE, verbose = FALSE)
    net_df <- merge(data.frame(null_net$sims[[1000]][[1]][[1]][[1]])[,-3], 
                     data.frame(id = 1:16), by.x = "X1", by.y = "id",
                     all = T)
    net_df$count <- i 
    null_data <- rbind(null_data, net_df)
  }
  test_net <- siena07( myalgorithm2, data = my_dat, returnDeps = TRUE, effects = test_eff_struct, batch=TRUE, verbose = FALSE)
  test_data <- merge(data.frame(test_net$sims[[1000]][[1]][[1]][[1]])[,-3], 
                     data.frame(id = 1:16), by.x = "X1", by.y = "id",
                     all = T)
  test_data$count <- M  # test data is always indexed by M
  to_plot <- rbind(null_data, test_data)
  to_plot$plot_order <- rep(sample(M), as.vector(table(to_plot$count)))
  plot <- ggplot(data = to_plot, aes(from_id = X1, to_id = X2)) + 
            geom_net(fiteach = TRUE, directed = T, size = 1, arrowsize = .5) +
            facet_wrap(~plot_order) + theme_net()
  data_plot_id <- unique(to_plot$plot_order[which(to_plot$count == M)])
  return(list(lineup = plot, test_id = data_plot_id))
}

lu1 <- create_smfriend_lu(null_eff_struct = null_model_eff2, test_eff_struct = eff_models_smallFriends[[39]], M = 9)
lu1$lineup
lu1$test_id
