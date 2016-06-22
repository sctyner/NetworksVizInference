multiple_runs <- function(effects, num_plots){
  n <- num_plots
  data_for_plot <- NULL
  for (i in 1:n){
    net <- siena07( myalgorithm2, data = mysmalldata, returnDeps = TRUE, effects = eff_models_smallFriends[[i]], batch=TRUE, verbose = FALSE)
    sim1000 <- merge(data.frame(net$sims[[1000]][[1]][[1]][[1]])[,-3], 
                     data.frame(id = 1:16), by.x = "X1", by.y = "id",
                     all = T)
    sim1000$count <- i 
    data_for_plot <- rbind(data_for_plot, sim1000)
  }
  actual2 <- merge(data.frame(as.edgelist(as.network(fd2.w2))), 
                   data.frame(id = 1:16), 
                   by.x = "X1", by.y = "id", all = T)
  actual2$count <- "true_wave2"
  data_for_plot <-rbind(data_for_plot,actual2)
  return(data_for_plot)
}  

homXTransTripSims <- multiple_runs(effects = eff_models_smallFriends[[38]], num_plots = 8)
nullmodelSims <- multiple_runs(effects = null_model_eff2, num_plots = 8) 


ggplot(data = homXTransTripSims, aes(from_id = X1, to_id = X2)) +
  geom_net(fiteach = TRUE, directed=TRUE, label = TRUE, labelcolour = 'red', size = 1, arrowsize = .5) + theme_net() +
  facet_wrap(~count)

ggplot(data = nullmodelSims, aes(from_id = X1, to_id = X2)) +
  geom_net(fiteach = TRUE, directed=TRUE, label = TRUE, labelcolour = 'red', size = 1, arrowsize = .5) + theme_net() +
  facet_wrap(~count)