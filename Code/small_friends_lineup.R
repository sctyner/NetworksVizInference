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
  return(list(data = to_plot, lineup = plot, test_id = data_plot_id))
}

lu1 <- create_smfriend_lu(null_eff_struct = null_model_eff2, test_eff_struct = eff_models_smallFriends[[39]], M = 9)
lu1$data
lu1$lineup
lu1$test_id

eff_models_smallFriends[[39]]
# the added significant effect above is of type "eval" and defined as the transitive triplets jumping on the covariate.
# s_{i77}(x) = \sum_{j neq h} x_{ij}x_{ih}x_{hj} * I(v_h = v_i \neq v_j)
# this refers to transitive closure, restricted to “jump outside of
# V -groups” in the sense that the focal actor and the mediating
# actor have the same value of V , but the target actor has a
# different value;

# Here are the results from the model with these added
runs_models_smallFriends[[39]]
test_results[175,]

# Make interactive
# interactive lineups with gridSVG
library(grid)
library(gridSVG)
scriptURL = "http://www.hofroe.net/examples/lineup/action-back.js"

add_data <- function(filename="lineup-details.csv", sample_size, test_param, param_value, p_value, obs_plot_location, pic_name, experiment="turk21", difficulty, data_name,
                     question="Which graph looks the most different?") {
  write.table(data.frame(
    sample_size=sample_size,
    test_param=test_param,
    param_value=param_value,
    p_value=p_value,
    obs_plot_location=obs_plot_location, 
    pic_name=pic_name,
    experiment=experiment,
    difficulty=difficulty,
    data_name=data_name,
    question=question
  ), 
  file=filename, row.names=FALSE, sep=",",
  col.names=!file.exists(filename), append=TRUE)
}

make_interactive <- function(filename, script, toggle="toggle", high = "#c5c5c5", background="#ffffff") {
  require(gridSVG)
  grid.force()  
  grobs <- grid.ls()
  
  idx <- grep("panel-", grobs$name)
  for (i in idx) { 
    grid.garnish(grobs$name[i],
                 onmouseover=paste("frame('",grobs$name[i+2], ".1.1')", sep=""),
                 onmouseout=paste("deframe('",grobs$name[i+2], ".1.1')", sep=""), 
                 onmousedown=sprintf("%shigh(evt, '%s.1.1', '%s', '%s')", toggle, grobs$name[i+2], high, background)
    )
  }
  
  # use script on server to get locally executable javascript code
  # or use inline option (default)
  grid.script(filename=script)
  grid.export(filename)
}

head(lu1$data)
write.csv(lu1$data, "Data/lineupdata/smallfriends-m-9-rep-1.csv", row.names=FALSE)
file <- 1
#dir.create(path = "lineups")
#dir.create(path = "lineups/pdfs")
ggsave(lu1$lineup, file=sprintf("lineups/pdfs/smallfriends-m-9-rep-%s.pdf",file), width = 7.2, height = 4.5, units = "in")
#dir.create(path = "lineups/svgs")
tmpfile <- sprintf("%s.svg",tempfile(tmpdir="lineups/svgs"))
print(lu1$lineup)
# isn't working. keeps throwing the same 2 errors
make_interactive(filename= tmpfile, script=scriptURL,  
                 high="#d5d5d5",  background="#ffffff")


