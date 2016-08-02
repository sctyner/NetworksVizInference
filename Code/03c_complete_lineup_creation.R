library(RSiena)
library(geomnet)
library(grid)
library(gridSVG)
##### LOAD IN DATA #####
#looks like 20:35 are pretty well connected, go with those for now
#setwd("Desktop/NetworksResearch/NetworksVizInference/")
friend.data.w1 <- as.matrix(read.table("Data/s50_data/s50-network1.dat"))
friend.data.w2 <- as.matrix(read.table("Data/s50_data/s50-network2.dat"))
friend.data.w3 <- as.matrix(read.table("Data/s50_data/s50-network3.dat"))
fd2.w1 <- friend.data.w1[20:35,20:35]
fd2.w2 <- friend.data.w2[20:35,20:35]
fd2.w3 <- friend.data.w3[20:35,20:35]
# read in covariate data
drink <- as.matrix(read.table("Data/s50_data/s50-alcohol.dat"))
drink2 <- drink[20:35,]
# siena data
friendData2 <- array( c( fd2.w1, fd2.w2, fd2.w3 ),
                      dim = c( 16, 16, 3 ) )
# and next give this the role of the dependent variable:
friend2 <- sienaDependent(friendData2)
alcohol2 <- varCovar( as.matrix(drink2) )
# create siena data object
mysmalldata <- sienaDataCreate( friend2, alcohol2)
null_model_eff2 <- getEffects(mysmalldata)
myalgorithm2 <- sienaAlgorithmCreate( projname = 's50' , n3 = 1000)
sig_eff_names <- read.csv("Data/sig_eff_names_smfriends.csv")
runs_models_smallFriends <- readRDS("Data/runs_models_smallFriends.RDS")
eff_models_smallFriends <- readRDS("Data/eff_models_smallFriends.RDS")
# functions to create lineups 
create_smfriend_lu <- function(null_eff_struct, test_eff_struct, M, my_dat=mysmalldata, ...){
 # browser()
  null_data <- NULL
  n <- M-1
  null_net <- siena07( myalgorithm2, data = my_dat, returnDeps = TRUE, effects = null_eff_struct, batch=TRUE, verbose = FALSE)
  null_eff_struct2 <- null_eff_struct
  null_eff_struct2$initialValue[null_eff_struct2$include] <- c(null_net$rate,null_net$theta)
  sim_algorithm <- sienaAlgorithmCreate( projname = "sim_model", cond = FALSE,
                                useStdInits = FALSE, nsub = 0 , simOnly = TRUE)
  sim_ans    <-  siena07( sim_algorithm, data = my_dat, effects = null_eff_struct2, returnDeps = TRUE)
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
    null_data <- rbind(null_data, net_df)
  }
  test_net <- siena07( myalgorithm2, data = my_dat, returnDeps = TRUE, effects = test_eff_struct, batch=TRUE, verbose = FALSE)
  test_data <- merge(data.frame(test_net$sims[[1000]][[1]][[1]][[1]])[,-3], 
                     data.frame(id = 1:16), by.x = "X1", by.y = "id",
                     all = T)
  for (j in 1:nrow(test_data)){
    if (!(test_data$X1[j] %in% test_data$X2) & is.na(test_data$X2[j])){
      test_data$X2[j] <- test_data$X1[j]
    } else {test_data$X2[j] <- test_data$X2[j]}
  }
  test_data$count <- M  # test data is always indexed by M
  to_plot <- rbind(null_data, test_data)
  to_plot$plot_order <- rep(sample(M), as.vector(table(to_plot$count)))
  to_plot$X1 <- as.factor(to_plot$X1)
  to_plot$X2 <- as.factor(to_plot$X2)
  plot <- ggplot(data = to_plot, aes(from_id = X1, to_id = X2)) + 
    geom_net(fiteach = TRUE, directed = F, size = 1, arrowsize = .5) +
    facet_wrap(~plot_order) + theme_net() + theme(panel.background = element_rect(fill = "white", color = 'black'))
  data_plot_id <- unique(to_plot$plot_order[which(to_plot$count == M)])
  return(list(data = to_plot, lineup = plot, test_id = data_plot_id, null_mod_ests = c(null_net$rate,null_net$theta), alt_mod_estimates = c(test_net$rate,test_net$theta)))
}


# lu1 <- create_smfriend_lu(null_eff_struct = null_model_eff2, test_eff_struct = eff_models_smallFriends[[39]], M = 6)
# savepdf(file = "Lineup-Images/pdfs/testcropping", width = 7.2*2.54, height = 4.5*2.54)
# print(lu1$lineup)
# dev.off()

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

make_interactive <- function(filename2, script, toggle="toggle", high = "#c5c5c5", background="#ffffff") {
  require(gridSVG)
  grid.force()  
  grobs <- grid.ls()
  
  idx <- grep("panel-", grobs$name)
  for (d in idx) { 
    grid.garnish(grobs$name[d],
                 onmouseover=paste("frame('",grobs$name[d+2], ".1.1')", sep=""),
                 onmouseout=paste("deframe('",grobs$name[d+2], ".1.1')", sep=""), 
                 onmousedown=sprintf("%shigh(evt, '%s.1.1', '%s', '%s')", toggle, grobs$name[d+2], high, background)
    )
  }
  # use script on server to get locally executable javascript code
  # or use inline option (default)
  grid.script(filename=script)
  grid.export(filename2)
}

# trim white space function {http://robjhyndman.com/hyndsight/crop-r-figures/}
savepdf <- function(file, width=16, height=10)
{
  fname <- paste(file,".pdf",sep="")
  pdf(fname, width=width/2.54, height=height/2.54,
      pointsize=10)
  par(mgp=c(2.2,0.45,0), tcl=-0.4, mar=c(3.3,3.5,1.1,1))
}
  
# function to create the lineups and linup data

make_interactive_lineups <- function(M, reps, model.effects, null.effects){
  for (r in 1:reps){
    lineup <- create_smfriend_lu(null_eff_struct = null.effects, test_eff_struct = model.effects, M = M)
    filename <- paste0("Data/lineupdata/smallfriends-m-", M, '-rep-', r, ".csv")
    write.csv(lineup$data, sprintf("Data/lineupdata/smallfriends-m-%s-rep-%s.csv",M,r), row.names=FALSE)
    #ggsave(lineup$lineup, file=sprintf("Lineup-Images/pdfs/smallfriends-m-%s-rep-%s.pdf",M,r), width = 7.2, height = 4.5, units = "in")
    savepdf(file = sprintf("Lineup-Images/pdfs/smallfriends-m-%s-rep-%s",M,r), width = 7.2*2.54, height = 4.5*2.54)
    print(lineup$lineup)
    tmpfile <- sprintf("%s.svg",tempfile(tmpdir="Lineup-Images/svgs"))
    make_interactive(filename2 = tmpfile, script=scriptURL,  
                     high="#d5d5d5",  background="#ffffff")
    dev.off()
  }
  return(list(lineup = lineup))
}

#make_interactive_lineups(M = 4, reps=3, model.effects = null_model_eff2, null.effects = eff_models_smallFriends[[39]])


#make_interactive_lineups(M = 3, reps=20, model.effects = null_model_eff2, null.effects = eff_models_smallFriends[[39]])

#make_interactive_lineups(M = 6, reps=20, model.effects = null_model_eff2, null.effects = eff_models_smallFriends[[39]])

#make_interactive_lineups(M = 9, reps=20, model.effects = null_model_eff2, null.effects = eff_models_smallFriends[[39]])

#make_interactive_lineups(M = 12, reps=20, model.effects = null_model_eff2, null.effects = eff_models_smallFriends[[39]])

# make_interactive_lineups(M = 16, reps=20, model.effects = null_model_eff2, null.effects = eff_models_smallFriends[[39]])
