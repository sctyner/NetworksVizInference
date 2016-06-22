# write a new function for lineup creation. 
# WANT:
# 1. All lineup images come from same instance of estimated parameters
# Doesn't happen now as it is written, due to the create_smfriends_lineup fun.
# idea: fit the models 100 times and average the effect estimates to get values
# to use in all 
# 2. Split the function into two: 
#  a. create and save the data into a csv file 
#  b. get the data and put it into a network

# 

library(ggplot2)
library(geomnet)
library(dplyr)
library(RSiena)
library(grid)
library(gridSVG)

# get data 
null_model_dist <- read.csv("../Data/distribution_null_model.csv")
null_model_dist <- null_model_dist[,-1]
alt_model_dist <- read.csv("../Data/distribution_jumpTT_model.csv")
alt_model_dist <- alt_model_dist[,-1]
alt_model2_dist <- read.csv("../Data/distribution_dblpairs_model.csv")
alt_model2_dist <- alt_model2_dist[,-1]

null_mod_sv <- as.numeric(null_model_dist %>% summarise_each(funs(mean)))
alt_mod_sv <- as.numeric(alt_model_dist %>% summarise_each(funs(mean)))
alt_mod2_sv <- as.numeric(alt_model2_dist %>% summarise_each(funs(mean)))

eff_models_smallFriends <- readRDS("Data/eff_models_smallFriends.RDS")


# function 1: create and save data:

create_lu_dat_xy <- function(null_model, null_model_svs, alt_model, alt_model_svs, m, my_dat = mysmalldata, sd, ...){
  require(RSiena)
  # null model setup
  null_model$initialValue[null_model$include] <- null_model_svs
  # alt model setup
  alt_model$initialValue[alt_model$include] <- alt_model_svs
  # algorithm setup
  sim_algorithm <- sienaAlgorithmCreate( projname = "sim_model", cond = FALSE,
                                         useStdInits = FALSE, nsub = 0 , simOnly = TRUE)
  # get model sims
  set.seed(sd) # for replication
  null_sims    <-  siena07(sim_algorithm, data = my_dat, effects = null_model, 
                          returnDeps = TRUE, batch=TRUE, verbose = FALSE, 
                         silent = TRUE)
  alt_sims    <-  siena07(sim_algorithm, data = my_dat, effects = alt_model, 
                           returnDeps = TRUE, batch=TRUE, verbose = FALSE, 
                           silent = TRUE)
  # create lineup data
    n <- m-1
    null_data <- NULL
    for (i in 1:n){
      net_df <- merge(data.frame(null_sims$sims[[1000-(n-i)]][[1]][[1]][[1]])[,-3], 
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
    test_data <- merge(data.frame(alt_sims$sims[[1000]][[1]][[1]][[1]])[,-3], 
                       data.frame(id = 1:16), by.x = "X1", by.y = "id",
                       all = T)
    for (j in 1:nrow(test_data)){
      if (!(test_data$X1[j] %in% test_data$X2) & is.na(test_data$X2[j])){
        test_data$X2[j] <- test_data$X1[j]
      } else {test_data$X2[j] <- test_data$X2[j]}
    }
    test_data$count <- m  # test data is always indexed by M
    to_plot <- rbind(null_data, test_data)
    return(to_plot)
}

create_lu_dat_yx <- function(null_model, null_model_svs, alt_model, alt_model_svs, m, my_dat = mysmalldata, sd, ...){
  require(RSiena)
  # null model setup
  null_model$initialValue[null_model$include] <- null_model_svs
  # alt model setup
  alt_model$initialValue[alt_model$include] <- alt_model_svs
  # algorithm setup
  sim_algorithm <- sienaAlgorithmCreate( projname = "sim_model", cond = FALSE,
                                         useStdInits = FALSE, nsub = 0 , simOnly = TRUE)
  # get model sims
  set.seed(sd) # for replication
  null_sims    <-  siena07(sim_algorithm, data = my_dat, effects = null_model, 
                           returnDeps = TRUE, batch=TRUE, verbose = FALSE, 
                           silent = TRUE)
  alt_sims    <-  siena07(sim_algorithm, data = my_dat, effects = alt_model, 
                          returnDeps = TRUE, batch=TRUE, verbose = FALSE, 
                          silent = TRUE)
  # create lineup data
  n <- m-1
  alt_data <- NULL
  for (i in 1:n){
    net_df <- merge(data.frame(alt_sims$sims[[1000-(n-i)]][[1]][[1]][[1]])[,-3], 
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
  null_data <- merge(data.frame(null_sims$sims[[1000]][[1]][[1]][[1]])[,-3], 
                     data.frame(id = 1:16), by.x = "X1", by.y = "id",
                     all = T)
  for (j in 1:nrow(null_data)){
    if (!(null_data$X1[j] %in% null_data$X2) & is.na(null_data$X2[j])){
      null_data$X2[j] <- null_data$X1[j]
    } else {null_data$X2[j] <- null_data$X2[j]}
  }
  null_data$count <- m  # null data is always indexed by M
  to_plot <- rbind(alt_data, null_data)
  return(to_plot)
}

create_lu_dat_xy(null_model = null_model_eff2, null_model_svs = null_mod_sv,
                 alt_model = eff_models_smallFriends[[39]], alt_model_svs = alt_mod_sv,
                 m = 3, sd = 1)

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


# create_lu <- function(null_model, null_model_svs, alt_model, alt_model_svs, M, my_dat = mysmalldata, reps, yx = FALSE,...){
#   for (r in 1:reps){
#     if (yx == FALSE){
#       lu_dat <- create_lu_dat_xy(null_model, null_model_svs, alt_model, alt_model_svs, m=M, my_dat, sd = (r + M))
#     } else lu_dat <- create_lu_dat_yx(null_model, null_model_svs, alt_model, alt_model_svs,m=M, my_dat, sd = (r + M))
#     lu_dat$plot_order <- rep(sample(M), as.vector(table(lu_dat$count)))
#     lu_dat$X1 <- as.factor(lu_dat$X1)
#     lu_dat$X2 <- as.factor(lu_dat$X2)
#     if (yx == FALSE){
#       filename <- paste0("Data/lineupdata/smallfriends-m-", M, '-rep-', r, ".csv")
#       filename2 <- sprintf("Lineup-Images/pdfs/smallfriends-m-%s-rep-%s",M,r)
#     } else {
#       filename <- paste0("Data/lineupdata/smallfriends-rev-m-", M, '-rep-', r, ".csv")
#       filename2 <- sprintf("Lineup-Images/pdfs/smallfriends-rev-m-%s-rep-%s",M,r)
#     }
#     write.csv(lu_dat, file = filename)
#     lu_plot <- ggplot(data = lu_dat, aes(from_id = X1, to_id = X2)) + 
#       geom_net(fiteach = TRUE, directed = F, size = 1, arrowsize = .5) +
#       facet_wrap(~plot_order) + theme_net() + theme(panel.background = element_rect(fill = "white", color = 'black'))
#     savepdf(file = filename2, width = 8*2.54, height = 8*2.54)
#     print(lu_plot)
#     dev.off()
#     tmpfile <- sprintf("%s.svg",tempfile(tmpdir="Lineup-Images/svgs"))
#     make_interactive(filename2 = tmpfile, script=scriptURL,  
#                      high="#d5d5d5",  background="#ffffff")
#     }
# }

# create_lu(null_model = null_model_eff2, null_model_svs = null_mod_sv,
#           alt_model = eff_models_smallFriends[[39]], alt_model_svs = alt_mod_sv,
#           M = 3, reps = 20)
# 
# create_lu(null_model = null_model_eff2, null_model_svs = null_mod_sv,
#           alt_model = eff_models_smallFriends[[39]], alt_model_svs = alt_mod_sv,
#           M = 6, reps = 20)
# 
# create_lu(null_model = null_model_eff2, null_model_svs = null_mod_sv,
#           alt_model = eff_models_smallFriends[[39]], alt_model_svs = alt_mod_sv,
#           M = 9, reps = 20)
# create_lu(null_model = null_model_eff2, null_model_svs = null_mod_sv,
#           alt_model = eff_models_smallFriends[[39]], alt_model_svs = alt_mod_sv,
#           M = 12, reps = 20)
# create_lu(null_model = null_model_eff2, null_model_svs = null_mod_sv,
#           alt_model = eff_models_smallFriends[[39]], alt_model_svs = alt_mod_sv,
#           M = 16, reps = 20)
# 
# create_lu(null_model = null_model_eff2, null_model_svs = null_mod_sv,
#           alt_model = eff_models_smallFriends[[39]], alt_model_svs = alt_mod_sv,
#           M = 3, reps = 20, yx = TRUE)
# create_lu(null_model = null_model_eff2, null_model_svs = null_mod_sv,
#           alt_model = eff_models_smallFriends[[39]], alt_model_svs = alt_mod_sv,
#           M = 6, reps = 20, yx = TRUE)
# create_lu(null_model = null_model_eff2, null_model_svs = null_mod_sv,
#           alt_model = eff_models_smallFriends[[39]], alt_model_svs = alt_mod_sv,
#           M = 9, reps = 20, yx = TRUE)
# create_lu(null_model = null_model_eff2, null_model_svs = null_mod_sv,
#           alt_model = eff_models_smallFriends[[39]], alt_model_svs = alt_mod_sv,
#           M = 12, reps = 20, yx = TRUE)
# create_lu(null_model = null_model_eff2, null_model_svs = null_mod_sv,
#           alt_model = eff_models_smallFriends[[39]], alt_model_svs = alt_mod_sv,
#           M = 16, reps = 20, yx = TRUE)

#write same function for additional effect. 
create_lu2 <- function(null_model, null_model_svs, alt_model, alt_model_svs, M, my_dat = mysmalldata, reps, yx = FALSE,...){
  for (r in 1:reps){
    if (yx == FALSE){
      lu_dat <- create_lu_dat_xy(null_model, null_model_svs, alt_model, alt_model_svs, m=M, my_dat, sd = (r + M))
    } else lu_dat <- create_lu_dat_yx(null_model, null_model_svs, alt_model, alt_model_svs,m=M, my_dat, sd = (r + M))
    lu_dat$plot_order <- rep(sample(M), as.vector(table(lu_dat$count)))
    lu_dat$X1 <- as.factor(lu_dat$X1)
    lu_dat$X2 <- as.factor(lu_dat$X2)
    if (yx == FALSE){
      filename <- paste0("Data/lineupdata/smallfriends-eff2-m-", M, '-rep-', r, ".csv")
      filename2 <- sprintf("Lineup-Images/pdfs/smallfriends-eff2-m-%s-rep-%s",M,r)
    } else {
      filename <- paste0("Data/lineupdata/smallfriends-eff2-rev-m-", M, '-rep-', r, ".csv")
      filename2 <- sprintf("Lineup-Images/pdfs/smallfriends-eff2-rev-m-%s-rep-%s",M,r)
    }
    write.csv(lu_dat, file = filename)
    lu_plot <- ggplot(data = lu_dat, aes(from_id = X1, to_id = X2)) + 
      geom_net(fiteach = TRUE, directed = F, size = 1, arrowsize = .5) +
      facet_wrap(~plot_order) + theme_net() + theme(panel.background = element_rect(fill = "white", color = 'black'))
    savepdf(file = filename2, width = 8*2.54, height = 8*2.54)
    print(lu_plot)
    dev.off()
    tmpfile <- sprintf("%s.svg",tempfile(tmpdir="Lineup-Images/svgs"))
    make_interactive(filename2 = tmpfile, script=scriptURL,  
                     high="#d5d5d5",  background="#ffffff")
  }
}


create_lu2(null_model = null_model_eff2, null_model_svs = null_mod_sv,
          alt_model = eff_models_smallFriends[[18]], alt_model_svs = alt_mod2_sv,
          M = 3, reps = 20)

create_lu2(null_model = null_model_eff2, null_model_svs = null_mod_sv,
          alt_model = eff_models_smallFriends[[18]], alt_model_svs = alt_mod2_sv,
          M = 6, reps = 20)

create_lu2(null_model = null_model_eff2, null_model_svs = null_mod_sv,
          alt_model = eff_models_smallFriends[[18]], alt_model_svs = alt_mod2_sv,
          M = 9, reps = 20)
create_lu2(null_model = null_model_eff2, null_model_svs = null_mod_sv,
          alt_model = eff_models_smallFriends[[18]], alt_model_svs = alt_mod2_sv,
          M = 12, reps = 20)
create_lu2(null_model = null_model_eff2, null_model_svs = null_mod_sv,
          alt_model = eff_models_smallFriends[[18]], alt_model_svs = alt_mod2_sv,
          M = 16, reps = 20)

create_lu2(null_model = null_model_eff2, null_model_svs = null_mod_sv,
          alt_model = eff_models_smallFriends[[18]], alt_model_svs = alt_mod2_sv,
          M = 3, reps = 20, yx = TRUE)
create_lu2(null_model = null_model_eff2, null_model_svs = null_mod_sv,
          alt_model = eff_models_smallFriends[[18]], alt_model_svs = alt_mod2_sv,
          M = 6, reps = 20, yx = TRUE)
create_lu2(null_model = null_model_eff2, null_model_svs = null_mod_sv,
          alt_model = eff_models_smallFriends[[18]], alt_model_svs = alt_mod2_sv,
          M = 9, reps = 20, yx = TRUE)
create_lu2(null_model = null_model_eff2, null_model_svs = null_mod_sv,
          alt_model = eff_models_smallFriends[[18]], alt_model_svs = alt_mod2_sv,
          M = 12, reps = 20, yx = TRUE)
create_lu2(null_model = null_model_eff2, null_model_svs = null_mod_sv,
          alt_model = eff_models_smallFriends[[18]], alt_model_svs = alt_mod2_sv,
          M = 16, reps = 20, yx = TRUE)
