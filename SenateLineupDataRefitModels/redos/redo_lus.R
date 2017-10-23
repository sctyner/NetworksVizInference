# redoing some lineups 10-19-17
library(tidyverse)
library(netvizinf)
library(geomnet)
library(grid)
library(gridSVG)

# for the svgs 
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
# for the pdfs 
savepdf <- function(file, width=16, height=10){
  fname <- paste(file,".pdf",sep="")
  pdf(fname, width=width/2.54, height=height/2.54,
      pointsize=10)
  par(mgp=c(2.2,0.45,0), tcl=-0.4, mar=c(3.3,3.5,1.1,1))
}

# get all the data
redo_dets <- data_frame(data_file = list.files("SenateLineupData/redos/data"))
# create information necessary for making svgs, pdfs, pic details file
redo_dets %>% mutate(data_name = data_file) %>% 
  mutate(data_file = str_replace(data_file, ".csv", "")) %>%
  separate(data_file, c("model", "sign", "difficulty", "rep"), "_") -> redo_dets
redo_dets$model <- as.factor(redo_dets$model)
levels(redo_dets$model) <- c(3,4,2,5)
redo_dets$model <- as.integer(as.character(redo_dets$model))
redo_dets$sign <- as.factor(redo_dets$sign)
levels(redo_dets$sign) <- c(2,1)
redo_dets$sign <- as.integer(as.character(redo_dets$sign))
redo_dets$difficulty <- as.factor(redo_dets$difficulty)
levels(redo_dets$difficulty) <- c(1,3,2)
redo_dets$difficulty <- as.integer(as.character(redo_dets$difficulty))
get_dat_plot <- function(sign, data_name){
  dat <- read_csv(paste0("SenateLineupData/redos/data/", data_name))
  sims <- unique(dat$sim)
  datplot <- setdiff(sims, 1:5)
  unique(dat$ord[which(dat$sim == datplot)])
}

redo_dets %>% mutate(obs_plot_location = map2_int(sign, data_name, get_dat_plot),
                     pic_id = as.integer(paste0(difficulty, sign,model,rep))) -> redo_dets

# combine to get test_param, test_value
dets_parms <- read_csv("SenateLineupData/redos/details-redo.csv")

# join with redo_dets.

names(dets_parms)
names(redo_dets)

redo_dets2 <- left_join(dets_parms, redo_dets)

# make svgs, pdfs 


senate_lu_storage <- function(details, url = "http://www.hofroe.net/examples/lineup/action-back.js"){
  #  browser()
  picture_details <- data_frame(pic_id = integer(0), sample_size = integer(0), 
                                test_param = character(0), param_value = character(0), 
                                p_value = numeric(0), obs_plot_location = integer(0),
                                pic_name = character(0), experiment = character(0), 
                                difficulty = character(0), data_name = character(0))
  
  for(i in 1:nrow(details)){
    # read data
    dat <- read_csv(paste0("SenateLineupData/redos/data/", details$data_name[i]))
    # make plot
    p <-ggplot(data = dat) + 
      geom_net(aes(from_id = from, to_id = to), 
               arrow = arrow(type = 'open', length = unit(2, "points") ), 
               linewidth = .25, singletons = T, fiteach = T, directed = T, 
               color = 'black', arrowgap = .015, arrowsize = .3, size = .3) + 
      theme_net() + 
      theme(panel.background = element_rect(color = 'black')) +
      facet_wrap(~ord)
    savepdf(file = paste0("SenateLineupData/redos/pdfs/senate-mod-",
                          details$model[i], '-sign-', details$sign[i], '-difficulty-', 
                          details$difficulty[i],  '-rep-', details$rep[i]), width = 7.2*2.54, height = 4.5*2.54)
    print(p)
    tmpfile <- sprintf("%s.svg",tempfile(tmpdir="SenateLineupData/redos/svgs"))
    make_interactive(filename2 = tmpfile, script=url,  
                     high="#d5d5d5",  background="#ffffff")
    dev.off()
    picture_details <- picture_details %>% add_row(
      pic_id = details$pic_id[i], sample_size = NA, test_param = details$test_param[i],
      param_value = details$param_value[i], p_value = NA, obs_plot_location = details$obs_plot_location[i],
      pic_name = tmpfile, experiment = "turk22", difficulty = NA, data_name = details$data_name[i])
    
  }
  return(picture_details)
}


redone_pic_details <- senate_lu_storage(details = redo_dets2)

write_csv(redone_pic_details, "SenateLineupData/redos/redo-picture-details.csv")

# redo 4 of the trials 

redo_tri <- data_frame(data_file = list.files("SenateLineupData/redos/trials/"))
redo_tri %>% mutate(data_name = data_file) %>% 
  mutate(data_file = str_replace(data_file, ".csv", "")) %>%
  separate(data_file, c("trial", "model", "type", "rep"), "_") -> redo_tri
redo_tri$model <- c(7, 3, 4, 5)
get_dat_plot <- function(sign, data_name){
  dat <- read_csv(paste0("SenateLineupData/redos/trials/", data_name))
  sims <- unique(dat$sim)
  datplot <- setdiff(sims, 1:5)
  unique(dat$ord[which(dat$sim == datplot)])
}

redo_tri %>% mutate(obs_plot_location = map2_int(type, data_name, get_dat_plot),
                     pic_id = as.integer(paste0(model,type, rep))) -> redo_tri
redo_tri <- mutate(redo_tri, test_param = model, param_value = type)

senate_lu_storage <- function(details, url = "http://www.hofroe.net/examples/lineup/action-back.js"){
  #  browser()
  picture_details <- data_frame(pic_id = integer(0), sample_size = integer(0), 
                                test_param = character(0), param_value = character(0), 
                                p_value = numeric(0), obs_plot_location = integer(0),
                                pic_name = character(0), experiment = character(0), 
                                difficulty = character(0), data_name = character(0))
  
  for(i in 1:nrow(details)){
    # read data
    dat <- read_csv(paste0("SenateLineupData/redos/trials/", details$data_name[i]))
    # make plot
    p <-ggplot(data = dat) + 
      geom_net(aes(from_id = from, to_id = to), 
               arrow = arrow(type = 'open', length = unit(2, "points") ), 
               linewidth = .25, singletons = T, fiteach = T, directed = T, 
               color = 'black', arrowgap = .015, arrowsize = .3, size = .3) + 
      theme_net() + 
      theme(panel.background = element_rect(color = 'black')) +
      facet_wrap(~ord)
    savepdf(file = paste0("SenateLineupData/redos/trials/pdfs/trial-senate-mod-",
                          details$model[i], '-type-', details$type[i], '-rep-', 
                          details$rep[i]), width = 7.2*2.54, height = 4.5*2.54)
    print(p)
    tmpfile <- sprintf("%s.svg",tempfile(tmpdir="SenateLineupData/redos/svgs"))
    make_interactive(filename2 = tmpfile, script=url,  
                     high="#d5d5d5",  background="#ffffff")
    dev.off()
    picture_details <- picture_details %>% add_row(
      pic_id = details$pic_id[i], sample_size = NA, test_param = details$test_param[i],
      param_value = details$param_value[i], p_value = NA, obs_plot_location = details$obs_plot_location[i],
      pic_name = tmpfile, experiment = "turk22", difficulty = NA, data_name = details$data_name[i])
    
  }
  return(picture_details)
}


redo_trials_Details <- senate_lu_storage(details = redo_tri)
write_csv(redo_trials_Details, "SenateLineupData/redos/trials/picture-details-trials.csv")

