# create lineups for experiment
library(tidyverse)
library(netvizinf)
library(geomnet)
library(grid)
library(gridSVG)
details <- data_frame(pic_id = integer(0), data_file = character(0), obs_plot_location = integer(0))

# get all the data
details <- data_frame(data_file = c(list.files("SenateLineupData/Non-GoF/"), list.files("SenateLineupData/GoF/")))
details %>% mutate(data_name = data_file) %>% 
  mutate(data_file = str_replace(data_file, ".csv", "")) %>%
  separate(data_file, c("model", "sign", "difficulty", "rep"), "_") -> details
details$model <- as.factor(details$model)
levels(details$model) <- c(1,3,4,2,6,5)
details$model <- as.integer(as.character(details$model))
details$sign <- as.factor(details$sign)
levels(details$sign) <- c(9,2,1)
details$sign <- as.integer(as.character(details$sign))
details$difficulty <- as.factor(details$difficulty)
levels(details$difficulty) <- c(1,3,2)
details$difficulty <- as.integer(as.character(details$difficulty))
details$difficulty[is.na(details$difficulty)] <- 9
details$rep[is.na(details$rep)] <- 1
get_dat_plot <- function(sign, data_name){
  if(sign == 9){
    dat <- read_csv(paste0("SenateLineupData/GoF/", data_name))
  } else {
    dat <- read_csv(paste0("SenateLineupData/Non-GoF/", data_name))
  }
  sims <- unique(dat$sim)
  datplot <- setdiff(sims, 1:5)
  unique(dat$ord[which(dat$sim == datplot)])
}

details %>% mutate(obs_plot_location = map2_int(sign, data_name, get_dat_plot)) -> details
details %>% mutate(pic_id = as.integer(paste0(difficulty, sign,model,rep))) -> details

# get test_param, test_value
library(readxl)
expDet <- read_excel("SenateLineupData/effects_for_lineups.xlsx")
expDet <- expDet %>% mutate(test_param = Model)
expDet$test_param <- as.factor(expDet$test_param)
levels(expDet$test_param) <- c(1,3,4,2,6,5)
expDet$test_param <- as.integer(as.character(expDet$test_param))
expDet$model <- expDet$test_param
expDet$Sign <- as.factor(expDet$Sign)
levels(expDet$Sign) <- c(2,1,9)
expDet$Sign <- as.integer(as.character(expDet$Sign))
expDet$sign <- expDet$Sign
expDet$Difficulty <- as.factor(expDet$Difficulty)
levels(expDet$Difficulty) <- c(9,1,3,2)
expDet$Difficulty <- as.integer(as.character(expDet$Difficulty))
expDet$difficulty <- expDet$Difficulty
expDet <- expDet %>% select(model, sign, difficulty, param_value = `Multiplier Value`)

details <- left_join(details, expDet)
details <- details %>% mutate(test_param = model)
details$test_param <- as.factor(details$test_param)
levels(details$test_param) <- c("dens", "recip", "jttp", "jtts", "simttb", "samettp")
head(details)

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

senate_lu_storage2 <- function(details, url = "http://www.hofroe.net/examples/lineup/action-back.js"){
#  browser()
  picture_details <- data_frame(pic_id = integer(0), sample_size = integer(0), test_param = character(0),
                                param_value = character(0), p_value = numeric(0), obs_plot_location = integer(0),
                                pic_name = character(0), experiment = character(0), difficulty = character(0), data_name = character(0))
  
  for(i in 1:nrow(details)){
    if (details$sign[i] == 9){
      dat <- read_csv(paste0("SenateLineupData/GoF/", details$data_name[i]))
    } else{
      dat <- read_csv(paste0("SenateLineupData/Non-GoF/", details$data_name[i]))
    }
    p <-ggplot(data = dat) + 
      geom_net(aes(from_id = from, to_id = to), 
               arrow = arrow(type = 'open', length = unit(2, "points") ), 
               linewidth = .25, singletons = T, fiteach = T, directed = T, 
               color = 'black', arrowgap = .015, arrowsize = .3, size = .3) + 
      theme_net() + 
      theme(panel.background = element_rect(color = 'black')) +
      facet_wrap(~ord)
    savepdf(file = paste0("Lineup-Images/senate/ForExperiment10-15/pdfs/senate-mod-",
                          details$model[i], '-sign-', details$sign[i], '-difficulty-', 
                          details$difficulty[i],  '-rep-', details$rep[i]), width = 7.2*2.54, height = 4.5*2.54)
    print(p)
    tmpfile <- sprintf("%s.svg",tempfile(tmpdir="Lineup-Images/senate/ForExperiment10-15/svgs/"))
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

final_lineup_details <- senate_lu_storage2(details = details)
write.csv(final_lineup_details, "final_lineup_details_10_15.csv")

final_lineup_details$pic_name <- str_replace(final_lineup_details$pic_name, "//", "/")

final_lineup_details <- final_lineup_details %>% filter(!(pic_id %in% c(9931,9941,9951)))

# redo the gof ones to actually add the data in there...

load("Data/senate/se112adjmat.RDS")
se112adj
wave2 <- data.frame(se112adj)
senators <- data.frame(id = colnames(wave2), number = 1:155)
wave2$from <- colnames(wave2)
wave2 <- wave2 %>% gather(to, val, Alan.Stuart.Franken:William.Cowan) %>% filter(val > 0)
wave2 <- merge(wave2, senators, by.x = "from", by.y = "id", all = T)
wave2$from <- senators$number[match(wave2$from, senators$id)]
wave2$to <- senators$number[match(wave2$to, senators$id)]
wave2$from <- paste0("V", wave2$from)
wave2$to <- ifelse(is.na(wave2$to), NA, paste0("V", wave2$to))
wave2 <- wave2 %>% mutate(sim = 1001, model = "data", wave = 1) %>% select(-c(val, number))


# make new function for the GoF plots. 

details_gof <- data_frame(data_file = list.files("SenateLineupData/GoF"))
details_gof %>% mutate(data_name = data_file) %>% 
  mutate(data_file = str_replace(data_file, ".csv", "")) %>%
  separate(data_file, c("model", "sign", "difficulty", "rep"), "_") -> details_gof
details_gof$model <- as.factor(details_gof$model)
levels(details_gof$model) <- c(7,3,4,6)
details_gof$model <- as.integer(as.character(details_gof$model))
details_gof$sign <- 9
details_gof$difficulty <- as.integer(as.character(details_gof$difficulty))
details_gof$rep <- as.integer(as.character(details_gof$rep))

details_gof$test_param <- details_gof$model
details_gof$test_param <- as.factor(details_gof$test_param)
levels(details_gof$test_param) <- c("jttp", "jtts","simttb", "bigmod")
details_gof$test_param <- as.character(details_gof$test_param)
details_gof <- details_gof %>% mutate(pic_id = as.integer(paste0(difficulty, sign, model, rep)), 
                        param_value = "data", 
                        obs_plot_location = map2_int(sign, data_name, get_dat_plot)) 
names(details_gof) 
names(details)

picture_details_gof2 <- data_frame(pic_id = integer(0), sample_size = integer(0), test_param = character(0),
                              param_value = character(0), p_value = numeric(0), obs_plot_location = integer(0),
                              pic_name = character(0), experiment = character(0), difficulty = character(0), data_name = character(0))

for(i in 2:nrow(details_gof2)){
    dat <- read_csv(paste0("SenateLineupData/GoF/", details_gof2$data_name[i]))
    datplot <- unique(dat$ord[which(dat$sim == 1001)])
    #datplot <- unique(dat$ord[dat$sim == 1001])
    #details2 <- filter(details, test_param == "bigmod") 
    # dat <- dat %>% filter(sim != 1001) %>% 
    #   bind_rows(wave2) %>% 
    #   mutate(ord = ifelse(is.na(ord), datplot, ord))
    p <- ggplot(data = dat) + 
          geom_net(aes(from_id = from, to_id = to), 
             arrow = arrow(type = 'open', length = unit(2, "points") ), 
             linewidth = .25, singletons = T, fiteach = T, directed = T, 
             color = 'black', arrowgap = .015, arrowsize = .3, size = .3) + 
          theme_net() + 
          theme(panel.background = element_rect(color = 'black')) +
          facet_wrap(~ord)
  savepdf(file = paste0("Lineup-Images/senate/ForExperiment10-15/pdfs/senate-mod-",
                        details_gof2$model[i], '-sign-', details_gof2$sign[i], '-difficulty-', 
                        details_gof2$difficulty[i],  '-rep-', details_gof2$rep[i]), width = 7.2*2.54, height = 4.5*2.54)
  print(p)
  tmpfile <- sprintf("%s.svg",tempfile(tmpdir="Lineup-Images/senate/ForExperiment10-15/svgs"))
  make_interactive(filename2 = tmpfile, script=url,  
                   high="#d5d5d5",  background="#ffffff")
  dev.off()
  picture_details_gof2 <- picture_details_gof2 %>% add_row(
    pic_id = details_gof2$pic_id[i], sample_size = NA, test_param = details_gof2$test_param[i],
    param_value = details_gof2$param_value[i], p_value = NA, obs_plot_location = datplot,
    pic_name = tmpfile, experiment = "turk22", difficulty = NA, data_name = details_gof2$data_name[i])
}  


picture_details_gof$pic_name <- str_replace(picture_details_gof$pic_name, "//", "/")
picture_details_gof3 <- bind_rows(picture_details_gof, picture_details_gof2)

write_csv(picture_details_gof3, "final_lineup_details_gof_10_16.csv")

# dat <- read_csv(paste0("SenateLineupData/GoF/", details$data_name[109]))
# datplot <- unique(dat$ord[dat$sim == 1001])
# dat %>% filter(sim != 1001) %>% 
#   bind_rows(wave2) %>% 
#   mutate(ord = ifelse(is.na(ord), datplot, ord)) %>% 
#   ggplot() + 
#   geom_net(aes(from_id = from, to_id = to), 
#            arrow = arrow(type = 'open', length = unit(2, "points") ), 
#            linewidth = .25, singletons = T, fiteach = T, directed = T, 
#            color = 'black', arrowgap = .015, arrowsize = .3, size = .3) + 
#   theme_net() + 
#   theme(panel.background = element_rect(color = 'black')) +
#   facet_wrap(~ord)

deets1 <- read_csv("final_lineup_details_10_15.csv")
deets2 <- read_csv("final_lineup_details_gof_10_16.csv")
deets1$param_value <- as.character(deets1$param_value)
names(deets1)
names(deets2)

deets <- bind_rows(deets1[,-1], deets2)

write_csv(deets, "~/Desktop/Dissertation/lineups/experiments/turk22/details/picture-details.csv")
