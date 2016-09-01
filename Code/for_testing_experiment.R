# make really easy lineups

source("Code/03c_complete_lineup_creation.R")

make_interactive_lineups_M1M2 <- function(M, reps, model.effects, null.effects){
  for (r in 1:reps){
    lineup <- create_smfriend_lu(null_eff_struct = null.effects, test_eff_struct = model.effects, M = M)
    filename <- paste0("Data/lineupdata_experiment/smallfriends-m-", M, '-rep-', r, ".csv")
    write.csv(lineup$data, sprintf("Data/lineupdata_experiment/smallfriends-m-%s-rep-%s.csv",M,r), row.names=FALSE)
    #ggsave(lineup$lineup, file=sprintf("Lineup-Images/pdfs/smallfriends-m-%s-rep-%s.pdf",M,r), width = 7.2, height = 4.5, units = "in")
    savepdf(file = sprintf("Lineup-Images/ForExperimentTraining/pdfs/smallfriends-m-%s-rep-%s",M,r), width = 7.2*2.54, height = 4.5*2.54)
    print(lineup$lineup)
    tmpfile <- sprintf("%s.svg",tempfile(tmpdir="Lineup-Images/ForExperimentTraining/svgs"))
    make_interactive(filename2 = tmpfile, script=scriptURL,  
                     high="#d5d5d5",  background="#ffffff")
    dev.off()
  }
  return(list(lineup = lineup))
}

make_interactive_lineups_M1M2(M = 12, reps = 20, model.effects = null_model_eff2, null.effects = eff_models_smallFriends[[39]])

make_interactive_lineups_M2M1 <- function(M, reps, model.effects, null.effects){
  for (r in 1:reps){
    lineup <- create_smfriend_lu(null_eff_struct = null.effects, test_eff_struct = model.effects, M = M)
    filename <- paste0("Data/lineupdata_experiment/smallfriends-rev-m-", M, '-rep-', r, ".csv")
    write.csv(lineup$data, sprintf("Data/lineupdata_experiment/smallfriends-rev-m-%s-rep-%s.csv",M,r), row.names=FALSE)
    #ggsave(lineup$lineup, file=sprintf("Lineup-Images/pdfs/smallfriends-m-%s-rep-%s.pdf",M,r), width = 7.2, height = 4.5, units = "in")
    savepdf(file = sprintf("Lineup-Images/ForExperimentTraining/pdfs/smallfriends-rev-m-%s-rep-%s",M,r), width = 7.2*2.54, height = 4.5*2.54)
    print(lineup$lineup)
    tmpfile <- sprintf("%s.svg",tempfile(tmpdir="Lineup-Images/ForExperimentTraining/svgs"))
    make_interactive(filename2 = tmpfile, script=scriptURL,  
                     high="#d5d5d5",  background="#ffffff")
    dev.off()
  }
  return(list(lineup = lineup))
}

make_interactive_lineups_M2M1(M = 12, reps = 20, model.effects = eff_models_smallFriends[[39]],  null.effects = null_model_eff2)

# pick ones that are easy 

handpickedlu1 <- data.frame(rep = c(3,1,5,8,8,10,11,13,15,15,16,3), 
                            plot_order = c(7,4,2,3,7,5,6,12,5,6,4,5), 
                            type = c(rep("M2",11), "M1"))
handpickedlu2 <- data.frame(rep = c(1,3,2,3,8,10,13,16,17,18,19,16), 
                            plot_order = c(8,1,7,11,9,12,9,3,3,7,6,8), 
                            type = c(rep("M2",11), "M1"))
handpickedlu3 <- data.frame(rep = c(10,12,14,15,16,17,17,18,19,19,20,17), 
                            plot_order = c(11,9,9,10,11,12,1,6,7,12,10,2), 
                            type = c(rep("M2",11), "M1"))
# note: last one is always the "data" plot
get_handpicked_data <- function(rep, order, type){
  handpickedlu_data <- data.frame()
  for (i in 1:length(rep)){
  dat <- read.csv(paste0("Lineup-Images/ForExperimentTraining/datafiles/smallfriends-rev-m-12-rep-", rep[i], ".csv"))
  subdat <- dat %>% dplyr::filter(plot_order == order[i])
  subdat$count <- i
  subdat$rep <- rep[i]
  subdat$type <- type[i]
  handpickedlu_data <- rbind(handpickedlu_data, subdat)
  }
  return(handpickedlu_data)
}

handpickedlu_dat1 <- get_handpicked_data(rep = handpickedlu1$rep, 
                                         order = handpickedlu1$plot_order,
                                         type = handpickedlu1$type)
head(handpickedlu_dat1)
set.seed(12345)
handpickedlu_dat1$plot_order2 <- rep(sample(12),as.numeric(table(handpickedlu_dat1$count)))

handpickedlu_dat2 <- get_handpicked_data(rep = handpickedlu2$rep, 
                                         order = handpickedlu2$plot_order,
                                         type = handpickedlu1$type)
handpickedlu_dat2
set.seed(23456)
handpickedlu_dat2$plot_order2 <- rep(sample(12),as.numeric(table(handpickedlu_dat2$count)))


handpickedlu_dat3 <- get_handpicked_data(rep = handpickedlu3$rep, 
                                         order = handpickedlu3$plot_order,
                                         type = handpickedlu3$type)
handpickedlu_dat3
set.seed(34567)
handpickedlu_dat3$plot_order2 <- rep(sample(12),as.numeric(table(handpickedlu_dat3$count)))

make_interactive_lineups_handpicked <- function(dat, rep){
  require(geomnet)
  svg_filenames <- rep(NA, nrow(dat))
  savepdf(file = sprintf("Lineup-Images/ForExperimentTraining/pdfs/smallfriends-rev-m-12-rep-%s",rep), width = 7.2*2.54, height = 4.5*2.54)
  theplot <- ggplot(data = dat, aes(from_id = X1, to_id = X2)) + 
    geom_net(fiteach = TRUE, directed = F, size = 1, arrowsize = .5) +
    facet_wrap(~plot_order2) + theme_net() + 
    theme(panel.background = element_rect(fill = "white", color = 'black'))
    print(theplot)
    tmpfile <- sprintf("%s.svg",tempfile(tmpdir="Lineup-Images/ForExperimentTraining/svgs"))
    make_interactive(filename2 = tmpfile, script=scriptURL,  
                     high="#d5d5d5",  background="#ffffff")
    dev.off()
    svg_filenames <- tmpfile
  return(svg_filenames)
}

svg1 <- make_interactive_lineups_handpicked(dat = handpickedlu_dat1, rep = "a")

handpickedlu_dat1$svg <- svg1

svg2 <- make_interactive_lineups_handpicked(dat = handpickedlu_dat2, rep = "b")

handpickedlu_dat2$svg <- svg2

svg3 <- make_interactive_lineups_handpicked(dat = handpickedlu_dat3, rep = "c")

handpickedlu_dat3$svg <- svg3

readr::write_csv(handpickedlu_dat1, "Lineup-Images/ForExperimentTraining/datafiles/small-friends-rev-m-12-rep-a.csv")
readr::write_csv(handpickedlu_dat2, "Lineup-Images/ForExperimentTraining/datafiles/small-friends-rev-m-12-rep-b.csv")
readr::write_csv(handpickedlu_dat3, "Lineup-Images/ForExperimentTraining/datafiles/small-friends-rev-m-12-rep-c.csv")
