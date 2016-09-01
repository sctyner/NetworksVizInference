# picking the lineups!!!!

# M1v M2 

# want size M = 12 with upper_qu = 1 /

data.frame(lus_summary_2 %>% filter(part == "smallfriends" & 
                         param_name == "transitive triplets jumping alcohol2" &  
                         upper_qu == 1) %>% 
  arrange(desc(data_est))) 

# only M 12 rep 7

# want size M = 12 with upper_qu large (>.5) and small data_dist
data.frame(lus_summary_2 %>% filter(part == "smallfriends" & 
                                    param_name == "transitive triplets jumping alcohol2" & 
                                    upper_qu < 1 & upper_qu >=0.5 &
                                    data_dist < 2) %>% 
             arrange(data_dist)) 

# M 12: rep 19, 11, 3, 4

7, 19, 11, 3, 4

# M2 v M1 

# want size M = 12 with lower_qu = 1 

data.frame(lus_summary_2b %>% filter(part == "smallfriends-rev" & 
                                      param_name == "transitive triplets jumping alcohol2" &  
                                      lower_qu == 1) %>% 
             arrange(data_dist)) 

# M 12: rep 18,4, 15

# want size M = 12 with lower_qu large (>.5) and small data_dist

data.frame(lus_summary_2b %>% filter(part == "smallfriends-rev" & 
                                       param_name == "transitive triplets jumping alcohol2" &  
                                       lower_qu < 1 & lower_qu >=.75 & 
                                       data_dist < 2) %>% 
             arrange(data_dist)) 
# M 12: rep 2, 20

18,4, 15, 2, 20 
# M1 v M3

# want size M = 12 with lower_qu = 1 

data.frame(lus_summary_3 %>% filter(part == "smallfriends-eff2" & 
                                       param_name == "number pairs at doubly achieved distance 2" &  
                                       lower_qu == 1) %>% 
             arrange(desc(data_dist))) 

# NO M = 12

# want size M = 12 with lower_qu large (>.5) and small data_dist

data.frame(lus_summary_3 %>% filter(part == "smallfriends-eff2" & 
                                    param_name == "number pairs at doubly achieved distance 2" &  
                                    lower_qu < 1 & lower_qu >=.4) %>% 
             arrange(desc(data_dist))) 
# M 12 rep: 17, 16, 20, 1, 12

# M3 v M1

# want size M = 12 with upper_qu = 1 

data.frame(lus_summary_3b %>% filter(part == "smallfriends-eff2-rev" & 
                                      param_name == "number pairs at doubly achieved distance 2" &  
                                      upper_qu == 1) %>% 
             arrange(data_dist)) 

# M 12 rep: 9, 12

# want size M = 12 with upper_qu large (>.5) and small data_dist

data.frame(lus_summary_3b %>% filter(part == "smallfriends-eff2-rev" & 
                            param_name == "number pairs at doubly achieved distance 2" &  
                            upper_qu < 1 & upper_qu >=.6, 
                            data_dist < 2) %>% 
  arrange(desc(upper_qu)))

# M 12: rep: 17, 8, 4

9, 12, 17, 8, 4

# M1 v M2 big JTTs

jtts %>% filter(model == "smallfriends", jtt >= 15) %>% arrange(desc(jtt))


library(RSiena)
library(geomnet)
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

make_interactive_lineups_final <- function(dat){
  require(geomnet)
  svg_filenames <- rep(NA, nrow(dat))
  for (i in 1:nrow(dat)){
    lineupname <- dat$lineupname[i]
    M <- dat$M[i]
    r <- dat$rep[i]
    datafile <- paste0("Data/lineupdata/",lineupname,"-m-", M, '-rep-', r, ".csv")
    lu_data <- read.csv(file = datafile, stringsAsFactors = FALSE)
    savepdf(file = sprintf("Lineup-Images/ForExperimentFinal/pdfs/%s-m-%s-rep-%s",lineupname,M,r), width = 7.2*2.54, height = 4.5*2.54)
    theplot <- ggplot(data = lu_data, aes(from_id = X1, to_id = X2)) + 
      geom_net(fiteach = TRUE, directed = F, size = 1, arrowsize = .5) +
      facet_wrap(~plot_order) + theme_net() + 
      theme(panel.background = element_rect(fill = "white", color = 'black'))
    print(theplot)
    tmpfile <- sprintf("%s.svg",tempfile(tmpdir="Lineup-Images/ForExperimentFinal/svgs"))
    make_interactive(filename2 = tmpfile, script=scriptURL,  
                     high="#d5d5d5",  background="#ffffff")
    dev.off()
    svg_filenames[i] <- tmpfile
  }
  return(svg_filenames)
}

for_experiment_data <- data.frame(lineupname = rep(c("smallfriends", "smallfriends-rev", "smallfriends-eff2", "smallfriends-eff2-rev"), c(10, 5, 5, 5)),
           M = rep(12, 25), 
           rep = c(7, 19, 11, 3, 4, # m1 v m2
                   18,20,  8,13, 6, # m1 v m2 jtt
                   18,4, 15, 2, 20, # m2 v m1
                   17, 16, 20, 1, 12, # m1 v m3
                   9, 12, 17, 8, 4)) # m3 v m1

for_experiment_data$svgfilename <- make_interactive_lineups_final(dat = for_experiment_data)

for_experiment_data$svgfile <- svg_filenames
write.csv(for_experiment_data, "Lineup-Images/ForExperimentFinal/data_for_experiment.csv")


