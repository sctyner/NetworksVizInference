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



# data scrambling

library(geomnet)
data("football")

head(football$edges)
football_net <- merge(football$edges, football$vertices, by.x = "from", by.y = "label", all = T)
head(football_net)

# IS there a way to cluster the points in the shuffled graph using the x,y coordinates
# of the points after they are on the graph

# find random graph model with structure of clusters. 
# params - number of clusters, between cluster connectedness, within cluster connectivity,
# number of nodes, degree of connectectedness should be the same for every node

#ggplot(data = football_net, aes(from_id = from, to_id = to)) + 
#  geom_net() + theme_net()

football_net_scrambled1 <- football_net
football_net_scrambled1$to <- football_net_scrambled1$to[sample.int(nrow(football_net))]
  
football_net_scrambled2 <- football_net
football_net_scrambled2$to <- football_net_scrambled2$to[sample.int(nrow(football_net))]


football_net_lineup <- rbind(football_net, football_net_scrambled1, football_net_scrambled2)

football_net_lineup$group <- rep(c("Data", "Null1", "Null2"), each = nrow(football_net))

ggplot(data = football_net_lineup, aes(from_id = from, to_id = to)) + 
  geom_net(ealpha = .5, fiteach = TRUE, colour = 'black') + theme_net() + facet_wrap(~group, nrow=1)

create_net_lineup <- function(net_data, to_id, m){
  N <- nrow(net_data)
  nullplots <- NULL
  for(i in 1:(m-1)){
    nullplots[[i]] <- net_data
    nullplots[[i]][,to_id] <- nullplots[[i]][sample.int(N),to_id]
  }
  nullplots <- plyr::ldply(nullplots)
  lineup_data <- rbind(net_data, nullplots)
  lineup_data$group <- rep(sample.int(m), each=N)
  return(list(lineup_data = lineup_data, data_plot = lineup_data$group[1]))
}

footballm3 <- create_net_lineup(football_net, to_id = "to", m = 3)
p1 <- ggplot(data = footballm3$lineup_data, aes(from_id = from, to_id = to)) + 
  geom_net(ealpha = .5, fiteach = TRUE, colour = 'black') + theme_net() + facet_wrap(~group) +
  theme(panel.background = element_rect(fill="white"))

footballdata <- footballm3$lineup_data
footballdata$data_plot <- footballm3$data_plot
#dir.create(path = "Data/lineupdata")
write.csv(footballdata, "Data/lineupdata/football-m-3-rep-1.csv", row.names=FALSE)
file <- 1
#dir.create(path = "lineups")
#dir.create(path = "lineups/pdfs")
ggsave(p1, file=sprintf("lineups/pdfs/football-m-3-rep-%s.pdf",file))
#dir.create(path = "lineups/svgs")
tmpfile <- sprintf("%s.svg",tempfile(tmpdir="lineups/svgs"))
print(p1)
make_interactive(filename= tmpfile, script=scriptURL,  
                 high="#d5d5d5",  background="#ffffff")
add_data("lineup-details.csv",
         sample_size=nrow(footballdata),
         test_param=sprintf("football-m-3-rep-%d", file), 
         param_value=sprintf("football-m-3-rep-%d", file), 
         p_value=NA,
         obs_plot_location=footballm3$data_plot,
         pic_name=tmpfile,
         data_name="lineupdata/football-m-3-rep-1.csv", 
         experiment="turk21", 
         difficulty="football-m-3-rep-1"
)



footballm6 <- create_net_lineup(football_net, to_id = "to", m = 6)
ggplot(data = footballm6$lineup_data, aes(from_id = from, to_id = to)) + 
  geom_net(ealpha = .5, fiteach = TRUE, colour = 'black') + theme_net() + facet_wrap(~group)

footballm9 <- create_net_lineup(football_net, to_id = "to", m = 9)
ggplot(data = footballm9$lineup_data, aes(from_id = from, to_id = to)) + 
  geom_net(ealpha = .5, fiteach = TRUE, colour = 'black') + theme_net() + facet_wrap(~group)

footballm12 <- create_net_lineup(football_net, to_id = "to", m = 12)
ggplot(data = footballm12$lineup_data, aes(from_id = from, to_id = to)) + 
  geom_net(ealpha = .5, fiteach = TRUE, colour = 'black') + theme_net() + facet_wrap(~group)

footballm15 <- create_net_lineup(football_net, to_id = "to", m = 15)
ggplot(data = footballm15$lineup_data, aes(from_id = from, to_id = to)) + 
  geom_net(ealpha = .5, fiteach = TRUE, colour = 'black') + theme_net() + facet_wrap(~group, nrow = 3)

footballm18 <- create_net_lineup(football_net, to_id = "to", m = 18)
ggplot(data = footballm18$lineup_data, aes(from_id = from, to_id = to)) + 
  geom_net(ealpha = .5, fiteach = TRUE, colour = 'black') + theme_net() + facet_wrap(~group, nrow = 3)

