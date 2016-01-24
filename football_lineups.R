# data scrambling

library(geomnet)
data("football")

head(football$edges)
football_net <- merge(football$edges, football$vertices, by.x = "from", by.y = "label", all = T)
head(football_net)

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


library(plyr)
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

library(magrittr)
footballm3 <- create_net_lineup(football_net, to_id = "to", m = 3)
ggplot(data = footballm3$lineup_data, aes(from_id = from, to_id = to)) + 
  geom_net(ealpha = .5, fiteach = TRUE, colour = 'black') + theme_net() + facet_wrap(~group)

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

