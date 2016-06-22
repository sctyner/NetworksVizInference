# email lineups
# do some stratified scrambling.
# sramble only WITHIN department. 
# H_0: within a department, all emails are random
# H_1: "", there is a strucutre. 

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



library(geomnet)
data("email")

head(email$nodes)

email_net <- merge(subset(email$edges, nrecipients < 54), email$nodes, by.x = "From", by.y = "label", all = T)

# create lineups in different sizes
emailm3 <- create_net_lineup(email_net, to_id = "to", m =3)
emailm6 <- create_net_lineup(email_net, to_id = "to", m =6)
emailm9 <- create_net_lineup(email_net, to_id = "to", m =9)

# we need to set the seed to make the examples reproducible.
# One way to get a replicate for graphs might be to have some lineups with the same data but different seeds
ggplot(data = emailm3$lineup_data, aes(from_id = From, to_id = to)) + 
  geom_net(ealpha = .5, fiteach = TRUE, colour = 'black') + theme_net() + facet_wrap(~group)

ggplot(data = emailm6$lineup_data, aes(from_id = From, to_id = to)) + 
  geom_net(ealpha = .5, fiteach = TRUE, colour = 'black') + theme_net() + facet_wrap(~group)

ggplot(data = emailm9$lineup_data, aes(from_id = From, to_id = to)) + 
  geom_net(ealpha = .5, fiteach = TRUE, colour = 'black') + theme_net() + facet_wrap(~group)



