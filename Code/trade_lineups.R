# Olga's data. 

trade_net <- readRDS("Data/trade_net.rds")

library(geomnet)

tradem3 <- create_net_lineup(trade_net, to_id = "cname2", m =3)
ggplot(data = tradem3$lineup_data, aes(from_id = cname1, to_id = cname2)) + 
  geom_net(ealpha = .5, fiteach = TRUE, colour = 'black') + theme_net() + facet_wrap(~group)

tradem6 <- create_net_lineup(trade_net, to_id = "cname2", m =6)
ggplot(data = tradem6$lineup_data, aes(from_id = cname1, to_id = cname2)) + 
  geom_net(ealpha = .5, fiteach = TRUE, colour = 'black') + theme_net() + facet_wrap(~group)

tradem9 <- create_net_lineup(trade_net, to_id = "cname2", m =9)
ggplot(data = tradem9$lineup_data, aes(from_id = cname1, to_id = cname2)) + 
  geom_net(ealpha = .5, fiteach = TRUE, colour = 'black') + theme_net() + facet_wrap(~group)

tradem12 <- create_net_lineup(trade_net, to_id = "cname2", m =12)
ggplot(data = tradem12$lineup_data, aes(from_id = cname1, to_id = cname2)) + 
  geom_net(ealpha = .5, fiteach = TRUE, colour = 'black') + theme_net() + facet_wrap(~group)


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