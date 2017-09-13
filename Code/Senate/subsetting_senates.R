# senate data
library(network)
library(tidyverse)
library(geomnet)

load("Data/senate/senate111.rda")
load("Data/senate/senate112.rda")
load("Data/senate/senate113.rda")
load("Data/senate/senate114.rda")
senators111 <- read_csv("Data/senate/senators111.csv")

sens252


se111df <- fortify_senate(net_us_se111, val = 0)
se111df25 <- fortify_senate(net_us_se111, val = .25)

se111df25noHRC <- filter(se111df25, target != "Hillary Rodham Clinton" & source != "Hillary Rodham Clinton") 

se111df25noHRC2 <- merge(se111df25noHRC, filter(senators111, vertex.names != "Hillary Rodham Clinton") , 
      by.x= c("source",intersect(names(senators111), names(se111df25noHRC))), 
      by.y = c("vertex.names",intersect(names(senators111), names(se111df25noHRC))), all = T)


missingSource <- unique(se111df25noHRC$target)[-which(unique(se111df25noHRC$target) %in% unique(se111df25noHRC$source))]


net_us_se111v2 <- net_us_se111 
delete.vertices(net_us_se111v2, which(net_us_se111v2 %v% "vertex.names" == "Hillary Rodham Clinton"))
se111dfnoHRC <- fortify_senate(net_us_se111v2, val = 0)
se111dfnoHRC25 <- fortify_senate(net_us_se111v2, val = .25)
se111dfnoHRC25 %>% filter(gsw > .25)


fortify_senate <- function(net, var = "gsw", val){
  net2 <- net 
  delete.edges(net2, which(net2 %e% var < val))
  delete.edges(net2, which(net2 %e% var < val))
  vertex.colnames <- network::list.vertex.attributes(net2)
  edge.colnames <- network::list.edge.attributes(net2)
  N <- network::network.size(net2)
  P <- length(vertex.colnames)
  N2 <- nrow(network::as.edgelist(net2))
  P2 <- length(edge.colnames)
  node.data <- data.frame(matrix("", nrow = N, ncol = P), 
                          stringsAsFactors = F)
  names(node.data) <- vertex.colnames
  for (i in 1:P) {
    node.data[, i] <- network::get.vertex.attribute(net2, vertex.colnames[i])
  }
  edge.data <- data.frame(matrix("", nrow = N2, ncol = P2), 
                          stringsAsFactors = F)
  names(edge.data) <- edge.colnames
  for (i in 1:P2) {
    edge.data[, i] <- network::get.edge.attribute(net2, edge.colnames[i])
  }
  edge.data <- edge.data %>% select(-na)
  node.data <- node.data %>% select(-c(district, na))
  
  # return
  merge(edge.data, node.data, by.x = "source", by.y="vertex.names",all = T)
}

combine_senates <- function(senates, var2, val2){
  if (!is.list(senates)){
    stop("Error! senates needs to be a list of networks")
  } 
  if (!network::is.network(senates[[1]])){
    stop("error! senates needs to be a list of objects of class network")
  }
  
  M <- length(senates)
  senates2 <- list() 
  for (i in 1:M){
    mysen <- fortify_senate(senates[[i]], var = var2, val = val2)
    yr <- network::get.network.attribute(senates[[i]], "congress")
    mysen$senate <- yr
    senates2[[i]] <- mysen 
  }
  return(bind_rows(senates2))
}

sens <- list(net_us_se111, net_us_se112, net_us_se113, net_us_se114)
net_us_se111.2 <- net_us_se111
net_us_se112.2 <- net_us_se112
net_us_se113.2 <- net_us_se113
net_us_se114.2 <- net_us_se114
delete.vertices(net_us_se111.2, which(get.vertex.attribute(net_us_se111.2, "vertex.names") == "Hillary Rodham Clinton"))
delete.vertices(net_us_se112.2, which(get.vertex.attribute(net_us_se112.2, "vertex.names") == "Hillary Rodham Clinton"))
delete.vertices(net_us_se113.2, which(get.vertex.attribute(net_us_se113.2, "vertex.names") == "Hillary Rodham Clinton"))
delete.vertices(net_us_se114.2, which(get.vertex.attribute(net_us_se114.2, "vertex.names") == "Hillary Rodham Clinton"))

sens2 <- list(net_us_se111.2, net_us_se112.2, net_us_se113.2, net_us_se114.2)

sens25 <- combine_senates(senates = sens, var2 = "gsw", val2 = .25)
sens252 <- combine_senates(senates = sens2[2:4], var2 = "gsw", val2 = .25)

sens28 <- combine_senates(senates = sens, var2 = "gsw", val2 = .28)
sens282 <- combine_senates(senates = sens2, var2 = "gsw", val2 = .28)

sens30 <- combine_senates(senates = sens, var2 = "gsw", val2 = .30)
sens35 <- combine_senates(senates = sens, var2 = "gsw", val2 = .35)
sens40 <- combine_senates(senates = sens, var2 = "gsw", val2 = .40)
sens45 <- combine_senates(senates = sens, var2 = "gsw", val2 = .45)
sens50 <- combine_senates(senates = sens, var2 = "gsw", val2 = .50)


ggplot(data = se111df25noHRC2) + 
  geom_net(directed = T, arrowsize = .25, singletons= F, fiteach = T, linewidth = .25, layout.alg = 'fruchtermanreingold', fontsize = 2, repel=T,
           aes(from_id = source, to_id = target, color = party),
           size = 1) + 
  theme_net() + 
  scale_color_manual(values = c("royalblue", "forestgreen","firebrick")) + 
  theme(legend.position = 'bottom') 

# combine no hrc 111 with rest of years 

se111df25noHRC2$senate <- 111
sens252 <- rbind(se111df25noHRC2[,names(sens252)], sens252)

#write_csv(sens252, "Data/senate/senateNetgsw25.csv")

set.seed(56049382)
ggplot(data = sens252) + 
  geom_net(directed = T, arrowsize = .25, singletons= F, fiteach = T, linewidth = .25, layout.alg = 'fruchtermanreingold', fontsize = 2, repel=T,
           aes(from_id = source, to_id = target, color = party),
           size = 1) + 
  theme_net() + 
  scale_color_manual(values = c("royalblue", "forestgreen","firebrick")) + 
  theme(legend.position = 'bottom') + 
  facet_wrap(~senate, nrow = 2, labeller = "label_both") + 
  xlim(c(0,1.05)) + 
  ylim(c(0,1.05)) + 
  ggtitle("GSW > .25")
ggplot(data = sens30) + 
  geom_net(directed = T, arrowsize = .25, singletons= T, fiteach = T, linewidth = .25, layout.alg = 'fruchtermanreingold', fontsize = 2, repel=T,
           aes(from_id = source, to_id = target, color = party),
           size = 1) + 
  theme_net() + 
  scale_color_manual(values = c("royalblue", "forestgreen","firebrick")) + 
  theme(legend.position = 'bottom') + 
  facet_wrap(~senate, nrow = 2, labeller = "label_both") + 
  xlim(c(0,1.05)) + 
  ylim(c(0,1.05)) + 
  ggtitle("GSW > .30")
ggplot(data = sens35) + 
  geom_net(directed = T, arrowsize = .25, singletons= T, fiteach = T, linewidth = .25, layout.alg = 'fruchtermanreingold', fontsize = 2, repel=T,
           aes(from_id = source, to_id = target, color = party),
           size = 1) + 
  theme_net() + 
  scale_color_manual(values = c("royalblue", "forestgreen","firebrick")) + 
  theme(legend.position = 'bottom') + 
  facet_wrap(~senate, nrow = 2, labeller = "label_both") + 
  xlim(c(0,1.05)) + 
  ylim(c(0,1.05)) + 
  ggtitle("GSW > .35")

ggplot(data = sens40) + 
  geom_net(directed = T, arrowsize = .25, singletons= T, fiteach = T, linewidth = .25, layout.alg = 'fruchtermanreingold', fontsize = 2, repel=T,
           aes(from_id = source, to_id = target, color = party),
           size = 1) + 
  theme_net() + 
  scale_color_manual(values = c("royalblue", "forestgreen","firebrick")) + 
  theme(legend.position = 'bottom') + 
  facet_wrap(~senate, nrow = 2, labeller = "label_both") + 
  xlim(c(0,1.05)) + 
  ylim(c(0,1.05)) + 
  ggtitle("GSW > .40")

ggplot(data = sens45) + 
  geom_net(directed = T, labelon = T, arrowsize = .25, singletons= T, fiteach = T, linewidth = .25, layout.alg = 'fruchtermanreingold', fontsize = 2, repel=T,
           aes(from_id = source, to_id = target, color = party),
           size = 1) + 
  theme_net() + 
  scale_color_manual(values = c("royalblue", "forestgreen","firebrick")) + 
  theme(legend.position = 'bottom') + 
  facet_wrap(~senate, nrow = 2, labeller = "label_both") + 
  xlim(c(0,1.05)) + 
  ylim(c(0,1.05)) + 
  ggtitle("GSW > .45")

ggplot(data = sens50) + 
  geom_net(directed = T, arrowsize = .25, singletons= T, fiteach = T, linewidth = .25, layout.alg = 'fruchtermanreingold', fontsize = 2, repel=T,
           aes(from_id = source, to_id = target, color = party),
           size = 1) + 
  theme_net() + 
  scale_color_manual(values = c("royalblue", "forestgreen","firebrick")) + 
  theme(legend.position = 'bottom') + 
  facet_wrap(~senate, nrow = 2, labeller = "label_both") + 
  xlim(c(0,1.05)) + 
  ylim(c(0,1.05)) + 
  ggtitle("GSW > .50")
ggplot(data = sens28) + 
  geom_net(directed = T, arrowsize = .25, singletons= T, fiteach = T, linewidth = .25, layout.alg = 'fruchtermanreingold', fontsize = 2, repel=T,
           aes(from_id = source, to_id = target, color = party),
           size = 1) + 
  theme_net() + 
  scale_color_manual(values = c("royalblue", "forestgreen","firebrick")) + 
  theme(legend.position = 'bottom') + 
  facet_wrap(~senate, nrow = 2, labeller = "label_both") + 
  xlim(c(0,1.05)) + 
  ylim(c(0,1.05)) + 
  ggtitle("GSW > .28")
