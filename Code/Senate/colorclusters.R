# make an svg with only 1 plot
dat <- read_csv("~/Desktop/Dissertation/NetworksVizInference/SenateLineupData/trials/bigmod_1_1.csv")

dat4 <- filter(dat, ord == 4)

p <- ggplot(data = dat) + 
  geom_net(aes(from_id = from, to_id = to), 
           arrow = arrow(type = 'open', length = unit(2, "points") ), 
           linewidth = .25, singletons = T, fiteach = T, directed = T, 
           color = 'black', arrowgap = .015, arrowsize = .3, size = 1) + 
  theme_net() + 
  theme(panel.background = element_rect(color = 'black')) +
  facet_wrap(~ord)
make_interactive("testinggridsvg.svg", script = scriptURL)
ggplot(data = dat4) + 
  geom_net(aes(from_id = from, to_id = to), 
           arrow = arrow(type = 'open', length = unit(2, "points") ), 
           linewidth = .25, singletons = T, fiteach = T, directed = T, 
           color = 'black', arrowgap = .015, arrowsize = .3, size = 1) + 
  theme_net() + 
  theme(panel.background = element_rect(color = 'black')) +
  facet_wrap(~ord)
make_interactive("testinggridsvg4.svg", script = scriptURL)

# test coloring to add to the graphs

library(tidyverse)
library(igraph)
net <- read.csv("~/Downloads/simttbx3sd1392457revTRUE.csv", stringsAsFactors = FALSE)
df <- data.frame(expand.grid(
  from=paste0("V", 1:155), to = paste0("V", 1:155)), stringsAsFactors = FALSE)


netnest <- net %>% group_by(sim) %>% nest()
netnest <- netnest %>% mutate(
  data = data %>% purrr::map(.f = function(d) {
    adj1 <- d %>% right_join(df) %>% 
      xtabs(!is.na(dep.var.id)~from+to, data =.) %>% as.matrix()
    
    ig1 <- graph_from_adjacency_matrix(adj1)
    comps <- components(ig1)
    clu_df <- data.frame(from=names(comps$membership), clu = comps$membership)
    clu_df$csize=comps$csize[clu_df$clu]
    d %>% left_join(clu_df, by="from")
  })
)

net1 <- unnest(netnest, data)
nodes <- data.frame(id = rep(paste0("V", 1:155), length(unique(net1$sim))), 
                    sim = rep(unique(net1$sim), each = 155), 
                    ord = rep(unique(net1$ord), each = 155))
net1 <- merge(net1, nodes, by.x = c("from", "sim", "ord"), by.y = c("id", "sim", "ord"), all = T)

net1 %>% ggplot(aes(from_id=from, to_id=to, colour=log(csize))) + 
  geom_net(arrow = arrow(type = 'open', length = unit(2, "points") ), 
           linewidth = .25, singletons = T, fiteach = T, directed = T, 
           arrowgap = .015, arrowsize = .3, size = 1, layout.alg = "target") + 
  theme_net() +
  theme(panel.background = element_rect(color = 'black')) +
  scale_colour_gradient(low="#f768a1", high = "#49006a") + facet_wrap(~ord) +
  theme(legend.position="none")
