library(magrittr)
library(dplyr)
dir <- "Data/lineupdata/"
lineupdata <- dir(dir, pattern="small")

djtt <- function(X1, X2) {
  data <- data.frame(X1=X1, X2=X2)
  data <- na.omit(data)
  if (nrow(data) == 0) return(0)
  geomnet::jtt(data, from_id="X1", to_id="X2")
}


dframe <- lineupdata %>% purrr::map_df(.f = function(x) {
  net = read.csv(file.path(dir=dir, x))
  net %>% group_by(plot_order) %>% summarize(
    jtt = djtt(X1, X2))
}, .id="filename")

dframe <- data.frame(dframe)
dframe$filename <- lineupdata[as.numeric(dframe$filename)]

dframe$m <- gsub(".*-m-([0-9]*)-.*", "\\1", dframe$filename)
dframe$rep <- gsub(".*-rep-([0-9]*).csv", "\\1", dframe$filename)
dframe$model <- gsub("(.*)-m.*", "\\1", dframe$filename)

library(ggplot2)
qplot(data=subset(dframe, model=="smallfriends"), 
      x=plot_order, y=jtt, colour=factor(rep)) + facet_wrap(~filename)

qplot(data=subset(dframe, model=="smallfriends-rev"), 
      x=plot_order, y=jtt, colour=factor(rep)) + facet_wrap(~filename)


write.csv(dframe, file="Data/lineups-jtt.csv", row.names=FALSE)