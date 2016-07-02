# merge the lineup pilot study info with the lus_ests_truth
load("~/Desktop/NetworksResearch/NetworksVizInference/Data/lus_ests_truth.rda")
# lus_ests_truth$convergence <- NA
# 
# for (i in 1:nrow(lus_ests_truth)){
#   if (is.na(lus_ests_truth$converg_stat[i])){
#     lus_ests_truth$convergence[i] <- NA
#   } else if (lus_ests_truth$param_name[i] != "rate"){
#     if (lus_ests_truth$converg_stat[i] <= 0.1 & lus_ests_truth$converg_stat[i] >= -.1){
#       lus_ests_truth$convergence[i] <- "Converged"
#     } else{  lus_ests_truth$convergence[i] <- "Did not converge"} 
#   } else {
#     if (lus_ests_truth$converg_stat[i] <= 0.3){
#       lus_ests_truth$convergence[i] <- "Converged"
#     } else { lus_ests_truth$convergence[i] <- "Did not converge" }
#   }
# }
# 
# 


names(lus_ests_truth)
summary(lus_ests_truth$convergence)


ggexpdat <- read.csv("~/Desktop/NetworksResearch/NetworksVizInference/GGExperimentApr28/data_for_experiment.csv")
ggexpres <- readxl::read_excel("~/Desktop/NetworksResearch/NetworksVizInference/GGExperimentApr28/responses_GGExpApr28.xlsx")
head(ggres)
library(dplyr)
ggexp_datres <- merge(ggexpres, ggexpdat[,-c(4,7)], by.x = "Lineup", by.y = "pres_order")
head(ggexp_datres)
ggexp_datres <-ggexp_datres %>% filter(Reasoning != 'SAM')
names(ggexp_datres)
ggexp_datres <- ggexp_datres[,-11]
head(lus_ests_truth)
# lus ests truth:  lineupid   lineupname M rep model
# ggexp_datres: lineupname  M rep 
head(ggexp_datres)

lus_truth_expres <- left_join(lus_ests_truth, ggexp_datres, by = c("lineupname" = "lineupname", "M" = "M", "rep" = "rep", "panel_num" = "ChosenLU"))
save(lus_truth_expres, file = "~/Desktop/NetworksResearch/NetworksVizInference/Data/lus_truth_expres.RDA")
