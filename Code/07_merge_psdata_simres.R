# merge the lineup pilot study info with the lus_ests_truth

load("~/Desktop/NetworksResearch/NetworksVizInference/Data/lus_ests_truth.rda")
lus_ests_truth$convergence <- NA

for (i in 1:nrow(lus_ests_truth)){
  if (is.na(lus_ests_truth$converg_stat[i])){
    lus_ests_truth$convergence[i] <- NA
  } else if (lus_ests_truth$param_name[i] != "rate"){
    if (lus_ests_truth$converg_stat[i] <= 0.1 & lus_ests_truth$converg_stat[i] >= -.1){
      lus_ests_truth$convergence[i] <- "Converged"
    } else{  lus_ests_truth$convergence[i] <- "Did not converge"} 
  } else {
    if (lus_ests_truth$converg_stat[i] <= 0.3){
      lus_ests_truth$convergence[i] <- "Converged"
    } else { lus_ests_truth$convergence[i] <- "Did not converge" }
  }
}




names(lus_ests_truth)
summary(lus_ests_truth$convergence)


# read.csv("~/Desktop/NetworksResearch/NetworksVizInference/GGExperimentApr28/data_for_experiment.csv")
readxl::read_excel("~/Desktop/NetworksResearch/NetworksVizInference/GGExperimentApr28/responses_GGExpApr28.xlsx")
