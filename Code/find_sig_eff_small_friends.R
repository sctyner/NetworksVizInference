# figure out how to loop through effects in RSiena
library(RSiena)
#looks like 20:35 are pretty well connected, go with those for now
fd2.w1 <- friend.data.w1[20:35,20:35]
fd2.w2 <- friend.data.w2[20:35,20:35]
fd2.w3 <- friend.data.w3[20:35,20:35]
# read in covariate data
drink <- as.matrix(read.table("Data/s50_data/s50-alcohol.dat"))
drink2 <- drink[20:35,]
# siena data
friendData2 <- array( c( fd2.w1, fd2.w2, fd2.w3 ),
                      dim = c( 16, 16, 3 ) )
# and next give this the role of the dependent variable:
friend2 <- sienaDependent(friendData2)
alcohol2 <- varCovar( as.matrix(drink2) )
# create siena data object
mysmalldata <- sienaDataCreate( friend2, alcohol2)
null_model_eff2 <- getEffects(mysmalldata)
includeEffects( null_model_eff2, "sameXTransTrip", interaction1 = "alcohol2", character = TRUE)
effectsDocumentation(null_model_eff2)
library(dplyr)
library(rvest)
RSeffects <- read_html("null_model_eff2.html") %>% html_nodes("table") %>% html_table()
RSeffects <- RSeffects[[1]]


effStructures <- NULL
for (i in 1:nrow(RSeffects)){
  effStructures[[i]] <- includeEffects( null_model_eff2, RSeffects$shortName[i], type = RSeffects$type[i], 
                  interaction1 = RSeffects$inter1[i], character = TRUE) 
}

# length(summary(effStructures[[3]])[[2]])
effcount <- rep(NA, length(effStructures))
for (i in 1:length(effStructures)){
  effcount[i] <- length(summary(effStructures[[i]])[[2]])
}

idx2test <- which(effcount == 5) #only test the ones with one added effect
n <- length(idx2test)
myalgorithm2 <- sienaAlgorithmCreate( projname = 's50' , n3 = 1000)
test_results <- data.frame(shortName = rep("",n), type = rep("",n), inter1 = rep("",n),
                           estimate = rep(0,n), se = rep(0,n), Waldpval = rep(0,n),
                           stringsAsFactors = FALSE)
for (i in 1:n){
  ests_test <- siena07( myalgorithm2, data = mysmalldata, returnDeps = TRUE, 
                        effects = effStructures[[idx2test[i]]], batch=TRUE, verbose = FALSE)
  if (is(try(Wald.RSiena(c(0,0,1), ests_test)), "try-error")){
    pval <- NA
  } else {
  wald_test <- Wald.RSiena(c(0,0,1), ests_test)
  pval <- wald_test$pvalue
  }
  test_results[i,] <- c(ests_test$effects$shortName[3], 
                        ests_test$effects$type[3],
                        ests_test$effects$interaction1[3],
                        ests_test$theta[3],
                        ests_test$se[3],
                        pval) 
  }
test_results[,4:6] = apply(test_results[,4:6], 2, function(x) as.numeric(as.character(x)))
write.csv(test_results, file = "effects_significance_smallFriends.csv") 

test_results %>% filter(Waldpval<.05) -> sig_eff_names
write.csv(sig_eff_names, "Data/sig_eff_names_smfriends.csv")

# library(ggplot2)
# ggplot(test_results %>% filter(Waldpval <.05)) +
#   geom_point(aes(x = estimate, y = -Waldpval, color = inter1)) +
#   geom_hline(yintercept = -0.05) + 
#   facet_wrap(~type)

null_model_smallFriends <- getEffects(mysmalldata)

test_results %>% filter(Waldpval < .05 & Waldpval > 0) -> sig_eff_names
write.csv(sig_eff_names, "Data/sig_eff_names.csv")

eff_models_smallFriends <- NULL
for (i in 1:nrow(sig_eff_names)){
  eff_models_smallFriends[[i]] <- includeEffects( null_model_eff2, sig_eff_names[i,1], 
                                                   type = sig_eff_names[i,2], 
                                                   interaction1 = sig_eff_names[i,3],
                                                  character=T)
}

runs_models_smallFriends <- NULL
for (i in 1:nrow(sig_eff_names)){
  runs_models_smallFriends[[i]] <- siena07( myalgorithm2, data = mysmalldata, returnDeps = TRUE, effects = eff_models_smallFriends[[i]], batch=TRUE, verbose = FALSE)
}

runs_models_smallFriends[[40]]


# create_small_lineup <- function(RSienaRes, num_plots, fiteach, sig_effs_row){
#   nets <- NULL
#   n <- num_plots
#   N <- length(RSienaRes$sims)
#   for (i in (N-n+1):N){
#     getnet <- merge(data.frame(RSienaRes$sims[[i]][[1]][[1]][[1]])[,-3], 
#                     data.frame(id = 1:16), by.x = "X1", by.y = "id",
#                     all = T)
#     getnet$count <- i 
#     nets <- rbind(nets, getnet)
#   }
#   actual2 <- merge(data.frame(as.edgelist(as.network(fd2.w2))), 
#                    data.frame(id = 1:16), 
#                    by.x = "X1", by.y = "id", all = T)
#   actual2$count <- "true_wave2"
#   nets <-rbind(nets,actual2)
#   ggplot(data = nets, aes(from_id = X1, to_id = X2)) +
#     geom_net(fiteach = fiteach, directed=TRUE, label = TRUE, labelcolour = 'red', size = 1, arrowsize = .5) + theme_net() +
#     labs(title = paste(sig_eff_names$shortName[sig_effs_row], sig_eff_names$type[sig_effs_row], sig_eff_names$inter1[sig_effs_row], "p-value =", round(sig_eff_names$Waldpval[sig_effs_row],7), sep = " ")) + 
#     facet_wrap(~count)
# }
# for (i in 1:40){
#   create_small_lineup(runs_models_smallFriends[[i]], 5, fiteach = TRUE, sig_effs_row =  i)
#   ggsave(paste("Code/Plots/", sig_eff_names[i,1],sig_eff_names[i,2],sig_eff_names[i,3], ".pdf",sep=""), width = 7.2, height = 4.5, units = "in")
#   }