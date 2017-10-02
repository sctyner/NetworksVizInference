# fit RSiena Models to Senate Data
library(tidyverse)
library(lubridate)
library(geomnet)
library(RSienaTest)
library(RSiena)
library(rvest)
library(netvizinf)
test_results <- read_csv("Data/senate/sigEffsSenate912.csv")

legisls <- read_csv("Data/senate/legislators_us.csv")
senateNet <- read_csv("Data/senate/senateNetgsw25.csv")
obamayears <- 2009:2016

senators <- unique(c(senateNet$source, senateNet$target)) 
senators <- as.character(na.omit(senators))


legisls %>% mutate(yearStart = year(start), yearEnd = year(end)) -> legisls

(legisls %>% 
    filter(mandate == "se" & (yearStart %in% obamayears | yearEnd %in% obamayears)) %>% 
    mutate(name1 = ifelse(is.na(middle), paste(first,last), paste(first,middle,last)),
           name = ifelse(is.na(suffix), name1, paste(name1, suffix))) %>% 
    select(name))[[1]] %>% unique %>% sort -> allsenators
startingsemat <- matrix(0, nrow = length(allsenators), ncol = length(allsenators))
rownames(startingsemat) <- allsenators
colnames(startingsemat) <- allsenators

make_se_adjmat <- function(se_no, adjmat=startingsemat, obamaSenates=seobama){
  senatedf <- obamaSenates %>% filter(senate == se_no & !is.na(target))
  N <- nrow(senatedf)
  for(i in 1:N){
    k <- which(rownames(adjmat) == senatedf$source[i])
    l <- which(colnames(adjmat) == senatedf$target[i])
    adjmat[senatedf$source[i], senatedf$target[i]] <- 1
  }
  return(adjmat)
}

se111adj <- make_se_adjmat(se_no = 111, obamaSenates = senateNet)
se112adj <- make_se_adjmat(se_no = 112, obamaSenates = senateNet)
se113adj <- make_se_adjmat(se_no = 113, obamaSenates = senateNet)
se114adj <- make_se_adjmat(se_no = 114, obamaSenates = senateNet)

remove_sens <- which(!(colnames(se111adj) %in% senators))

allSensAMempty <- startingsemat[-remove_sens, -remove_sens]
save(allSensAMempty, file = "Data/senate/allSensAdjMatEmpty.RDS")


se111adj <- se111adj[-remove_sens, -remove_sens]
se112adj <- se112adj[-remove_sens, -remove_sens]
se113adj <- se113adj[-remove_sens, -remove_sens]
se114adj <- se114adj[-remove_sens, -remove_sens]

senateCovars <- senateNet %>% 
  select(source,senate, n_au, party, sex) %>%
  unique %>% 
  spread(senate, n_au)

senateData <- array( c( se111adj, se112adj, se113adj, se114adj ),
                     dim = c( 155, 155, 4 ) )
senateData2 <- sienaDependent(senateData)
party <- coCovar(val = as.numeric(as.factor(senateCovars$party)))
# party 1 is dem 2 is ind 3 is rep
sex <- coCovar(val = as.numeric(as.factor(senateCovars$sex)))
# sex 1 is F 2 is M
nbills_au <- senateCovars[,4:7]
names(nbills_au) <- paste0("Sen",111:114) 
nbills_au <- as.matrix(nbills_au)
nbills_au[which(is.na(nbills_au))] <- 0
rownames(nbills_au) <- senateCovars$source
bills <- varCovar(nbills_au)
# rsiena objects are party, sex, bills, senateData2
senateSiena <- sienaDataCreate( senateData2, bills, sex, party )
save(senateSiena, file = "Data/senate/senateSienaNoHRC.rda")
senateModelEff <- getEffects(senateSiena)
#setwd("Data/senate")
#effectsDocumentation(senateModelEff)
#setwd("../..")
possibleSenateEffects <- read_html("Data/senate/senateModelEff.html") %>% html_node("table") %>% html_table()
SenateEffects2 <- possibleSenateEffects %>% filter(type %in% c("rate", "eval"))
SenateEffects2 %>% head

#senateSigEffs <- find_sig_effs(dat = senateSiena)
#write_csv(x = senateSigEffs, path = "data/congress/senateSignificantEffects.csv")

source('~/Desktop/RPackages/netvizinf/R/find-sig-effs2.R')
# find_sig_effs2 <- function(dat, N = 1000, effs){
#   require(RSiena)
#   require(dplyr)
#   require(tidyr)
#   require(purrr)
  dat <- senateSiena
  # skeleton effects structure (bare minimum in obj. fn.)
  eff_basic <- getEffects(dat)
  N <- 2000
  RSeffects <-SenateEffects2
  
  RSeffects <- RSeffects %>%
    nest(shortName:inter1) %>%
    mutate(eff_struct = map(data , .f = get_effects, eff_basic = eff_basic)) %>%
    mutate(num_eff = map(eff_struct, .f = function(x) length(summary(x)[[2]])))
  min_num_eff <- min(unlist(RSeffects$num_eff))
  idx2test <- which(RSeffects$num_eff > min_num_eff) #only test the ones with one added effect
  n <- length(idx2test)
  myalgorithm2 <- sienaAlgorithmCreate( projname = Sys.time() , n3 = N, firstg = .02)
  myalgorithm3 <- sienaAlgorithmCreate( projname = Sys.time() , n3 = N, firstg = .02, useStdInits = FALSE)
  #test_results <- data.frame(shortName = rep("",n), type = rep("",n), inter1 = rep("",n),
   #                          estimate = rep(0,n), se = rep(0,n), Waldpval = rep(0,n),
    #                         refits = rep(0,n), overallConverge = rep(0,n),
     #                        stringsAsFactors = FALSE)
# Run this for loop over night mon sep 11 start 11:43pm. N = 2000 
  # fuck my life R session aborted when it was nearly done and killed everything
  for (i in 90:n){
  #  ok -- everything 89 and after from 1:n is "unspecified interaction effect" and don't need to run.
    ests_test <- siena07( myalgorithm2, data = dat,
                          effects = RSeffects$eff_struct[[idx2test[i]]], batch=TRUE,
                          verbose = TRUE, silent = FALSE)
    convergeCheck <- ests_test$tconv.max
    redoCount <- 0
    
    while(convergeCheck > 0.25){
      ests_test <- siena07( myalgorithm3, data = dat, prevAns = ests_test, 
                            effects = RSeffects$eff_struct[[idx2test[i]]], batch=TRUE,
                            verbose = TRUE, silent = FALSE)
      convergeCheck <- ests_test$tconv.max
      redoCount <- redoCount + 1 
      if (is.na(convergeCheck)){
        break
      }
      if(redoCount == 5){
        break
      }
    }
    
    num_notrate <- length(ests_test$theta)
    test_vect <- rep(0,num_notrate)
    addlparm <- which(is.na(match(ests_test$effects$effectName, c("outdegree (density)", "reciprocity"))))
    test_vect[addlparm] <- 1
    if (is(try(RSienaTest::Wald.RSiena(test_vect, ests_test)), "try-error")){
      pval <- NA
    } else {
      wald_test <- RSienaTest::Wald.RSiena(test_vect, ests_test)
      pval <- wald_test$pvalue
    }
    test_results[i, ] <- c(shortName = ests_test$effects$shortName[num_notrate],
                          type = ests_test$effects$type[num_notrate],
                          inter1 = ests_test$effects$interaction1[num_notrate],
                          estimate = ests_test$theta[num_notrate],
                          se = sqrt(ests_test$covtheta[num_notrate, num_notrate]),
                          Waldpval = pval, 
                          refits = redoCount, 
                          overallConverge = convergeCheck)
    write_csv(test_results, "Data/senate/sigEffsSenate912.csv")
    
  }
  
  return(test_results)
  
}

write_csv(test_results, "Data/senate/sigEffsSenate911.csv")


restest <- find_sig_effs2(dat = senateSiena, N = 10, effs = SenateEffects2[1:10,])

