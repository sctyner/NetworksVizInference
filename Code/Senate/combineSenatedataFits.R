# combine data from server
library(tidyverse)
M1sen1 <- read_csv("Data/senate/basicModelFitsSenate.csv")
M1sen2 <- read_csv("Data/senate/basicModelFitsSenate2.csv")
M1sen3 <- read_csv("Data/senate/basicModelFitsSenate3.csv")
M1sen4 <- read_csv("Data/senate/basicModelFitsSenate4.csv")
M1sen4 <- M1sen4[,-1]
M1sen5 <- read_csv("Data/senate/basicModelFitsSenate5.csv")
M1sen6 <- read_csv("Data/senate/basicModelFitsSenate6.csv")
M1sen6 <- M1sen6[,-1]
M1sen7 <- read_csv("Data/senate/basicModelFitsSenate7.csv")
M1sen8 <- read_csv("Data/senate/basicModelFitsSenate8.csv")
M1sen8 <- M1sen8[,-1]
M1sen9 <- read_csv("Data/senate/basicModelFitsSenate9.csv")
M1sen10 <- read_csv("Data/senate/basicModelFitsSenate10.csv")
M1sen10 <- M1sen10[,-1]

allM1sen <- do.call(rbind, list(M1sen1,M1sen2, M1sen3, M1sen4, M1sen5, M1sen6,
                    M1sen7, M1sen8, M1sen9, M1sen10))
allM1sen$sim <- 1:nrow(allM1sen)
#write_csv(allM1sen, "Data/senate/basicModelFitsSenate1000.csv")

inisdsen1 <- read_csv("Data/senate/inisdModelFitsSenate1-10.csv")
inisdsen1.2 <- read_csv("Data/senate/inisdModelFitsSenate111-100.csv")
inisdsen2 <- read_csv("Data/senate/inisdModelFitsSenate200.csv")
inisdsen3 <- read_csv("Data/senate/inisdModelFitsSenate300.csv")
inisdsen4 <- read_csv("Data/senate/inisdModelFitsSenate400.csv")
inisdsen5 <- read_csv("Data/senate/inisdModelFitsSenate500.csv")
inisdsen6 <- read_csv("Data/senate/inisdModelFitsSenate600.csv")
inisdsen7 <- read_csv("Data/senate/inisdModelFitsSenate700.csv")
inisdsen8 <- read_csv("Data/senate/inisdModelFitsSenate800.csv")
inisdsen9 <- read_csv("Data/senate/inisdModelFitsSenate900.csv")
inisdsen10 <- read_csv("Data/senate/inisdModelFitsSenate1000.csv")

allinisdsen <- do.call(rbind, list(inisdsen1, inisdsen1.2, inisdsen2, inisdsen3, inisdsen4,
                                   inisdsen5, inisdsen6, inisdsen7, inisdsen8, inisdsen9, inisdsen10))
allinisdsen <- allinisdsen[,-1]
allinisdsen$sim <- 1:nrow(allinisdsen)
# write_csv(allinisdsen, "Data/senate/inisdModelFitsSenate1000.csv")

jttpSen1 <- read_csv("Data/senate/jttpModelFitsSenate100.csv")
jttpSen2 <- read_csv("Data/senate/jttpModelFitsSenate200.csv")
jttpSen3 <- read_csv("Data/senate/jttpModelFitsSenate300.csv")
jttpSen4 <- read_csv("Data/senate/jttpModelFitsSenate400.csv")
jttpSen5 <- read_csv("Data/senate/jttpModelFitsSenate500.csv")
jttpSen6 <- read_csv("Data/senate/jttpModelFitsSenate600.csv")
jttpSen7 <- read_csv("Data/senate/jttpModelFitsSenate700.csv")
jttpSen8 <- read_csv("Data/senate/jttpModelFitsSenate800.csv")
jttpSen9 <- read_csv("Data/senate/jttpModelFitsSenate900.csv")
jttpSen10 <- read_csv("Data/senate/jttpModelFitsSenate1000.csv")

alljttpsen <- do.call(rbind, list(jttpSen1, jttpSen2, jttpSen3, jttpSen4,
                      jttpSen5,jttpSen6,jttpSen7,jttpSen8,jttpSen9,jttpSen10))
names(alljttpsen)[1] <- "sim"
alljttpsen$sim <- 1:nrow(alljttpsen)
#write_csv(alljttpsen, "Data/senate/jttpModelFitsSenateAll1000.csv")

alljttpsen %>% mutate(converged = maxConv <= 0.25) %>% 
ggplot() + 
  geom_density(aes(x = jumpXTransTrip, color = converged))

alljttpsen %>% mutate(lower = jumpXTransTrip - 1.96*se_jumpXTransTrip, 
                   upper = jumpXTransTrip + 1.96*se_jumpXTransTrip,
                   issig = !(0 < upper & 0 > lower)) %>% 
ggplot() + geom_histogram(aes(jumpXTransTrip, fill = issig), position = "fill")
  
# jtts 

jttsSen1 <- read_csv("Data/senate/jttsModelFitsSenate100.csv")
jttsSen2 <- read_csv("Data/senate/jttsModelFitsSenate200.csv")
jttsSen3 <- read_csv("Data/senate/jttsModelFitsSenate300.csv")
jttsSen4 <- read_csv("Data/senate/jttsModelFitsSenate400.csv")
jttsSen5 <- read_csv("Data/senate/jttsModelFitsSenate500.csv")
jttsSen6 <- read_csv("Data/senate/jttsModelFitsSenate600.csv")
jttsSen7 <- read_csv("Data/senate/jttsModelFitsSenate700.csv")
jttsSen8 <- read_csv("Data/senate/jttsModelFitsSenate800.csv")
jttsSen9 <- read_csv("Data/senate/jttsModelFitsSenate900.csv")
jttsSen10 <- read_csv("Data/senate/jttsModelFitsSenate1000.csv")

alljttssen <- do.call(rbind, list(jttsSen1, jttsSen2, jttsSen3, jttsSen4,
                                  jttsSen5,jttsSen6,jttsSen7,jttsSen8,jttsSen9,jttsSen10))
names(alljttssen)[1] <- "sim"
alljttssen$sim <- 1:nrow(alljttssen)
#write_csv(alljttssen, "Data/senate/jttsModelFitsSenateAll1000.csv")
