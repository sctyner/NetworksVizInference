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


