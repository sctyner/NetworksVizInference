# make lineups
library(tidyverse)
library(netvizinf)
library(geomnet)
load("Data/senate/FauxTrueData/M_1_sims_for_fdata.RDS")
basicsims <- read_csv("Data/senate/FauxTrueData/M1senSimDat.csv")
jttpsims <- read_csv("Data/senate/FauxTrueData/jttpSenSimDat.csv")
jttssims <- read_csv("Data/senate/FauxTrueData/jttsSenSimDat.csv")
samepsims <- read_csv("Data/senate/FauxTrueData/samepSenSimDat.csv")
simttbsims <- read_csv("Data/senate/FauxTrueData/simttbSenSimDat.csv")
samettpsims <- read_csv("Data/senate/FauxTrueData/samettpSenSimDat.csv")
basicsims$model <- "basic"
jttpsims$model <- "jttp"
jttssims$model <- "jtts"
samepsims$model <- "samep"
simttbsims$model <- "simttb"
samettpsims$model <- "samettp"
datSims <- bind_rows(basicsims, jttpsims, jttssims, samepsims, simttbsims, samettpsims)

# write_csv(datSims, "Data/senate/FauxTrueData/alldatasims.csv")

head(basicsims)
basicsims %>% filter(wave == 1, sim == 1) %>% mutate(sim = 1001)

gotsims %>% filter(model == "basic") %>%
  mutate(simsdf = map(sims, sims_to_df)) -> testsims
(testsims %>% select(simsdf))[[1]][[1]] %>% filter(wave == 1, sim %in% 1:5) -> for_lu

bind_rows(for_lu, basicsims %>% filter(wave == 1, sim == 1) %>% mutate(sim = 1001)) %>% 
  mutate(from = paste0("V", from), to = ifelse(is.na(to), to, paste0("V", to))) -> for_lu
for_lu$ord <- rep(sample(6), as.numeric(table(for_lu$sim)))
  
ggplot(data = for_lu) + 
  geom_net(aes(from_id = from, to_id = to), fiteach = T, directed = T, color = 'black', arrowgap = .01, arrowsize = .3, size = 2) + 
  theme_net() + 
  facet_wrap(~ord)

jttpsims %>% filter(wave == 1, sim == 30) %>% mutate(sim = 1001)
gotsims %>% filter(model == "jttp" & condition == "double") %>%
  mutate(simsdf = map(sims, sims_to_df)) -> testsims2
(testsims2 %>% select(simsdf))[[1]][[1]] %>% filter(wave == 1, sim %in% c(4,50,10,2,34)) -> for_lu2


bind_rows(for_lu2, jttpsims %>% filter(wave == 1, sim == 30) %>% mutate(sim = 1001)) %>% 
  mutate(from = paste0("V", from), to = ifelse(is.na(to), to, paste0("V", to))) -> for_lu2
for_lu2$ord <- rep(sample(6), as.numeric(table(for_lu2$sim)))

ggplot(data = for_lu2) + 
  geom_net(aes(from_id = from, to_id = to), fiteach = T, directed = T, color = 'black', arrowgap = .01, arrowsize = .3, size = 2) + 
  theme_net() + 
  facet_wrap(~ord)
table(for_lu2$sim, for_lu2$ord)

gotsims %>% filter(model == "jttp" & condition == "opposite") %>%
  mutate(simsdf = map(sims, sims_to_df)) -> testsims3
(testsims3 %>% select(simsdf))[[1]][[1]] %>% filter(wave == 1, sim %in% c(34,58,67,24,99)) -> for_lu3

bind_rows(for_lu3, jttpsims %>% filter(wave == 1, sim == 34) %>% mutate(sim = 1001)) %>% 
  mutate(from = paste0("V", from), to = ifelse(is.na(to), to, paste0("V", to))) -> for_lu3
for_lu3$ord <- rep(sample(6), as.numeric(table(for_lu3$sim)))
ggplot(data = for_lu3) + 
  geom_net(aes(from_id = from, to_id = to), singletons = T, fiteach = T, directed = T, color = 'black', arrowgap = .01, arrowsize = .3, size = 2) + 
  theme_net() + 
  facet_wrap(~ord)
table(for_lu3$sim, for_lu3$ord)


senate_lu <- function(altsims, nullsims, M, seed, w = 1, mod, cond){
  set.seed(seed)
  nullidx <- sample(unique(nullsims$sim), 1)
  nulldat <- nullsims %>% filter(model == mod, wave == w, sim == nullidx) %>% mutate(sim = 1001)
  
  altsims %>% filter(model == mod & condition == cond) %>%
    mutate(simsdf = map(sims, netvizinf::sims_to_df)) -> altsims
  (altsims %>% select(simsdf))[[1]][[1]] -> altsims
  samp_from <- unique(altsims$sim)
  sampidx <- sample(samp_from, M-1)
  altsims %>% filter(wave == w, sim %in% sampidx) -> altsims
 
  bind_rows(altsims, nulldat) %>% 
    mutate(from = paste0("V", from), to = ifelse(is.na(to), to, paste0("V", to))) -> for_lu
  for_lu$ord <- rep(sample(M), as.numeric(table(for_lu$sim)))
  
  ans <- which(table(for_lu$sim, for_lu$ord)[M,] != 0)
  p <- ggplot(data = for_lu) + 
    geom_net(aes(from_id = from, to_id = to), 
             arrow = arrow(type = 'open', length = unit(2, "points") ), 
             linewidth = .25, singletons = T, fiteach = T, directed = T, 
             color = 'black', arrowgap = .015, arrowsize = .3, size = 1) + 
    theme_net() + 
    theme(panel.background = element_rect(color = 'black')) +
    facet_wrap(~ord)
  
  return(list(plot = p, datplot = ans, data = for_lu))
}


library(charlatan)

simsbasichalf <- saom_simulate2(dat = senateSiena, struct = SenBasic, parms = M1SenMeans * c(1,1,1,.5,.5), N = 190)
simsbasicquart <- saom_simulate2(dat = senateSiena, struct = SenBasic, parms = M1SenMeans * c(1,1,1,.25,.25), N = 190)
simsbasicneghalf <- saom_simulate2(dat = senateSiena, struct = SenBasic, parms = M1SenMeans * c(1,1,1,-.5,-.5), N = 190)
simsbasicnegquart <- saom_simulate2(dat = senateSiena, struct = SenBasic, parms = M1SenMeans * c(1,1,1,-.25,-.25), N = 190)
simsbasicoppdens <- saom_simulate2(dat = senateSiena, struct = SenBasic, parms = M1SenMeans * c(1,1,1,-1,1), N = 190)
simsbasicopprecip <- saom_simulate2(dat = senateSiena, struct = SenBasic, parms = M1SenMeans * c(1,1,1,1,-1), N = 190)

gotsims %>% add_row(model = "basic", condition = c("halveDR", "quarterDR", "oppositeDRhalf", "oppositeDRquart", "oppositeD", "oppositeR"), 
                    inits = NA, structs = NA, dat = NA, N = NA, sims = NA) -> gotsims
gotsims$inits[37:42] <- list(M1SenMeans * c(1,1,1,.5,.5), M1SenMeans * c(1,1,1,.25,.25), M1SenMeans * c(1,1,1,-.5,-.5),
                             M1SenMeans * c(1,1,1,-.25,-.25), M1SenMeans * c(1,1,1,-1,1), M1SenMeans * c(1,1,1,1,-1)) 
gotsims$structs[37:42] <- list(SenBasic, SenBasic,SenBasic,SenBasic,SenBasic,SenBasic )
gotsims$dat[37:42] <- list( senateSiena, senateSiena, senateSiena, senateSiena, senateSiena, senateSiena)
gotsims$N[37:42] <- 190
gotsims$sims[37:42] <- list(simsbasichalf, simsbasicquart, simsbasicneghalf, simsbasicnegquart, simsbasicoppdens, simsbasicopprecip)
gotsims <- arrange(gotsims, model, condition)

save(gotsims, file = "Data/senate/FauxTrueData/M_1_sims_for_fdata.RDS")


#########################
# Get more lineups made - what do i need to save? 
# 1. svg filename
# 2. experimental condition 
# 3. store the csv that created the lineup
scriptURL = "http://www.hofroe.net/examples/lineup/action-back.js"

library(grid)
library(gridSVG)
dev.new(width = 20, height = 17.5)
senate_lu(altsims = gotsims, nullsims = jttpsims, M = 6, seed = 4852049, 
          w = 1, mod = "jttp", cond = "opposite")

data(theme_elements)
TEnet <- fortify(as.edgedf(theme_elements$edges[,c(2,1)]), theme_elements$vertices)
ggplot(data = TEnet, aes(from_id = from_id, to_id = to_id)) +
  geom_net(directed = T, arrow = arrow(type = 'open')) + 
  theme_net() 

make_interactive(filename2 = "Lineup-Images/senate/test.svg", script=scriptURL,  
                 high="#d5d5d5",  background="#ffffff")
dev.off()

make_lu_dat <- gotsims[rep(1:nrow(gotsims), each = 10),1:2]

make_lu_dat$seed <- ch_integer(n = nrow(make_lu_dat), min = 10000, max = 999999)

make_lu_dat <- make_lu_dat %>% group_by(model, condition) %>% mutate(rep = row_number())
make_lu_dat <- make_lu_dat %>% ungroup() %>% mutate(pic_id = row_number())


senate_lu_storage <- function(details, Altsims, Nullsims, m, Wave, url = "http://www.hofroe.net/examples/lineup/action-back.js"){
  counter <- 0
  picture_details <- data_frame(pic_id = integer(0), sample_size = integer(0), test_param = character(0),
                                param_value = character(0), p_value = numeric(0), obs_plot_location = integer(0),
                                pic_name = character(0), experiment = character(0), difficulty = character(0), data_name = character(0))
  
  for(i in 1:nrow(details)){
      counter <- counter + 1
      lineup <- senate_lu(altsims = Altsims, nullsims = Nullsims, M = m, 
                          seed = details$seed[i], w = Wave, mod = details$model[i],
                          cond = details$condition[i])
      datfilename <- paste0("Data/senate/Lineupdata/senate-m-", m, "-", details$model[i], '-', details$condition[i], '-rep-', details$rep[i], ".csv")
      write_csv(lineup$data, datfilename)
      savepdf(file = paste0("Lineup-Images/senate/pdfs/senate-m-", m, "-", details$model[i], '-', details$condition[i],  '-rep-', details$rep[i]), width = 7.2*2.54, height = 4.5*2.54)
      print(lineup$plot)
      tmpfile <- sprintf("%s.svg",tempfile(tmpdir="Lineup-Images/senate/svgs"))
      make_interactive(filename2 = tmpfile, script=url,  
                       high="#d5d5d5",  background="#ffffff")
      dev.off()
      picture_details <- picture_details %>% add_row(
        pic_id = details$pic_id[i], sample_size = NA, test_param = details$model[i],
        param_value = details$condition[i], p_value = NA, obs_plot_location = lineup$datplot,
        pic_name = tmpfile, experiment = "turk22", difficulty = NA, data_name = datfilename)
      
    }
  return(picture_details)
}

library(grid)
library(gridSVG)
set.seed(138838)
testing <- senate_lu_storage(details = make_lu_dat[sample(420,10),], Altsims = gotsims, Nullsims = datSims, 
                  m = 6, Wave = 1)
testing_fill_in <- data_frame( pic_name = testing$pic_name, HH= "", ST = "")
write_csv(testing_fill_in, "testing_fill_in.csv")

write_csv(testing, "testing2.csv")



lineup_details <- senate_lu_storage(details = make_lu_dat, Altsims = gotsims, Nullsims = datSims, 
                                    m = 6, Wave = 1)


make_lu_dat[which(make_lu_dat$pic_id == 147),]

senate_lu(altsims = gotsims, nullsims = datSims, M = 6, seed = make_lu_dat[which(make_lu_dat$pic_id == 151),"seed"][[1]], w = 1, mod = make_lu_dat[which(make_lu_dat$pic_id == 151),"model"][[1]], cond = make_lu_dat[which(make_lu_dat$pic_id == 151),"condition"][[1]])

# final details 

effectsTest <- c("basic-both", "basic-density", "basic-reciprocity", "jttp", "jtts", "simttb", "samettp")
condition1 <- c("negative", "positive")
condition2 <- c("easy", "medium", "hard")

allconditions <- expand.grid(effectsTest, condition1, condition2)
names(allconditions) <- c("effect", "sign", "difficulty")
allconditions <- bind_rows(allconditions, allconditions, allconditions) %>% mutate(rep = rep(1:3, each = 42)) %>% arrange()
