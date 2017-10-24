# looking at experiment data
library(tidyverse)
library(RSQLite)

con <- dbConnect(SQLite(), "~/Desktop/turk.db")
#dbListTables(con)

fb <- dbReadTable(con, "feedback")
#head(fb)

fb <- filter(fb, description == "turk22")
#head(fb)
#fb$nick_name %>% unique %>% sort
#c("Amy Crawford", "hh", "sam", "fdgdfg", "Nick Berry", "sdfds")
#fb %>% filter(nick_name == "campaign_2165")
#fb %>% filter(nick_name == "ivasanthi@gmail.com")

turkfb <- filter(fb, !(nick_name %in% c("Amy Crawford", "hh", "sam", "fdgdfg", "Nick Berry", "sdfds")))

turkfb <- turkfb %>% mutate(response = map(response_no, 
                                 .f = function(x){parse_number(unlist(str_split(x, pattern=",")))}))

turkfb <- turkfb %>% mutate(weight = map_int(response, length)) 

turkfb <- turkfb %>% mutate(weight = 1/weight) 

turkfb <- turkfb %>% unnest(response)

pic_details <- read_csv("data/picture_details.csv")

#head(pic_details)

#pic_details <- filter(pic_details, experiment == "turk22")

turkfb2 <- left_join(turkfb, pic_details, by = "pic_id")

turkfb2 <- turkfb2 %>% mutate(datapick = response == obs_plot_location)
#summary(turkfb2$datapick)

# turkfb2 %>% group_by(test_param, param_value, pic_id) %>% 
#   summarize(datamean = mean(datapick, na.rm=T), n = n()) %>% 
#   ungroup() %>% 
#   mutate(param_value = as.numeric(param_value)) %>% 
#   filter(test_param == "recip") %>% 
#   ggplot(aes(x = param_value, y = datamean)) + 
#   geom_point() + 
#   xlim(c(0, .01)) + 
#   scale_x_log10()
#   #facet_wrap(~test_param, scales = 'free')
  
turkfb2 <- filter(turkfb2, !is.na(pic_id))
#write_csv(turkfb2, "data/turk22.csv")


# get parameter values of lineups 
load("data/allModelMeans.RDS")
modelMeanEsts <- modelMeanEsts %>% add_row(model = "bigmod", ests = NA)
modelMeanEsts$ests <- list(modelMeanEsts$ests[[1]], modelMeanEsts$ests[[2]],modelMeanEsts$ests[[3]], modelMeanEsts$ests[[4]], modelMeanEsts$ests[[5]],modelMeanEsts$ests[[6]],c(2.4405048,2.4594403,2.2098176,-4.9232775,4.8916183,2.3743720,0.2047038,6.9661589))

modelValues <- modelMeanEsts %>% unnest() %>% filter(model != "samep")
modelValues$param <- "rate"
modelValues$param[c(4:5, 9:10, 15:16, 21:22, 27:28, 33:34)] <- rep(c("dens", "recip"), 6)
modelValues$param[c(11,17,23,29,35,36,37)] <- c("jttp", "jtts", "samettp", "simttb", "jtts", "simttb", "samettp")
modelValues <- modelValues %>% filter(param != "rate", model != "bigmod")

# model - test_param; mult - param_value
compute_true_value <- function(mod, mult, values = modelValues){
  if (mod %in% c("dens", "recip")){
    est <- filter(values, model == "basic", param == mod) %>% select(ests) %>% as.numeric()
  } else if (mod == "bigmod"){
    est <- 1
  } else{
    est <- filter(values, model == mod, param == mod) %>% select(ests) %>% as.numeric()
  }
  est*mult
}

get_est <- function(mod, values = modelValues){
  if (mod %in% c("dens", "recip")){
    est <- filter(values, model == "basic", param == mod) %>% select(ests) %>% as.numeric()
  } else if (mod == "bigmod"){
    est <- 1
  } else{
    est <- filter(values, model == mod, param == mod) %>% select(ests) %>% as.numeric()
  }
  est
}

turkfb2 <- turkfb2 %>% mutate(size = map2_dbl(test_param, param_value, compute_true_value), initialEst = map_dbl(test_param, get_est))
turkfb2 <- turkfb2 %>% mutate(sign = sign(size - initialEst))
turkfb2 <- turkfb2 %>% mutate(difficulty = str_sub(pic_id, 1,1))


turkbf2_gof <- turkfb2 %>% filter(alt_model == "data")
turkbf2_sig <- turkfb2 %>% filter(alt_model != "data")

write_csv(turkbf2_gof, "turk22-gof.csv")
write_csv(turkbf2_sig, "turk22-sig.csv")
