# new data for prediction from model 

turk22 <- read_csv("data/turk22-sig.csv")
turk22 <- turk22 %>% mutate(type2 = ifelse(sign == 0, sign(initialEst), sign),
       type2 = ifelse(type == "one", type2, -1))
parmsRange <- turk22 %>% group_by(test_param, type2) %>% summarize(minparm = min(size), maxparm = max(size)) 

load("data/allModelMeans.RDS")
get_mult <- function(mod, size, values = modelValues){
  if (mod %in% c("dens", "recip")){
    est <- filter(values, model == "basic", param == mod) %>% select(ests) %>% as.numeric()
  } else if (mod == "bigmod"){
    est <- 1
  } else{
    est <- filter(values, model == mod, param == mod) %>% select(ests) %>% as.numeric()
  }
  size/est
}

newdata <- parmsRange %>% 
  mutate(newsizes = map2(minparm, maxparm, function(x,y){seq(x,y, (y-x)/1000)})) %>% 
  unnest %>% select(test_param, type2, size = newsizes)
newdata$type2 <- as.factor(newdata$type2)
newdata$pic_id <- sample(1001:1050, replace = T, size = nrow(newdata))
newdata$nick_name <- sample(LETTERS, replace = T, size = nrow(newdata))
newdata$centersize <- (newdata$size - sub)/div
newdata <- newdata %>% mutate(param_value = map2_dbl(test_param, size, get_mult))

newdata <- newdata %>% mutate(interaction = interaction(test_param, as.factor(type2)))

testnew <- predict(model1, newdata = newdata, allow.new.levels = T, type = 'response')
testnewcent <- predict(model1cent, newdata = newdata, allow.new.levels = T, type = 'response')
testnewmult <- predict(model1mult, newdata = newdata, allow.new.levels = T, type = 'response')
testnewmult2 <- predict(model2mult, newdata = newdata, allow.new.levels = T, type = 'response')
testfinal <- predict(finalGLMM, newdata = newdata, allow.new.levels = T, type = 'response')

testnew3 <- predict(model3, newdata = newdata, allow.new.levels = T, type = 'response')

testfixnew <- predict(modelfix, newdata = newdata, allow.new.levels = T, type = 'response')

newdata$prediction <- testnew
newdata$prediction3 <- testnew3
newdata$predictionfix <- testfixnew
newdata$predictioncent <- testnewcent
newdata$predictionmult <- testnewmult
newdata$predictionmult2 <- testnewmult2
newdata$predictfinal <- testfinal


plotdata <- turk22 %>% mutate(type2 = ifelse(sign == 0, sign(initialEst), sign),
                       type2 = ifelse(type == "one", type2, -1)) %>% 
  group_by(pic_id, size, test_param, sign, type2,alt_model) %>% summarize(
    datapick = mean(datapick)
  )

ggplot() + 
  geom_point(data = newdata, aes(x = size, y = prediction, color = as.factor(type2)), size=1) + 
  geom_line(data = newdata, aes(x = size, y = predictfinal, color = as.factor(type2))) + 
  geom_point(data = plotdata, aes(x = size, y = datapick, color = as.factor(type2)), shape = 2) + 
  facet_wrap(~test_param, scales = 'free') 

newdata2 <- newdata %>% select(test_param, type2, size, centersize, pic_id, nick_name, predictfinal)

write_csv(newdata2, "data/newdata_pred_glmm.csv")
  

