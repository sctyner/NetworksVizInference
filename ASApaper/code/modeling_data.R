# modeling data

turkbf2_sig %>% mutate(type2 = ifelse(sign == 0, sign(initialEst), sign),
                       type2 = ifelse(type == "one", type2, -1)) %>% 
  group_by(pic_id, size, test_param, sign, type2,alt_model) %>% summarize(
    datapick = mean(datapick)
  ) %>%
  filter(pic_id != 3132) %>% 
  ggplot(aes(x = size, y = datapick, colour=factor(type2))) + 
  geom_point() +
  facet_grid(~test_param, scales="free", labeller = 'label_both') +
  geom_smooth(se=FALSE, method="lm")

modelData_sig <- turkbf2_sig %>% mutate(type2 = ifelse(sign == 0, sign(initialEst), sign),
                                        type2 = ifelse(type == "one", type2, -1)) %>% filter(pic_id != 3132) 

model1 <- glmer(datapick ~ -1 + test_param:size:as.factor(type2) + (1|pic_id) + (1|nick_name), 
                data = modelData_sig, family = 'binomial')
summary(model1)

library(vinference)

pV(x = 16, K = 30, m = 6, scenario = 3)

vinfturk <- turkfb2 %>% group_by(pic_id) %>% summarise(npick = sum(datapick), tot = n(), 
                                                       pv = map2_dbl(npick, tot, pV, m = 6, scenario = 3))


# is difficulty balanced?
ggplot(data = turkfb2) + 
  geom_bar(aes(x = difficulty, fill = datapick), position = 'fill')

turkfb2 %>% group_by(pic_id, size, test_param, sign, type,alt_model) %>% summarize(
  datapick = mean(datapick)
) %>% ggplot(aes(x = size, y = datapick, colour=factor(sign), shape=type)) + 
  geom_point() +
  facet_grid(alt_model~test_param, scales="free", labeller = 'label_both') +
  geom_smooth(se=FALSE, method="lm")

library(lme4)

tryglmm <- glmer(formula = as.numeric(datapick) ~ test_param + size:test_param:sign:type + (1|pic_id) + (1|nick_name), family = binomial, data = turkfb2 %>% filter(alt_model != "data"))
summary(tryglmm)

# separate out type 
control <- glmerControl(optimizer = 'bobyqa')
glmm_one <- glmer(formula = as.numeric(datapick) ~ -1 + test_param + size:test_param:sign + (1|pic_id) + (1|nick_name), 
                  control = control, family = binomial, 
                  data = turkfb2 %>% filter(alt_model != "data", type == "one"))
summary(glmm_one)

glmm_M1 <- glmer(formula = as.numeric(datapick) ~ -1 + test_param + test_param:size + (1|pic_id) + (1|nick_name), 
                 control = control, family = binomial, 
                 data = turkfb2 %>% filter(alt_model != "data", type == "M-1"))
summary(glmm_M1)

glmm_gof <- glmer(formula = as.numeric(datapick) ~ -1 + test_param + (1|pic_id) + (1|nick_name), 
                  control = control, family = binomial, 
                  data = turkfb2 %>% filter(alt_model == "data"))

summary(glmm_gof)
