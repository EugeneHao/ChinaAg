library(tidyverse)
source("/Users/sunhao/Documents/GitHub/ChinaAg/From Hao/functions/get_lag.R")

regdat <- readRDS("/Users/sunhao/Documents/GitHub/ChinaAg/From Hao/data/18provinces/regdat_18prov.rds")
preddat <- readRDS("/Users/sunhao/Documents/GitHub/ChinaAg/From Hao/data/18provinces/preddat_18prov.rds") 

regdat <- rbind(regdat, preddat)

lm_corn_pf <- lm(y2 ~ 0 + region + pfex_corn + pfex_rice + pfex_soy + subsidy_corn + subsidy_rice + subsidy_soy + 
                   stateprop, data = regdat %>% filter(crop == "corn"))

summary(lm_corn_pf)


lm_soy_pf <- lm(y2 ~ 0 + region + pfex_corn + pfex_rice + pfex_soy + subsidy_corn + subsidy_rice + subsidy_soy + 
                   stateprop, data = regdat %>% filter(crop == "soybean"))

summary(lm_soy_pf)

lm_rice_pf <- lm(y2 ~ 0 + region + pfex_corn + pfex_rice + pfex_soy + subsidy_corn + subsidy_rice + subsidy_soy + 
                  stateprop, data = regdat %>% filter(crop == "rice"))

summary(lm_rice_pf)


library(lme4)
library(lmerTest)
glm_corn_pf <- lmer(y2 ~ 0 + pfex_corn + pfex_rice + pfex_soy + subsidy_corn + subsidy_rice + subsidy_soy + 
                   stateprop | region, data = regdat %>% filter(crop == "corn"),
                   control = lmerControl(optimizer = "Nelder_Mead"))

summary(glm_corn_pf)
