library(tidyverse)

regdat_V2 <- readRDS("/Users/sunhao/Documents/GitHub/ChinaAg/From Hao/data/18provinces_1002/prov.rds")

# y1: put rice and wheat into other 
# y2: put wheat into other 

regdat_V2 <- regdat_V2 %>% 
  mutate(y1_corn = log(corn_share)/log(other_share + rice_share + wheat_share), 
         y1_soybean = log(soybean_share)/log(other_share + rice_share + wheat_share), 
         y2_corn = log(corn_share)/log(other_share + wheat_share), 
         y2_soybean = log(soybean_share)/log(other_share + wheat_share),
         y2_rice = log(rice_share)/log(other_share + wheat_share),
         corn_log = log(corn_share), 
         soybean_log = log(soybean_share), 
         rice_log = log(rice_share), 
         corn_gp_sp = corn_revenue_sp - corn_cost,           # gp: gross profit 
         soybean_gp_sp = soybean_revenue_sp - soybean_cost, 
         rice_gp_sp = rice_revenue_sp - rice_cost,          
         corn_gp_ex = corn_revenue_ex - corn_cost, 
         soybean_gp_ex = soybean_revenue_ex - soybean_cost, 
         rice_gp_ex = rice_revenue_ex - rice_cost, 
         corn_profit_sp = corn_revenue_sp - corn_cost + corn_subsidy_lag,          
         soybean_profit_sp = soybean_revenue_sp - soybean_cost + soybean_subsidy_lag, 
         rice_profit_sp = rice_revenue_sp - rice_cost + rice_subsidy_lag,          
         corn_profit_ex = corn_revenue_ex - corn_cost + corn_subsidy_lag, 
         soybean_profit_ex = soybean_revenue_ex - soybean_cost + soybean_subsidy_lag, 
         rice_profit_ex = rice_revenue_ex - rice_cost + rice_subsidy_lag, 
         cornweight = corn_acreage/corn_nation, 
         corn_policy = cornweight * policy_soy, 
         soybean_policy = soyweight * policy_soy, 
         state_policy = corn_stateprop * policy_soy)   # all crop_stateprop are the same 

regdat_V2$NEfour <- 0
regdat_V2$NEfour[regdat_V2$region %in% c("Heilongjiang", "Liaoning", "Jilin", "InnerMongolia")] <- 1

lm_corn_sp <- lm(y1_corn ~ 0 + corn_gp_sp + soybean_gp_sp + corn_subsidy_lag + soybean_subsidy_lag + 
                   soybean_policy + state_policy + region,data = regdat_V2)

summary(lm_corn_sp)

lm_corn_ex <- lm(y1_corn ~ 0 + corn_gp_ex + soybean_gp_ex + corn_subsidy_lag + soybean_subsidy_lag + 
                  soybean_policy + state_policy + region,data = regdat_V2)

summary(lm_corn_ex)


lm_corn_sp_V2 <- lm(y1_corn ~ 0 + corn_profit_sp + soybean_profit_sp + corn_subsidy_lag + soybean_subsidy_lag + 
                   cornweight + soyweight + corn_policy + soybean_policy + state_policy + region,data = regdat_V2)

summary(lm_corn_sp_V2)

lm_corn_ex_V2 <- lm(y1_corn ~ 0 + corn_profit_ex + soybean_profit_ex + corn_subsidy_lag + soybean_subsidy_lag + 
                      cornweight + soyweight + soybean_policy + corn_stateprop + region,data = regdat_V2)

summary(lm_corn_ex_V2)



### now include rice 

lm_corn_ex_V3 <- lm(y2_corn ~ 0 + corn_gp_ex + soybean_gp_ex + rice_gp_ex + 
                   corn_subsidy_lag + soybean_subsidy_lag + rice_subsidy_lag + 
                   corn_policy + soybean_policy + state_policy + region,data = regdat_V2)

summary(lm_corn_ex_V3)


lm_corn_ex_V4 <- lm(y2_corn ~ 0 + corn_profit_ex + soybean_profit_ex + rice_profit_ex + 
                      corn_subsidy_lag + soybean_subsidy_lag + rice_subsidy_lag + 
                      cornweight + soyweight + soybean_policy + state_policy + corn_stateprop + region,data = regdat_V2)

summary(lm_corn_ex_V4)


## try log(corn)

lm_corn_ex_log <- lm(corn_log ~ 0 + corn_gp_ex + soybean_gp_ex + corn_subsidy_lag + soybean_subsidy_lag + 
                   corn_policy + soybean_policy + state_policy + region,data = regdat_V2)

summary(lm_corn_ex_log)


lm_corn_ex_log <- lm(y2_corn ~ 0 + corn_profit_ex + soybean_profit_ex + rice_profit_ex + 
                       corn_subsidy_lag + soybean_subsidy_lag + rice_subsidy_lag + 
                       corn_policy + soybean_policy + state_policy + corn_stateprop + region,data = regdat_V2)

summary(lm_corn_ex_log)

lm_soy_ex_log <- lm(y2_soybean ~ 0 + corn_profit_ex + soybean_profit_ex + rice_profit_ex + 
                       corn_subsidy_lag + soybean_subsidy_lag + rice_subsidy_lag + 
                       corn_policy + soybean_policy + corn_stateprop + NEfour + region,data = regdat_V2)

summary(lm_soy_ex_log)
