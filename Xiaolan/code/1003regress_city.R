library(tidyverse)
library("AER")
setwd("/Users/xiaolanwan/Documents/GitHub/ChinaAg/")

#prepare variables

citycomb <- readRDS("Xiaolan/data clean/citycomb.rds")

##compute soybean weight for cities
soynation <- readRDS("~/Documents/GitHub/ChinaAg/Xiaolan/data clean/soynation.rds")

citycomb <- citycomb %>% left_join(soynation, by = "year") 

citycomb <- citycomb %>% mutate(soy_weight = acre_soy/soy_nation)

citycomb <- citycomb %>% mutate(corn_futureprofit = corn_futureprice * corn_yield_lag-corn_service_cost-corn_labor_cost,
                    soy_futureprofit = soy_futureprice * soy_yield_lag-soy_service_cost-soy_labor_cost,
                    relative_futureprofit = corn_futureprofit - soy_futureprofit,
                    corn_lagprofit = corn_sp_lag * corn_yield_lag-corn_service_cost-corn_labor_cost,
                    soy_lagprofit = soy_sp_lag * soy_yield_lag-soy_service_cost-soy_labor_cost,
                    relative_revenue_lag = corn_sp_lag * corn_yield_lag - soy_sp_lag * soy_yield_lag,
                    relative_revenue_future = corn_futureprice * corn_yield_lag - soy_futureprice * soy_yield_lag,
                    relative_lagprofit = corn_lagprofit - soy_lagprofit, 
                    relative_comb = case_when(
                      year %in% 2016:2021 ~ relative_futureprofit,
                      TRUE ~ relative_lagprofit
                    ))

citycomb <- citycomb %>% mutate(relative_revenue = corn_nationprice * corn_yield - soy_nationprice*soy_yield)

#summary data
summary(citycomb$soy_weight)
summary(citycomb$relative_profit)
citycomb %>% group_by(year) %>% 
  summarise(relative = mean(relative_profit),
            subsidy = mean(soy_subsidy),
            corn_sale_lag = mean(corn_saleprice),
            corn_futureprice = mean(corn_futureprice),
            relative_futureprofit = mean(relative_futureprofit),
            relative_lagprofit = mean(relative_lagprofit),
            relative_comb = mean(relative_comb),
            relative_revenue_future = mean(relative_revenue_future),
            relative_revenue_lag = mean(relative_revenue_lag))

citycomb %>% group_by(year) %>% summarise(relative_revenue = mean(relative_revenue))
#regression:soybean weight

tobit(share_corn ~ corn_sp_lag + soy_sp_lag + I(soy_weight * policy_soy), data = citycomb,
      left = 0, right = 1) %>% summary()

tobit(share_soy ~ corn_sp_lag + soy_sp_lag + I(soy_weight * policy_soy), data = citycomb,
      left = 0, right = 1) %>% summary()


tobit(share_corn ~ I(soy_sp_lag/corn_sp_lag) + I(soy_weight * policy_soy), data = citycomb,
      left = 0, right = 1) %>% summary()

tobit(share_soy ~ I(soy_sp_lag/corn_sp_lag) + I(soy_weight * policy_soy), data = citycomb,
      left = 0, right = 1) %>% summary()

#cotrol variables:corn relative profit
tobit(share_corn ~ soy_subsidy+ I(soy_weight * policy_soy), data = citycomb,
      left = 0, right = 1) %>% summary()

tobit(share_soy ~ soy_subsidy+ I(soy_weight * policy_soy), data = citycomb,
      left = 0, right = 1) %>% summary()


##relative profits
tobit(share_corn ~ relative_comb+ I(soy_weight * policy_soy), data = citycomb,
      left = 0, right = 1) %>% summary()

tobit(share_soy ~ relative_comb + I(soy_weight * policy_soy), data = citycomb,
      left = 0, right = 1) %>% summary()



#cotrol variables:corn relative profit
tobit(share_corn ~ I(soy_subsidy/15000)+ I(soy_weight * policy_soy) + province, data = citycomb,
      left = 0, right = 1) %>% summary()

tobit(share_soy ~ I(soy_subsidy/15000)+ I(soy_weight * policy_soy), data = citycomb,
      left = 0, right = 1) %>% summary()



#sof
tobit(share_corn ~ I(soy_subsidy/15000)+ I(soy_weight * policy_soy) + stateprop, data = citycomb,
      left = 0, right = 1) %>% summary()

tobit(share_soy ~ I(soy_subsidy/15000)+ I(soy_weight * policy_soy) + stateprop, data = citycomb,
      left = 0, right = 1) %>% summary()

##subgroup
tobit(share_corn ~ I(soy_subsidy/15000)+ I(soy_weight * policy_soy), data = citycomb %>% filter(stateprop > 0.04),
      left = 0, right = 1) %>% summary()


tobit(share_corn ~ I(soy_subsidy/15000)+ I(soy_weight * policy_soy), data = citycomb %>% filter(stateprop < 0.04),
      left = 0, right = 1) %>% summary()





