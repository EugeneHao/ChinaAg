library(tidyverse)
northeast <- readRDS("~/Documents/GitHub/ChinaAg/From Hao/data/dongbei/multidat_nolag.rds")
northeast <- northeast %>% mutate(
  ac_price = price/yield,
  share_lag = lag(share,1),
  ex_profit = (ex_price - cost)*
  four = ifelse(region == "Neimenggu"|region == "Heilongjiang"|region == "Jilin"|region == "Liaoning",1,0)
)

yield_dta <- northeast %>% 
  arrange(year) %>% 
  group_by(crop,region) %>% 
  mutate(yield_avg = zoo::rollmean(yield, k = 3, fill = NA))
  


m_predit <- lm(ac_price ~ ex_price * crop , data = northeast)
northeast$ad_price <- predict(m_predit)
mt
m1 <- lm(share ~ share_lag + ex_price + crop, data = northeast)
summary(m1)

m2 <- lm(share ~ share_lag + ad_price + crop, data = northeast)
summary(m2)

m3 <- lm(share ~ share_lag + ad_price + crop + subsidy, data = northeast)
summary(m3)

m4 <- lm(share ~ share_lag + ad_price  + subsidy + four, data = northeast)
summary(m4)




