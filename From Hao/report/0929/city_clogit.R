library(tidyverse)
# library(mclogit)

city <- readRDS("/Users/sunhao/Documents/GitHub/ChinaAg/From Hao/data/citylevel/citycomb.rds")

city_soy <- city %>% select(province, city, year, stateprop, matches("soy"), policy16, policy19, policy21) %>% 
  select(-policy_soy, -target_soy) %>% 
  "names<-"(c("province", "city", "year", "stateprop", "acre", "share", "labor_cost", "service_cost", "land_cost",
              "yield", "yield_lag", "saleprice", "futureprice", "sp_lag", "subsidy", "yieldprop", "nationprice",
              "policy16", "policy19", "policy21")) %>% 
  mutate(income_fp = yield_lag * futureprice - labor_cost - service_cost - land_cost,
         income_sp = yield_lag * sp_lag - labor_cost - service_cost - land_cost, 
         policy16_prop = policy16 * stateprop, 
         policy19_prop = policy19 * stateprop, 
         policy21_prop = policy21 * stateprop) %>% 
  cbind(., categ = "soy")

city_corn <- city %>% select(province, city, year, stateprop, matches("corn")) %>% 
  "names<-"(c("province", "city", "year", "stateprop", "acre", "share", "labor_cost", "service_cost", "land_cost",
              "yield", "yield_lag", "saleprice", "futureprice", "sp_lag", "subsidy", "yieldprop", "nationprice")) %>% 
  cbind(., data.frame(policy16 = -city$policy16, policy19 = -city$policy19, policy21 = -city$policy21)) %>%
  mutate(income_fp = yield_lag * futureprice - labor_cost - service_cost - land_cost,
         income_sp = yield_lag * sp_lag - labor_cost - service_cost - land_cost,
         policy16_prop = policy16 * stateprop, 
         policy19_prop = policy19 * stateprop, 
         policy21_prop = policy21 * stateprop) %>% 
  cbind(., categ = "corn")

city_other <- 
  city %>% select(province, city, year, stateprop) %>% 
  cbind(., data.frame(acre = city$acre_total - city$acre_corn - city$acre_soy, 
                      share = 1 - city$share_corn - city$share_soy, 
                      labor_cost = 0, service_cost = 0, land_cost = 0, yield = 0, yield_lag = 0, saleprice = 0, 
                      futureprice = 0, sp_lag = 0, subsidy = 0, yieldprop = 0, nationprice = 0, 
                      policy16 = 0, policy19 = 0, policy21 = 0)) %>%
  mutate(income_fp = yield_lag * futureprice - labor_cost - service_cost - land_cost,
         income_sp = yield_lag * sp_lag - labor_cost - service_cost - land_cost,
         policy16_prop = policy16 * stateprop, 
         policy19_prop = policy19 * stateprop, 
         policy21_prop = policy21 * stateprop) %>% 
  cbind(., categ = "other")

subsidy_lag <- readRDS("/Users/sunhao/Documents/GitHub/ChinaAg/From Hao/data/heilongjiang/dat.rds") %>% 
  select(crop, year, subsidy_mu_lag) %>% 
  "colnames<-"(c("categ", "year", "subsidy_lag")) %>% 
  filter(categ %in% c("corn", "soybean"), year %in% 2010:2021) %>% 
  rbind(., data.frame(categ = "other", year = 2010:2021, subsidy_lag = 0))

subsidy_lag$categ[subsidy_lag$categ == "soybean"] <- "soy"

city_long <- 
  rbind(city_corn, city_soy, city_other) %>% 
  left_join(., subsidy_lag) %>% 
  arrange(province, city, year, categ) %>% 
  mutate(id = rep(1:nrow(city_corn), each = 3)) 

city_long$categ <- factor(city_long$categ)

city.clogit <- 
  mclogit::mclogit(cbind(categ, id) ~ income_sp + yield_lag + 
                     subsidy_lag + yieldprop + policy16_prop + policy19_prop + policy21_prop, 
                   data = city_long, weights = acre)

summary(city.clogit)

dispersion(city.clogit,method="Afroz")

