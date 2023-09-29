library(tidyverse)
setwd("/Users/sunhao/Documents/GitHub/ChinaAg/")

city_raw <- read_csv("From Hao/rawdata/city_raw_0928/city_acreage.csv") %>% 
  select(province, city, year, acre_total, acre_total, acre_corn, acre_soy, share_corn, share_soy)
names(city_raw)[1] <- "region"

# Step 1: check city level raw data ####
# check any missing year 
for(i in unique(city_raw$region))
{
  table(city_raw$region[city_raw$region == i], city_raw$city[city_raw$region == i]) %>% print()
}

# check any city shares the same name 
table(city_raw$city)

# check missing
city_raw %>% 
  filter(is.na(acre_total) | is.na(acre_corn) | is.na(acre_soy))

# remove wuhai 

city_raw <- city_raw %>% filter(!city == "wuhai")


# Step 2: add province-level data #### 

### 2.1 three different cost variables for corn and soybean  ####
cost0821 <- rbind(readxl::read_xls("/Users/sunhao/Documents/GitHub/ChinaAg/From Hao/rawdata/update/cost0821-1.xls"),
                  readxl::read_xls("/Users/sunhao/Documents/GitHub/ChinaAg/From Hao/rawdata/update/cost0821-2.xls"))

names(cost0821) <- c("index", "region", "crop", "year", "value")
cost0821$region <- factor(cost0821$region, 
                          labels = c("Yunnan", "Inner mongolia", "Jilin", "Sichuan", "Anhui", "Shandong", "Shanxi", "Guangxi",
                                     "Xinjiang", "Jiangsu", "Hebei", "Henan", "Hubei", "Gansu", "Guizhou", "Liaoning",
                                     "Shaanxi", "Heilongjiang"))
cost0821$year <- as.numeric(cost0821$year)
cost0821$value <- as.numeric(cost0821$value)

cost <- cost0821 %>% filter(region %in% c("Hebei", "Heilongjiang", "Henan", "Inner mongolia", 
                                  "Jilin", "Liaoning", "Shandong"), 
                    crop %in% c("大豆", "玉米"), 
                    year %in% 2007:2021)
cost$region <- as.character(cost$region)
cost$crop[cost$crop == "玉米"] <- "corn"
cost$crop[cost$crop == "大豆"] <- "soy"

cost$index <- factor(cost$index, 
                     labels = c("yield", "laber_cost", "soil_cost", "saleprice", "total_cost", "service_cost"))

cost <- cost %>% unite("new_key", crop, index, sep = "_") %>% 
  tidyr::spread(key = new_key, value) 

cost_pr <- cost %>% select("region", "year", 
                           "corn_laber_cost", "corn_service_cost", "corn_soil_cost", 
                           "soy_laber_cost", "soy_service_cost", "soy_soil_cost")

### 2.2 yield of corn and soybean ####

yield_lag <- cost0821 %>% filter(region %in% c("Hebei", "Heilongjiang", "Henan", "Inner mongolia", 
                                          "Jilin", "Liaoning", "Shandong"), 
                            crop %in% c("大豆", "玉米"))
yield_lag$region <- as.character(yield_lag$region)
yield_lag$crop[yield_lag$crop == "玉米"] <- "corn"
yield_lag$crop[yield_lag$crop == "大豆"] <- "soy"

yield_lag$index <- factor(yield_lag$index, 
                     labels = c("yield", "laber_cost", "soil_cost", "saleprice", "total_cost", "service_cost"))

yield <- yield_lag %>% filter(index == "yield") %>% arrange(region, crop, year)


get_lag <- function(data, oldname, lag, weight)
{
  newvec <- NULL
  index = which(names(data) == oldname)
  for(i in unique(data$region))
  {
    for(j in unique(data$crop))
    {
      tmpdat <- data %>% filter(region == i, crop == j)
      length <- nrow(tmpdat)
      oldvec <- tmpdat[,index] %>% unlist()
      tmpvec <- NULL
      for(k in 1:lag)
      {
        tmpvec <- cbind(tmpvec, c(rep(oldvec[1], k), oldvec[-(length + 1 - (1:k))]))
      }
      newvec <- c(newvec, as.vector(tmpvec %*% weight))
    }
  }
  return(newvec)
}


yield$yield_lag <- get_lag(yield, "value", 3, weight = c(0.34, 0.33, 0.33))

yield_pr <- yield %>% select(-yield_lag) %>% unite("new_key", crop, index, sep = "_") %>% 
  tidyr::spread(key = new_key, value) %>% 
  left_join(., 
            yield %>% select(-value) %>% unite("new_key", crop, index, sep = "_") %>% 
              tidyr::spread(key = new_key, yield_lag) %>% 
              "names<-"(c("region", "year", "corn_yield_lag", "soy_yield_lag")))


### 2.3 national sale price and expected price for corn and soybean ####

# future price (national)
fuprice <- readRDS("From Hao/data/heilongjiang/future_price.rds") %>% 
  filter(month == "03") %>% select(year, crop, ex_price) %>% 
  mutate(year = as.numeric(year))

my_fuprice <- data.frame(year = rep(2001:2021, 4), 
                         crop = rep(c("corn", "rice", "soybean", "wheat"), each = 21)) %>% 
  left_join(., fuprice) %>% spread(crop, ex_price) %>% 
  "colnames<-"(c("year", paste(c("corn", "rice", "soybean", "wheat"), "fp", sep = "_"))) %>% 
  filter(year %in% 2007:2021) %>% 
  select(year, corn_fp, soybean_fp) %>% 
  "names<-"(c("year", "corn_futureprice", "soy_futureprice"))

price_pr <- cost %>% select("region", "year", "corn_saleprice", "soy_saleprice") %>% 
  mutate(corn_saleprice = corn_saleprice/50, soy_saleprice = soy_saleprice/50) %>% 
  left_join(., my_fuprice) 


### 2.4 national sale price_lag for corn and soybean ####

cost_lag <- cost0821 %>% filter(region %in% c("Hebei", "Heilongjiang", "Henan", "Inner mongolia", 
                                  "Jilin", "Liaoning", "Shandong"), 
                    crop %in% c("大豆", "玉米"), 
                    year %in% 2006:2020)
cost_lag$region <- as.character(cost_lag$region)
cost_lag$crop[cost_lag$crop == "玉米"] <- "corn"
cost_lag$crop[cost_lag$crop == "大豆"] <- "soy"

cost_lag$index <- factor(cost_lag$index, 
                     labels = c("yield", "laber_cost", "soil_cost", "saleprice", "total_cost", "service_cost"))

cost_lag <- cost_lag %>% unite("new_key", crop, index, sep = "_") %>% 
  tidyr::spread(key = new_key, value) 
cost_lag$year <- cost_lag$year + 1

price_lag_pr <- cost_lag %>% select("region", "year", "corn_saleprice", "soy_saleprice") %>% 
  mutate(corn_saleprice = corn_saleprice/50, soy_saleprice = soy_saleprice/50) %>% 
  "names<-"(c("region", "year", "corn_sp_lag", "soy_sp_lag"))

### 2.5 subsidy for corn and soybean ####

subsidy <- readRDS("From Hao/data/heilongjiang/dat.rds") %>% select(crop, year, subsidy_mu) %>% 
  "colnames<-"(c("crop", "year", "subsidy")) %>% 
  filter(crop %in% c("corn", "soybean"), year %in% 2007:2021) %>% 
  spread(key = crop, value = subsidy) %>% 
  "names<-"(c("year", "corn_subsidy", "soy_subsidy"))

### 2.6 profit using national sale price_lag and expected profit for corn and soybean ####

city <- left_join(city_raw, cost_pr) %>% 
  left_join(., yield_pr) %>% left_join(., price_pr) %>% left_join(., price_lag_pr) %>% 
  left_join(., subsidy) 
  # mutate(corn_profit_lag = corn_yield * corn_sp_lag - corn_laber_cost - corn_service_cost - corn_soil_cost, 
  #        soy_profit_lag = soy_yield * soy_sp_lag - soy_laber_cost - soy_service_cost - soy_soil_cost, 
  #        corn_profit_ex = corn_yield * corn_futureprice - corn_laber_cost - corn_service_cost - corn_soil_cost, 
  #        soy_profit_ex = soy_yield * soy_futureprice - soy_laber_cost - soy_service_cost - soy_soil_cost)



