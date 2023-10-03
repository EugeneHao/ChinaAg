# now we want to use 18 different provinces (version 2)
# follows from 0822_dataclean.R


library(tidyverse)
setwd("/Users/sunhao/Documents/GitHub/ChinaAg/")
source("From Hao/functions/get_lag.R")

### Data 1: Cost Data  (including yield, sale price, cost) ####

rawcost <- rbind(readxl::read_xls("/Users/sunhao/Documents/GitHub/ChinaAg/From Hao/rawdata/update/cost0821-1.xls"),
                 readxl::read_xls("/Users/sunhao/Documents/GitHub/ChinaAg/From Hao/rawdata/update/cost0821-2.xls"))

names(rawcost) <- c("index", "region", "crop", "year", "value")


rawcost$region <- factor(rawcost$region, 
                         labels = c("Yunnan", "InnerMongolia", "Jilin", "Sichuan", "Anhui", "Shandong", "Shanxi", "Guangxi",
                                    "Xinjiang", "Jiangsu", "Hebei", "Henan", "Hubei", "Gansu", "Guizhou", "Liaoning",
                                    "Shaanxi", "Heilongjiang"))

# (Note: have not sorted by year yet ! )

costdat <- rawcost %>% filter(index != "总成本（元）") 

costdat$index <- factor(costdat$index, labels = c("yield", "laborcost", "landcost", "saleprice", "servicecost"))

ricecateg <- c("中籼稻", "早籼稻", "晚籼稻", "粳稻")

costdat$crop <- factor(costdat$crop, labels = c("rice", "soybean", "wheat", "rice", "rice", "corn", "rice"))
costdat$value <- as.numeric(costdat$value)

# take average for rice 
gg <- costdat %>% group_by(region, index, crop, year) %>% 
  summarise(avg = ifelse(n() == 1, value, sum(value, na.rm = T)/sum(!is.na(value)))) %>% 
  arrange(region, index, crop, year) %>% ungroup()
gg$year <- as.numeric(gg$year)

# 18 provinces, 5 indexes, 4 crops, 21 years 
newcostdat <- data.frame(region = rep(unique(gg$region), each = 5 * 4 * 21),
                         index = rep(rep(unique(gg$index), each = 4 * 21), 18), 
                         crop = rep(rep(unique(gg$crop), each = 21), 18 * 5), 
                         year = rep(2001:2021, 18 * 5 * 4))
newcostdat <- left_join(newcostdat, gg)


# impute yield and price 

avgcost <- readxl::read_xls("/Users/sunhao/Documents/GitHub/ChinaAg/From Hao/rawdata/update/costavg0821.xls")
names(avgcost) <- c("index", "region", "crop", "year", "avgvalue")

avgcost <- avgcost %>% filter(index != "总成本（元）")
avgcost$index <- factor(avgcost$index, labels = c("yield", "laborcost", "landcost", "saleprice", "servicecost"))

avgcost$crop <- factor(avgcost$crop, labels = c("soybean", "wheat", "corn", "rice"))
avgcost$avgvalue <- as.numeric(avgcost$avgvalue)
avgcost$year <- as.numeric(avgcost$year)

# combine data 
newcostdat <- newcostdat %>% left_join(., avgcost %>% select(-region)) 

for(i in 1:nrow(newcostdat))
{
  if(is.na(newcostdat$avg[i]))
  {
    newcostdat$avg[i] <- newcostdat$avgvalue[i]
  }
}

# update 2023-09-03   cost is cost per kg
costdat <- newcostdat %>% select(-avgvalue) %>% spread(key = index, value = avg) %>% 
  mutate(cost = landcost + servicecost + laborcost, 
         saleprice = saleprice/50) 


### Data 2: Acreage Information #### 
rawacre <- readxl::read_xls("/Users/sunhao/Documents/GitHub/ChinaAg/From Hao/rawdata/update/acre0821.xls")

colnames(rawacre) <- c("index", "region", "year", "value")
# region

rawacre$region <- factor(rawacre$region, 
                         labels = c("Yunnan", "InnerMongolia", "Jilin", "Sichuan", "Anhui", "Shandong", "Shanxi", "Guangxi",
                                    "Xinjiang", "Jiangsu", "Hebei", "Henan", "Hubei", "Gansu", "Guizhou", "Liaoning",
                                    "Shaanxi", "Heilongjiang"))

rawacre$index <- factor(rawacre$index, 
                        labels = c("total", "statefarm", "soybean", 
                                   "wheat", "corn", "rice", "allfarm"))

impute_values <- function(df) {
  closest_year_with_value <- min(df$year[!is.na(df$value)])
  for (i in 1:nrow(df)) {
    if (is.na(df$value[i])) {
      if (df$year[i] < closest_year_with_value) {
        df$value[i] <- df$value[df$year == closest_year_with_value]
      } else {
        df$value[i] <- df$value[i - 1]
      }
    }
  }
  return(df)
}

# Applying the function to each combination of index and region
imputed_data <- rawacre %>%
  group_by(index, region) %>%
  nest() %>%
  mutate(data = lapply(data, impute_values)) %>%
  unnest(data)


imputed_data <- imputed_data %>% spread(key = index, value = value) %>% 
  mutate_at(vars(year, total, statefarm, soybean, wheat, corn, rice, allfarm), as.numeric)

# we do not need 2000 data right now 
acredat <- imputed_data %>% filter(year != 2000) 

# compute share 
acredat <- acredat %>% mutate(soybean_share = round(100 * soybean/total, 2), 
                              wheat_share = round(100 * wheat/total, 2), 
                              corn_share = round(100 * corn/total, 2), 
                              rice_share = round(100 * rice/total, 2), 
                              stateprop = round(100 * statefarm/total, 2)) %>%   # allfarm/10
  mutate(other = 100 - soybean_share - wheat_share - corn_share - rice_share) 

# acredat %>% group_by(region) %>% summarise(meanprop = mean(stateprop))

# combine the two data sets 
multidat <- left_join(costdat, 
                      acredat %>% select(region, year, soybean_share, wheat_share, 
                                         corn_share, rice_share, stateprop, other) %>% 
                        gather(key = crop, value = share, -region, -year, -other, -stateprop) %>% 
                        mutate(crop = factor(crop, labels = c("corn", "rice", "soybean", "wheat"))))



### Data 3: Future Price #### 
fuprice <- readRDS("From Hao/data/heilongjiang/future_price.rds") %>% 
  filter(month == "03") %>% select(year, crop, ex_price) %>% 
  mutate(year = as.numeric(year))

my_fuprice <- data.frame(year = rep(2001:2021, 4), 
                         crop = rep(c("corn", "rice", "soybean", "wheat"), each = 21)) %>% 
  left_join(., fuprice)

# load the price data 
price <- readxl::read_xls("From Hao/rawdata/全国平均价格.xls")

names(price) <- c("index", "region", "crop", "year", "price")
price$year <- as.numeric(price$year)
price$price <- as.numeric(price$price)

price$crop <- factor(price$crop, labels = c("soybean", "wheat", "corn", "rice"))
price <- price %>% filter(year %in% 2001:2021) %>% select(crop, year, price)
# price is the price of 50kg crops
my_fuprice <- left_join(my_fuprice, price) %>% mutate(price = price/50)

# impute the missing future price 
for(i in unique(my_fuprice$crop))
{
  if(i == "rice")
  {
    price_diff <- (my_fuprice %>% filter(crop == i, !is.na(ex_price)) %>% "$"(ex_price) %>% mean()) - 
      (my_fuprice %>% filter(crop == i, !is.na(ex_price)) %>% "$"(price) %>% mean())
    my_fuprice$ex_price[is.na(my_fuprice$ex_price) & (my_fuprice$crop == i)] <-
      my_fuprice$price[is.na(my_fuprice$ex_price) & (my_fuprice$crop == i)] + price_diff
  } else
  {
    lm_exprice <- lm(ex_price ~ price, dat = my_fuprice %>% filter(crop == i))
    my_fuprice$ex_price[is.na(my_fuprice$ex_price) & (my_fuprice$crop == i)] <-
      predict(lm_exprice, newdata = my_fuprice %>% filter(crop == i, is.na(ex_price)))
  }
}

names(my_fuprice)[4] <- "national"

multidat <- multidat %>% left_join(., my_fuprice, by = c("crop", "year")) 


### Data 4: subsidy   #### 

subsidy <- readRDS("From Hao/data/heilongjiang/dat.rds") %>% select(crop, year, subsidy_mu) %>% 
  "colnames<-"(c("crop", "year", "subsidy"))

producer_sub <- data.frame(year = 2001:2021, 
                           producer_sub = c(rep(0, 14), 57.58, 71.45, 71.50, 71.70, 72.41, 56.72, 56.72))
# add land fertility protection subsidies (for V2)


multidat <- multidat %>% left_join(., subsidy) %>% left_join(., producer_sub)
multidat$subsidy[is.na(multidat$subsidy)] <- 0

# we only consider subsidy in the northeast China  (update 2023-09-03)
multidat$subsidy[!multidat$region %in% c("Heilongjiang", "Liaoning", "Jilin", "InnerMongolia")] <- 0

# important! arrange before we calculate lag 
multidat <- multidat %>% arrange(region, crop)

# Lag ####

multidat$yield_lag <- get_lag(multidat, "yield", 3, weight = c(0.34, 0.33, 0.33))
multidat$subsidy_lag <- get_lag(multidat, "subsidy", 2, weight = c(0.67, 0.33))
multidat$saleprice_lag <- get_lag(multidat, "saleprice", 2, weight = c(1, 0))

multidat <- multidat %>% 
  mutate(revenue_sp = yield_lag * saleprice_lag,
         revenue_ex = yield_lag * ex_price)

multidat$revenue_sp_lag <- get_lag(multidat, "revenue_sp", 1, weight = 1)
multidat$revenue_ex_lag <- get_lag(multidat, "revenue_ex", 1, weight = 1)

# arrange the data set 
multidat <- multidat %>% arrange(region, crop, year) %>% select(-other)


multidat_wide <- multidat %>%
  pivot_wider(names_from = crop, values_from = names(multidat)[-(1:3)], 
              names_glue = "{crop}_{.value}") %>% 
  mutate(other_share = 100 - corn_share - soybean_share - rice_share - wheat_share)



saveRDS(multidat, "From Hao/data/18provinces_1002/multidat_long.rds")
saveRDS(multidat_wide, "From Hao/data/18provinces_1002/multidat_wide.rds")

