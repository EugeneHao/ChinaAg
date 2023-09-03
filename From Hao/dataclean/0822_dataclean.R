# now we want to use 18 different provinces 

library(tidyverse)
setwd("/Users/sunhao/Documents/GitHub/ChinaAg/")
source("From Hao/functions/get_lag.R")

### Data 1: Cost Data  (including yield, sale price, cost) ####

rawcost <- rbind(readxl::read_xls("/Users/sunhao/Documents/GitHub/ChinaAg/From Hao/rawdata/update/cost0821-1.xls"),
                 readxl::read_xls("/Users/sunhao/Documents/GitHub/ChinaAg/From Hao/rawdata/update/cost0821-2.xls"))

names(rawcost) <- c("index", "region", "crop", "year", "value")


rawcost$region <- factor(rawcost$region, 
                         labels = c("Yunnan", "Neimenggu", "Jilin", "Sichuan", "Anhui", "Shandong", "Shanxi", "Guangxi",
                                    "Xinjiang", "Jiangsu", "Hebei", "Henan", "Hubei", "Gansu", "Guizhou", "Liaoning",
                                    "Shaanxi", "Heilongjiang"))
# (Note: have not sorted by year yet ! )

costdat <- rawcost %>% filter(index %in% c("主产品产量（千克）", "土地成本（元）", "平均出售价格（元）", "物质与服务费用（元）"))  # "总成本（元）"

costdat$index <- factor(costdat$index, labels = c("yield", "landcost", "price", "servicecost"))

ricecateg <- c("中籼稻", "早籼稻", "晚籼稻", "粳稻")

costdat$crop <- factor(costdat$crop, labels = c("rice", "soybean", "wheat", "rice", "rice", "corn", "rice"))
costdat$value <- as.numeric(costdat$value)

gg <- costdat %>% group_by(region, index, crop, year) %>% 
  summarise(avg = ifelse(n() == 1, value, sum(value, na.rm = T)/sum(!is.na(value)))) %>% 
  arrange(region, index, crop, year) %>% ungroup()
gg$year <- as.numeric(gg$year)

# 18 provinces, 4 indexes, 4 crops, 21 years 
newcostdat <- data.frame(region = rep(unique(gg$region), each = 4 * 4 * 21),
                         index = rep(rep(unique(gg$index), each = 4 * 21), 18), 
                         crop = rep(rep(unique(gg$crop), each = 21), 18 * 4), 
                         year = rep(2001:2021, 18 * 4 * 4))
newcostdat <- left_join(newcostdat, gg)


# impute yield and price 

avgcost <- readxl::read_xls("/Users/sunhao/Documents/GitHub/ChinaAg/From Hao/rawdata/update/costavg0821.xls")
names(avgcost) <- c("index", "region", "crop", "year", "avgvalue")

avgcost <- avgcost %>% filter(index %in% c("主产品产量（千克）", "土地成本（元）", "平均出售价格（元）", "物质与服务费用（元）"))
avgcost$index <- factor(avgcost$index, labels = c("yield", "landcost", "price", "servicecost"))

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

# update 2023-09-03
costdat <- newcostdat %>% select(-avgvalue) %>% spread(key = index, value = avg) %>% 
  mutate(cost = (landcost + servicecost)/yield) %>% select(-servicecost, -landcost)



### Data 2: Acreage Information #### 
rawacre <- readxl::read_xls("/Users/sunhao/Documents/GitHub/ChinaAg/From Hao/rawdata/update/acre0821.xls")

colnames(rawacre) <- c("index", "region", "year", "value")
# region

rawacre$region <- factor(rawacre$region, 
                         labels = c("Yunnan", "Neimenggu", "Jilin", "Sichuan", "Anhui", "Shandong", "Shanxi", "Guangxi",
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

# add land fertility protection subsidies (for V2)
subsidy$subsidy[subsidy$year == 2015] <- subsidy$subsidy[subsidy$year == 2015] + 57.58
subsidy$subsidy[subsidy$year == 2016] <- subsidy$subsidy[subsidy$year == 2016] + 71.45
subsidy$subsidy[subsidy$year == 2017] <- subsidy$subsidy[subsidy$year == 2017] + 71.50
subsidy$subsidy[subsidy$year == 2018] <- subsidy$subsidy[subsidy$year == 2018] + 71.70
subsidy$subsidy[subsidy$year == 2019] <- subsidy$subsidy[subsidy$year == 2019] + 72.41
subsidy$subsidy[subsidy$year == 2020] <- subsidy$subsidy[subsidy$year == 2020] + 56.72
subsidy$subsidy[subsidy$year == 2021] <- subsidy$subsidy[subsidy$year == 2021] + 56.72

multidat <- multidat %>% left_join(., subsidy)
multidat$subsidy[is.na(multidat$subsidy)] <- 0

# we only consider subsidy in the northeast China  (update 2023-09-03)
multidat$subsidy[!multidat$region %in% c("Heilongjiang", "Liaoning", "Jilin", "Neimenggu")] <- 0

# change cost unit to yuan/kg (update 2023-09-03)
# multidat$cost <- multidat$cost/50    # no need now 

# important! arrange before we calculate lag 
multidat <- multidat %>% arrange(region, crop)


# transform price to unit price 
multidat <- multidat %>% mutate(price = price/yield)

multidat <- multidat %>% 
  mutate(profit_true = yield * (price - cost) + subsidy)

multidat$yield_lag <- get_lag(multidat, "yield", 3, weight = c(0.34, 0.33, 0.33))
multidat$subsidy_lag <- get_lag(multidat, "subsidy", 2, weight = c(0.67, 0.33))

multidat <- multidat %>% mutate(profit_ex = yield_lag * (ex_price - cost) )   # now not include subsidy ! # + subsidy_lag

# 3. add lag profits as two covariates 

multidat$profit_ex_lag <- get_lag(multidat, "profit_ex", 1, weight = 1)
multidat$profit_true_lag <- get_lag(multidat, "profit_true", 1, weight = 1)

# arrange the data set 
multidat <- multidat %>% arrange(region, crop, year) 


# create the necessary variables 

multidat <- multidat %>% 
  mutate(pfex_corn = multidat %>% filter(crop == "corn") %>% "$"(profit_ex) %>% 
           matrix(., nrow = 21) %>% # "+"(400) %>% log() %>% 
           kronecker(rep(1, 4), .) %>% as.vector(), 
         pfex_rice = multidat %>% filter(crop == "rice") %>% "$"(profit_ex) %>% 
           matrix(., nrow = 21) %>% # "+"(400) %>% log() %>% 
           kronecker(rep(1, 4), .) %>% as.vector(), 
         pfex_soy = multidat %>% filter(crop == "soybean") %>% "$"(profit_ex) %>% 
           matrix(., nrow = 21) %>% # "+"(400) %>% log() %>% 
           kronecker(rep(1, 4), .) %>% as.vector(), 
         pfex_wheat = multidat %>% filter(crop == "wheat") %>% "$"(profit_ex) %>% 
           matrix(., nrow = 21) %>% # "+"(400) %>% log() %>% 
           kronecker(rep(1, 4), .) %>% as.vector()
  ) %>% 
  mutate(
    subsidy_corn = multidat %>% filter(crop == "corn") %>% "$"(subsidy) %>% 
      matrix(., nrow = 21) %>% kronecker(rep(1, 4), .) %>% as.vector(), 
    subsidy_rice = multidat %>% filter(crop == "rice") %>% "$"(subsidy) %>% 
      matrix(., nrow = 21) %>% kronecker(rep(1, 4), .) %>% as.vector(), 
    subsidy_soy = multidat %>% filter(crop == "soybean") %>% "$"(subsidy) %>% 
      matrix(., nrow = 21) %>% kronecker(rep(1, 4), .) %>% as.vector()
  )

multidat$share_lag <- get_lag(multidat, "share", 1, weight = 1)

multidat$share[multidat$share == 0] <- 0.1

multidat$policy <- multidat$subsidy > 0

multidat <- multidat %>% 
  mutate(share_wheat = multidat %>% filter(crop == "wheat") %>% "$"(share) %>% 
           matrix(., nrow = 21) %>% 
           kronecker(rep(1, 4), .) %>% as.vector()) %>% 
  mutate(y2 = log(share/(other + share_wheat)))           # y2 is the relative share 


preddat <- multidat %>% filter(year == 2021)
regdat <- multidat %>% filter(year < 2021)

saveRDS(regdat, "From Hao/data/18provinces/regdat_18prov.rds")
saveRDS(preddat, "From Hao/data/18provinces/preddat_18prov.rds")

write.csv(multidat, "From Hao/data/18provinces/alldata.csv")
