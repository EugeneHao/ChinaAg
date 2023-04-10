# focus on Dongbei

library(tidyverse)

### Data 1: Cost Data  (including yield, sale price, cost) ####

rawcost <- readxl::read_xls("/Users/sunhao/Documents/GitHub/ChinaAg/From Hao/rawdata/AgCost0408.xls")
names(rawcost) <- c("index", "region", "crop", "year", "value")

rawcost$region <- factor(rawcost$region, labels = c("Neimenggu", "Jilin", "Shandong", "Hebei",
                                        "Henan", "Liaoning", "Heilongjiang"))
# Note: Henan only keep 中籼稻 
costdat <- rawcost %>% filter(index %in% c("主产品产量（千克）", "平均出售价格（元）", "总成本（元）"), 
                              region != "Henan")
costdat$index <- factor(costdat$index, labels = c("yield", "price", "cost"))

costdat$crop <- factor(costdat$crop, labels = c("soybean", "wheat", "corn", "rice"))

costdat <- costdat %>% spread(key = index, value = value) 

# impute the missing values for yield and price 
costdat$yield <- as.numeric(costdat$yield)
costdat$price <- as.numeric(costdat$price)
costdat$cost <- as.numeric(costdat$cost)
costdat$year <- as.numeric(costdat$year)

for(i in 1:nrow(costdat))
{
  if(is.na(costdat$yield[i]))
  {
    indextmp <- c(ifelse(costdat$year[i]==2001, NA, i-1), 
                  ifelse(costdat$year[i]==2021, NA, i+1))
    indextmp <- indextmp[!is.na(indextmp)]
    dattmp <- costdat[indextmp,]
    costdat$yield[i] <- mean(dattmp$yield)
  } 
  if(is.na(costdat$price[i]))
  {
    indextmp <- c(ifelse(costdat$year[i]==2001, NA, i-1), 
                  ifelse(costdat$year[i]==2021, NA, i+1))
    indextmp <- indextmp[!is.na(indextmp)]
    dattmp <- costdat[indextmp,]
    costdat$price[i] <- mean(dattmp$price)
  } 
}

# impute cost (we do not have first 3 years' cost for all the crops and 1 futher cost in 2018)

for(j in c("Neimenggu", "Jilin", "Shandong", "Hebei", "Liaoning", "Heilongjiang"))
  for(i in unique(costdat %>% filter(region == j) %>% "$"(crop)))
  {
    tmpdat <- costdat %>% filter(region == j, crop == i)
    if(is.na(tmpdat$cost[1]))  # 2001 cost is missing 
    {
      lm_cost <- lm(cost ~ year, tmpdat)
      costdat$cost[costdat$region == j & costdat$crop == i & costdat$year %in% 2001:2003] <- 
        predict(lm_cost, newdata = costdat %>% filter(crop == i, region == j, year %in% 2001:2003))
    }
  }

tmp <- which(is.na(costdat$cost))
costdat$cost[is.na(costdat$cost)] <- mean(costdat$cost[c(tmp - 1, tmp + 1)])

# Note that in raw data, the unit of price and cost are yuan/50kg
costdat <- costdat %>% 
  mutate(price = price/50 * yield, cost = cost/50 * yield)

# Jilin and Liaoning do not have cost information for wheat 
jilin_wheat <- costdat %>% filter(region == "Heilongjiang", crop == "wheat") %>% 
  mutate(region = "Jilin")
liaoning_wheat <- costdat %>% filter(region == "Neimenggu", crop == "wheat") %>% 
  mutate(region = "Liaoning")

costdat <- rbind(costdat, jilin_wheat, liaoning_wheat)

### Data 2: Acreage Information #### 
rawacre <- readxl::read_xls("/Users/sunhao/Documents/GitHub/ChinaAg/From Hao/rawdata/PlantAcre0409.xls")

colnames(rawacre) <- c("index", "region", "year", "value")
# region
rawacre$region <- factor(rawacre$region, labels = c("Neimenggu", "Jilin", "Shandong", "Hebei",
                                                    "Henan", "Liaoning", "Heilongjiang"))
# filter 

acredat <- rawacre %>% filter(region != "Henan", index != "耕地面积（千公顷）")
# index 
acredat$index <- factor(acredat$index, labels = c("total", "soybean", "wheat", "corn", "rice"))

acredat <- acredat %>% spread(key = index, value = value)
acredat$year <- as.numeric(acredat$year)
acredat$total <- as.numeric(acredat$total)
acredat$soybean <- as.numeric(acredat$soybean)
acredat$wheat <- as.numeric(acredat$wheat)
acredat$corn <- as.numeric(acredat$corn)
acredat$rice <- as.numeric(acredat$rice)

for(i in 1:nrow(acredat))
{
  tmpyear = acredat$year[i]
  tmpreg = acredat$region[i]
  if(is.na(acredat$wheat[i]))
  {
    if(tmpyear == 2001)
    {
      acredat$wheat[i] <- acredat$wheat[i+1]
    } else if (tmpyear == 2021 & tmpreg !=  "Shandong")
    {
      acredat$wheat[i] <- acredat$wheat[i-1]
    } else
    {
      acredat$wheat[i] <- 0
    }
  }
}

# we do not need 2000 data right now 
acredat <- acredat %>% filter(year != 2000) 

# compute share 
acredat <- acredat %>% mutate(soybean_share = round(100 * soybean/total, 2), 
                              wheat_share = round(100 * wheat/total, 2), 
                              corn_share = round(100 * corn/total, 2), 
                              rice_share = round(100 * rice/total, 2))

# combine the two data sets 
multidat <- left_join(costdat, acredat %>% select(region, year, soybean_share, wheat_share, corn_share, rice_share) %>% 
  gather(key = crop, value = share, -region, -year) %>% 
  mutate(crop = factor(crop, labels = c("corn", "rice", "soybean", "wheat"))))

### Data 3: Future Price #### 
fuprice <- readRDS("data/heilongjiang/future_price.rds") %>% 
  filter(month == "03") %>% select(year, crop, ex_price) %>% 
  mutate(year = as.numeric(year))

my_fuprice <- data.frame(year = rep(2001:2021, 4), 
                         crop = rep(c("corn", "rice", "soybean", "wheat"), each = 21)) %>% 
  left_join(., fuprice)

# load the price data 
price <- readxl::read_xls("rawdata/全国平均价格.xls")

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
  lm_exprice <- lm(ex_price ~ price, dat = my_fuprice %>% filter(crop == i))
  my_fuprice$ex_price[is.na(my_fuprice$ex_price) & (my_fuprice$crop == i)] <- 
    predict(lm_exprice, newdata = my_fuprice %>% filter(crop == i, is.na(ex_price)))
}

names(my_fuprice)[4] <- "national"

multidat <- multidat %>% left_join(., my_fuprice, by = c("crop", "year")) 

### Data 4: subsidy   #### 

subsidy <- readRDS("data/heilongjiang/dat.rds") %>% select(crop, year, subsidy_mu) %>% 
  "colnames<-"(c("crop", "year", "subsidy"))

multidat <- multidat %>% left_join(., subsidy)
multidat$subsidy[is.na(multidat$subsidy)] <- 0

# change cost unit to yuan/kg 
multidat$cost <- multidat$cost/multidat$yield


