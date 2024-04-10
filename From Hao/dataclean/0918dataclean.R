library(tidyverse)
library("AER")
setwd("/Users/xiaolanwan/Documents/GitHub/ChinaAg/")

# acre
acre <- readxl::read_xls("Xiaolan/raw data/try.xls")
names(acre) <- c("index", "region", "year", "value")
acre$region <- factor(acre$region, 
                           labels = c("Yunnan","Nation", "Inner Mongolia", "Jilin", "Sichuan", "Anhui", "Shandong", "Shanxi", "Guangxi",
                                      "Xinjiang", "Jiangsu", "Hebei", "Henan", "Hubei", "Gansu", "Guizhou", "Liaoning",
                                      "Shaanxi", "Heilongjiang"))
acre$year <- as.numeric(acre$year)
acre$value <- as.numeric(acre$value)
acre <- acre %>% tidyr::spread(key = index, value) 
names(acre) <- c("region", "year", "acre_total","acre_soy","acre_wheat",
                 "acre_oil", "acre_corn", "acre_fruit", "acre_rice", "acre_state","acre_vege")
acre <- acre %>% mutate(share_soy = acre_soy/acre_total,
                share_wheat = acre_wheat/acre_total,
                share_oil = acre_oil/acre_total,
                share_corn = acre_corn/acre_total,
                share_fruit = acre_fruit/acre_total,
                share_rice = acre_rice/acre_total,
                share_vege = acre_vege/acre_total)

acre <- acre %>% mutate(stateprop = acre_state/acre_total * 100) 
acre$policy <- ifelse(acre$year > 2015 & acre$year < 2021, 1, 0)
acre <- acre %>% mutate(location = case_when(
  region %in% c("Heilongjiang", "Jilin", "Liaoning", "Inner Mongolia") ~ 1,
  region %in% c("Henan", "Hebei", "Shandong", "Anhui") ~ 2,
  region %in% c("Nation") ~ 0,
TRUE ~ 3
))


nationcrop <- acre %>% filter(region == "Nation") %>% select(year, `acre_soy`:`acre_vege`) %>% as.data.frame()
acre$nation_corn <- rep(nationcrop$acre_corn, times = 19)
acre$nation_soy <- rep(nationcrop$acre_soy, times = 19)
acre$nation_rice <- rep(nationcrop$acre_rice, times = 19)
acre$nation_wheat <- rep(nationcrop$acre_wheat, times = 19)
acre$nation_vege <- rep(nationcrop$acre_vege, times = 19)
acre$nation_oil <- rep(nationcrop$acre_oil, times = 19)
acre$nation_fruit <- rep(nationcrop$acre_fruit, times = 19)

acre<- acre %>% mutate(corn_weight = acre_corn/nation_corn,
                soy_weight = acre_soy/nation_soy,
                rice_weight = acre_rice/nation_rice,
                wheat_weight = acre_wheat/nation_wheat,
                vege_weight = acre_vege/nation_vege,
                oil_weight = acre_oil/nation_oil,
                fruit_weight = acre_fruit/nation_fruit)


acre <- acre %>% mutate(bigSOF = ifelse(acre_state > 30, 1, 0))

#rank region
rank_order <- c("Nation", "Heilongjiang", "Inner Mongolia", "Jilin","Liaoning",
                "Shandong", "Henan", "Hebei", "Anhui", "Jiangsu", "Shanxi", "Shaanxi",
                "Sichuan", "Yunnan","Guizhou","Guangxi","Hubei", "Gansu","Xinjiang")
acre$region <- factor(acre$region, levels = rank_order)

#soybean policy
acre$policy_soy <- rep(c(0,0,0,0,0,0,1,1,1,1,1,0), times = 19)
acre$target_soy <- rep(c(0,0,0,0,0,0,1131,565,283,616,304,0), times = 19)

acre$policy16 <- rep(c(0,0,0,0,0,0,1,1,1,1,1,0), times = 19)
acre$policy19 <- rep(c(0,0,0,0,0,0,0,0,0,1,1,0), times = 19)
acre$policy21 <- rep(c(0,0,0,0,0,0,0,0,0,0,0,-1), times = 19)



#city-level acreage of corn & soybean over the seven provinces over 2010-2021

#heilongjiang
hlj <- readxl::read_xls("Xiaolan/raw data/citydata/heilongjiang/hlj.xls")
names(hlj) <- c("index", "region", "year", "value")
hlj$year <- as.numeric(hlj$year)
hlj$value <- as.numeric(hlj$value)

#merge with missing find
hljmiss <- readxl::read_xls("Xiaolan/raw data/citydata/heilongjiang/hljmissing.xls")
names(hljmiss) <- c("index", "region", "year", "value")
hljmiss$year <- as.numeric(hljmiss$year)
hljmiss$value <- as.numeric(hljmiss$value)

##join missing
hlj <- hlj %>% left_join(hljmiss, by = c("index", "region", "year"), suffix = c("_A", "_B")) %>% 
  mutate(value = coalesce(value_A, value_B)) %>% 
  select(index, region, year, value)

##wide format
hlj <- hlj %>% tidyr::spread(key = index, value) 
names(hlj) <- c("region", "year", "acre_soy_ha","acre_wheat_ha","acre_total_ha",
                 "acre_oil_ha", "acre_corn_ha", "acre_fruit_ha", "acre_rice_ha", "acre_vege_ha")
hlj$province <- rep("Heilongjiang", times = 195)


##translate regions: attention to order!
unique(hlj$region)
trans <- c("qitaihe", "yichun", "jiamusi", "shuangyashan", "haerbin","daxinganling",
           "daqing","mudanjiang","suihua","jixi","hegang","heihe","qiqihaer")
hlj$city <- rep(trans, each = 15)


#change unit to "千公顷" 
hlj <- hlj %>% mutate(acre_soy = acre_soy_ha/1000,
               acre_wheat = acre_wheat_ha/1000,
               acre_total = acre_total_ha/1000,
               acre_oil = acre_oil_ha/1000,
               acre_corn = acre_corn_ha/1000,
               acre_fruit = acre_fruit_ha/1000,
               acre_rice = acre_rice_ha/1000,
               acre_vege = acre_vege_ha/1000)

#filter small corn acreage
hlj %>% group_by(city) %>% summarise(corn_acre = mean(acre_corn),
                                     soy_acre = mean(acre_soy)) %>% arrange(corn_acre)
#daxinganling is the smallest in corn, and can be removed

#compute acreage share
hlj <- hlj %>% mutate(share_soy = acre_soy/acre_total,
                        share_wheat = acre_wheat/acre_total,
                        share_oil = acre_oil/acre_total,
                        share_corn = acre_corn/acre_total,
                        share_fruit = acre_fruit/acre_total,
                        share_rice = acre_rice/acre_total,
                        share_vege = acre_vege/acre_total)



##check HLJ data validity
ggplot(data = hlj, aes(x = as.factor(year), y = share_corn, color = city))+
  geom_line(group = 1) +
  facet_wrap(~city) #corn

ggplot(data = hlj, aes(x = as.factor(year), y = share_soy, color = city))+
  geom_line(group = 1) +
  facet_wrap(~city) #soybean

#2017 soybean acre unit is (mu)

## select variables
hlj <- hlj %>% select(province, city, year, region, acre_total,acre_corn,acre_soy,share_corn,share_soy)


#Inner mongolia
nmg <- readxl::read_xls("Xiaolan/raw data/citydata/neimenggu/nmg.xls")
nmg_yield <- readxl::read_xls("Xiaolan/raw data/citydata/neimenggu/yield.xls")

names(nmg) <- c("index", "region", "year", "value")
names(nmg_yield) <- c("index", "region", "year", "value")


nmg$year <- as.numeric(nmg$year)
nmg$value <- as.numeric(nmg$value)

nmg_yield$year <- as.numeric(nmg_yield$year)
nmg_yield$value <- as.numeric(nmg_yield$value)

nmg <- nmg %>% tidyr::spread(key = index, value) 
nmg_yield <- nmg_yield %>% tidyr::spread(key = index, value) 

names(nmg) <- c("region","year","acre_total","prod_wheat","prod_oil","prod_corn","prod_bean")
names(nmg_yield) <- c("region", "year", "soyacre","corn_yield","bean_yield","beanacre")

nmg_yield <- nmg_yield %>% select(-region)

#compute acre share:
nmg <- nmg %>% left_join(nmg_yield, by = c("year")) 

nmg <- nmg %>% 
  mutate(acre_corn = prod_corn/corn_yield*10000,
         acre_bean = prod_bean/bean_yield*10000,
         acre_soy = acre_bean * soyacre/beanacre,
         share_corn = acre_corn/acre_total,
         share_soy = acre_soy/acre_total)




##translate city names
unique(nmg$region)

trans <- c("wulanchabu", "wuhai", "xingan", "baotou", "hulunbeier","huhehaote",
           "bayanzhuoer","chifeng","tongliao","eerduosi","xilinguole","alashanmeng")
nmg$city <- rep(trans, each = 15)

nmg$province <- rep("Inner mongolia", times = 180)

nmg <- nmg %>% select(province, city, year, region, acre_total, acre_corn, acre_soy, share_corn, share_soy) %>% 
  filter(city != "alashanmeng")   #filter out small city




#check data validity
nmg %>% group_by(city) %>% summarise(corn_acre = mean(acre_corn),
                                     soy_acre = mean(acre_soy)) %>% arrange(corn_acre)

ggplot(data = nmg, aes(x = as.factor(year), y = share_corn, color = city))+
  geom_line(group = 1) +
  facet_wrap(~city) #corn

ggplot(data = nmg, aes(x = as.factor(year), y = share_soy, color = city))+
  geom_line(group = 1) +
  facet_wrap(~city) #corn

## Jilin
jl <- readxl::read_xls("Xiaolan/raw data/citydata/jilin/jl.xls")
names(jl) <- c("index", "region", "year", "value")
jl$year <- as.numeric(jl$year)
jl$value <- as.numeric(jl$value)

##wide format
jl <- jl %>% tidyr::spread(key = index, value) 
names(jl) <- c("region", "year", "acre_soy_ha","acre_total_ha", "acre_rice_ha",
                "acre_oil_ha", "acre_corn_ha", "acre_fruit_ha",  "acre_vege_ha")
jl$province <- rep("Jilin", times = 135)


##translate regions: attention to order!
unique(jl$region)
trans <- c("jilin", "siping", "yanbian", "songyuan", "baicheng","baishan",
           "liaoyuan","tonghua","changchun")
jl$city <- rep(trans, each = 15)


#change unit to "千公顷" 
jl <- jl %>% mutate(acre_soy = acre_soy_ha/1000,
                      acre_total = acre_total_ha/1000,
                      acre_oil = acre_oil_ha/1000,
                      acre_corn = acre_corn_ha/1000,
                      acre_fruit = acre_fruit_ha/1000,
                      acre_rice = acre_rice_ha/1000,
                      acre_vege = acre_vege_ha/1000)

#compute acreage share
jl <- jl %>% mutate(share_soy = acre_soy/acre_total,
                      share_oil = acre_oil/acre_total,
                      share_corn = acre_corn/acre_total,
                      share_fruit = acre_fruit/acre_total,
                      share_rice = acre_rice/acre_total,
                      share_vege = acre_vege/acre_total)

#check data validity
jl %>% group_by(city) %>% summarise(corn_acre = mean(acre_corn),
                                     soy_acre = mean(acre_soy)) %>% arrange(corn_acre)


ggplot(data = jl, aes(x = as.factor(year), y = share_corn, color = city))+
  geom_line(group = 1) +
  facet_wrap(~city) #corn

ggplot(data = jl, aes(x = as.factor(year), y = share_soy, color = city))+
  geom_line(group = 1) +
  facet_wrap(~city) #corn


#select variables
jl <- jl %>% select(province, city, year, region, acre_total,acre_corn,acre_soy,share_corn,share_soy)


#Liaoning
ln <- readxl::read_xls("Xiaolan/raw data/citydata/liaoning/ln.xls")
names(ln) <- c("index", "region", "year", "value")
ln$year <- as.numeric(ln$year)
ln$value <- as.numeric(ln$value)

##wide format
ln <- ln %>% tidyr::spread(key = index, value)
names(ln) <- c("region", "year","acre_total", "acre_soy","acre_wheat","acre_rice",
                "acre_oil", "acre_corn","acre_vege")
ln$province <- rep("Liaoning", times = 210)

##translate regions: attention to order!
unique(ln$region)
trans <- c("dandong", "dalian", "fushun", "chaoyang", "benxi","shenyang",
           "panjin","yingkou","huludao","liaoyang","tieling","jinzhou","buxin","anshan")
ln$city <- rep(trans, each = 15)

#compute acreage share
ln <- ln %>% mutate(share_soy = acre_soy/acre_total,
                      share_wheat = acre_wheat/acre_total,
                      share_oil = acre_oil/acre_total,
                      share_corn = acre_corn/acre_total,
                      share_rice = acre_rice/acre_total,
                      share_vege = acre_vege/acre_total)


##check data validity
###filter small corn acreage
ln %>% group_by(city) %>% summarise(corn_acre = mean(acre_corn),
                                     soy_acre = mean(acre_soy)) %>% arrange(corn_acre)

ggplot(data = ln, aes(x = as.factor(year), y = share_corn, color = city))+
  geom_line(group = 1) +
  facet_wrap(~city) #corn


ggplot(data = ln, aes(x = as.factor(year), y = share_soy, color = city))+
  geom_line(group = 1) +
  facet_wrap(~city) #soy

## select variables
ln <- ln %>% select(province, city, year, region, acre_total,acre_corn,acre_soy,share_corn,share_soy)



#Henan
hn <- readxl::read_xls("Xiaolan/raw data/citydata/henan/hn.xls")
names(hn) <- c("index", "region", "year", "value")
hn$year <- as.numeric(hn$year)
hn$value <- as.numeric(hn$value)


#merge with missing find
hnmiss <- readxl::read_xls("Xiaolan/raw data/citydata/henan/hnmiss.xls")
names(hnmiss) <- c("index", "region", "year", "value")
hnmiss$year <- as.numeric(hnmiss$year)
hnmiss$value <- as.numeric(hnmiss$value)

##join missing
hn <- hn %>% left_join(hnmiss, by = c("index", "region", "year"), suffix = c("_A", "_B")) %>%
  mutate(value = coalesce(value_A, value_B)) %>%
  select(index, region, year, value)


##wide format
hn <- hn %>% tidyr::spread(key = index, value)
names(hn) <- c("region", "year", "acre_total", "acre_soy","acre_wheat",
                "acre_oil", "acre_corn", "acre_fruit", "acre_rice", "acre_vege")
hn$province <- rep("Henan", times = 270)


##translate regions: attention to order!
unique(hn$region)
trans <- c("sanmenxia", "xinyang", "nanyang", "zhoukou", "shangqiu","anyang",
           "pingdingshan","kaifeng","xinxiang","luoyang","jiyuan","luohe","puyang",
           "jiaozuo","xuchang","zhengzhou","zhumadian","hebi")
hn$city <- rep(trans, each = 15)

#compute acreage share
hn <- hn %>% mutate(share_soy = acre_soy/acre_total,
                      share_wheat = acre_wheat/acre_total,
                      share_oil = acre_oil/acre_total,
                      share_corn = acre_corn/acre_total,
                      share_fruit = acre_fruit/acre_total,
                      share_rice = acre_rice/acre_total,
                      share_vege = acre_vege/acre_total)



##check data validity
###filter small corn acreage
hn %>% group_by(city) %>% summarise(corn_acre = mean(acre_corn),
                                     soy_acre = mean(acre_soy)) %>% arrange(corn_acre)

ggplot(data = hn, aes(x = as.factor(year), y = share_corn, color = city))+
  geom_line(group = 1) +
  facet_wrap(~city) #corn

ggplot(data = hn, aes(x = as.factor(year), y = share_soy, color = city))+
  geom_line(group = 1) +
  facet_wrap(~city) #soybean

## select variables
hn <- hn %>% select(province, city, year, region, acre_total,acre_corn,acre_soy,share_corn,share_soy)


#Shandong
sd <- readxl::read_xls("Xiaolan/raw data/citydata/shandong/sd.xls")
names(sd) <- c("index", "region", "year", "value")
sd$year <- as.numeric(sd$year)
sd$value <- as.numeric(sd$value)


##wide format
sd <- sd %>% tidyr::spread(key = index, value)
names(sd) <- c("region", "year","acre_total_ha", "acre_soy_ha","acre_wheat_ha",
                "acre_oil_ha", "acre_corn_ha", "acre_fruit_ha", "acre_rice_ha", "acre_vege_ha")
sd$province <- rep("Shandong", times = 255)


##translate regions: attention to order!
unique(sd$region)
trans <- c("dongying", "linyi", "weihai", "dezhou", "rizhao","zaozhuang",
           "taian","jinan","jining","zibo","binzhou","weifang","yantai",
           "liaocheng","laiwu","heze","qingdao")
sd$city <- rep(trans, each = 15)

#change unit to "千公顷"
sd <- sd %>% mutate(acre_soy = acre_soy_ha/1000,
                      acre_wheat = acre_wheat_ha/1000,
                      acre_total = acre_total_ha/1000,
                      acre_oil = acre_oil_ha/1000,
                      acre_corn = acre_corn_ha/1000,
                      acre_fruit = acre_fruit_ha/1000,
                      acre_rice = acre_rice_ha/1000,
                      acre_vege = acre_vege_ha/1000)

#compute acreage share
sd <- sd %>% mutate(share_soy = acre_soy/acre_total,
                      share_wheat = acre_wheat/acre_total,
                      share_oil = acre_oil/acre_total,
                      share_corn = acre_corn/acre_total,
                      share_fruit = acre_fruit/acre_total,
                      share_rice = acre_rice/acre_total,
                      share_vege = acre_vege/acre_total)

##check data validity

###filter small corn acreage
sd %>% group_by(city) %>% summarise(corn_acre = mean(acre_corn),
                                     soy_acre = mean(acre_soy)) %>% arrange(corn_acre)


ggplot(data = sd, aes(x = as.factor(year), y = share_corn, color = city))+
  geom_line(group = 1) +
  facet_wrap(~city) #corn

ggplot(data = sd, aes(x = as.factor(year), y = share_soy, color = city))+
  geom_line(group = 1) +
  facet_wrap(~city) #soybean

## select variables
sd <- sd %>% select(province, city, year, region, acre_total,acre_corn,acre_soy,share_corn,share_soy)


##Hebei
hb <- readxl::read_xls("Xiaolan/raw data/citydata/hebei/hb.xls")
names(hb) <- c("index", "region", "year", "value")
hb$year <- as.numeric(hb$year)
hb$value <- as.numeric(hb$value)
hb <- hb %>% tidyr::spread(key = index, value)
names(hb) <- c("region","year","acre_total","acre_soy","acre_oil","acre_corn",
              "acre_vege")


#compute acre share:
hb <- hb %>%
  mutate(
         share_corn = acre_corn/acre_total,
         share_soy = acre_soy/acre_total,
         share_oil = acre_oil/acre_total,
         share_vege = acre_vege/acre_total
         )


##translate city names
unique(hb$region)
trans <- c("baoding", "tangshan", "langfang", "zhangjiakou", "chengde","cangzhou",
           "shijiazhuang","qinhuangdao","hengshui","xingtai","handan")
hb$city <- rep(trans, each = 15)
hb$province <- rep("Hebei", times = 165)



##check data validity
###filter small corn acreage
hb %>% group_by(city) %>% summarise(corn_acre = mean(acre_corn),
                                     soy_acre = mean(acre_soy, na.rm = T)) %>% arrange(corn_acre)

ggplot(data = hb, aes(x = as.factor(year), y = share_corn, color = city))+
  geom_line(group = 1) +
  facet_wrap(~city) #corn

ggplot(data = hb, aes(x = as.factor(year), y = share_soy, color = city))+
  geom_line(group = 1) +
  facet_wrap(~city) #soybean


## select variables
hb <- hb %>% select(province, city, year, region, acre_total,acre_corn,acre_soy,share_corn,share_soy)


##join data
city_acreage <- rbind(hlj,nmg,jl,ln,sd,hn,hb)  %>% as.data.frame()

write.csv(city_acreage, "Xiaolan/data clean/city_acreage.csv")


#State-own farm share
acre <- readxl::read_xls("Xiaolan/raw data/try.xls")
names(acre) <- c("index", "region", "year", "value")
acre <- acre %>% filter(region %in% c("黑龙江","内蒙古","吉林","辽宁","河南","河北","山东","全国"))
acre$year <- as.numeric(acre$year)
acre$value <- as.numeric(acre$value)
acre <- acre %>% tidyr::spread(key = index, value)

names(acre) <- c("region", "year", "acre_total","acre_soy","acre_wheat",
                 "acre_oil", "acre_corn", "acre_fruit", "acre_rice", "acre_state","acre_vege")


unique(acre$region)
acre$region <- factor(acre$region,
                      labels = c("Nation", "Inner mongolia","Jilin","Shandong","Hebei", "Henan","Liaoning","Heilongjiang"))
#orders!

#compute crop weight
nationcrop <- acre %>% filter(region == "Nation") %>% 
  select(year, acre_soy:acre_vege) %>% as.data.frame()

acre$nation_corn <- rep(nationcrop$acre_corn, times = 8)
acre$nation_soy <- rep(nationcrop$acre_soy, times = 8)
acre$nation_rice <- rep(nationcrop$acre_rice, times = 8)
acre$nation_wheat <- rep(nationcrop$acre_wheat, times = 8)
acre$nation_vege <- rep(nationcrop$acre_vege, times = 8)
acre$nation_oil <- rep(nationcrop$acre_oil, times = 8)
acre$nation_fruit <- rep(nationcrop$acre_fruit, times = 8)

acre<- acre %>% mutate(corn_weight = acre_corn/nation_corn,
                       soy_weight = acre_soy/nation_soy,
                       rice_weight = acre_rice/nation_rice,
                       wheat_weight = acre_wheat/nation_wheat,
                       vege_weight = acre_vege/nation_vege,
                       oil_weight = acre_oil/nation_oil,
                       fruit_weight = acre_fruit/nation_fruit)

#state_own farmland ratio
acre <- acre %>% mutate(stateprop = acre_state/acre_total * 100)

#policy indicator 
acre$policy_soy <- rep(c(0,0,0,0,0,0,1,1,1,1,1,0), times = 8) #here exist 8 regions
acre$target_soy <- rep(c(0,0,0,0,0,0,1131,565,283,616,304,0), times = 8)
acre$policy16 <- rep(c(0,0,0,0,0,0,1,1,1,1,1,0), times = 8)
acre$policy19 <- rep(c(0,0,0,0,0,0,0,0,0,1,1,0), times = 8)
acre$policy21 <- rep(c(0,0,0,0,0,0,0,0,0,0,0,-1), times = 8)

#select variables
colnames(acre)[1]<- "province"

acre <- acre %>% select(province, year, corn_weight, soy_weight, acre_state, stateprop,
                        policy_soy,target_soy,policy16,policy19,policy21) %>% 
  filter(province != "Nation") %>% 
  as.data.frame()

#national price
price <- readxl::read_xls("From Hao/rawdata/全国平均价格.xls")
names(price) <- c("index", "region", "crop", "year", "price")
price$year <- as.numeric(price$year)
price$price <- as.numeric(price$price)

price <- price %>% select(-region) %>% tidyr::spread(key = crop, price) %>% filter(year > 2009) 
names(price) <- c("index", "year","soy","wheat","corn", "rice")
price <- price %>% select(c(year,soy,corn)) %>% 
  mutate(soy_nationprice = soy/50,
         corn_nationprice = corn/50) %>% select(year, soy_nationprice,corn_nationprice) 

#merge data
cityclean <- readRDS("~/Documents/GitHub/ChinaAg/From Hao/data/citylevel/cityclean.rds")

citycomb <- cityclean %>% filter(year > 2009) %>% 
  left_join(acre, by = c("province","year")) %>% 
  left_join(price, by = "year")
  

#check data
acre %>% group_by(region) %>% summarise(weight_corn = mean(corn_weight),
                                        weight_soy = mean(soy_weight),
                                        weight_state = mean(stateprop))


#merge with clean dat
saveRDS(citycomb, "Xiaolan/data clean/citycomb.rds")

#compute per-mu profit
citycomb <- citycomb %>% mutate(corn_profit_gross = corn_sp_lag * corn_yield_lag - 
                      corn_labor_cost - corn_service_cost,
                    soy_profit_gross = soy_sp_lag * soy_yield_lag - 
                      soy_labor_cost - soy_service_cost,
                    corn_profit = corn_sp_lag * corn_yield_lag - 
                      corn_labor_cost - corn_service_cost - corn_land_cost,
                    soy_profit = soy_sp_lag * soy_yield_lag - 
                      soy_labor_cost - soy_service_cost - soy_land_cost,
                    corn_exprofit_gross = corn_futureprice * corn_yield_lag - 
                      corn_labor_cost - corn_service_cost,
                    soy_exprofit_gross = soy_futureprice * soy_yield_lag - 
                      soy_labor_cost - soy_service_cost,
                    corn_exprofit = corn_futureprice * corn_yield_lag - 
                      corn_labor_cost - corn_service_cost - corn_land_cost,
                    soy_exprofit = soy_futureprice * soy_yield_lag - 
                      soy_labor_cost - soy_service_cost - soy_land_cost
                    ) 


#preparing data for regression
soynation <- readRDS("~/Documents/GitHub/ChinaAg/Xiaolan/data clean/soynation.rds")

dat <- citycomb %>% mutate(policy_soyweight = soy_weight * policy_soy,
                           policy_nonsoyweight = soy_weight * (1-policy_soy),
                           relative_subsidy = soy_subsidy - corn_subsidy)

dat <- dat %>% mutate(soyhigh = ifelse(soy_weight > 0.08, 1, 0))

dat <- dat %>% mutate(soy_weight_city = acre_soy/acre_total,
                      corn_weight_city = acre_corn/acre_total,
                      policy_soyweight_city = soy_weight_city * policy_soy)

dat <- dat %>% mutate(policy_add = policy16 + policy19)

dat <- dat %>% mutate(soy_subsidy_kg = soy_subsidy/soy_yield,
                      corn_subsidy_kg = corn_subsidy/corn_yield,
                      soy_price_real = soy_saleprice + soy_subsidy_kg,
                      corn_price_real = corn_saleprice + corn_subsidy_kg,
                      soy_profit_exp = soy_saleprice + soy_subsidy_kg)

dat <- dat %>% mutate(sofhigh = ifelse(stateprop > 4, 1, 0))


dat %>% left_join(soynation, by = "year")
#check data

##descriptive summary
dat %>% group_by(province) %>% 
  summarise(corn_acre = mean(acre_corn, na.rm = T), 
            corn_share = mean(share_corn, na.rm = T),
            sd_corn = sd(share_corn, na.rm = T),
            soy_acre = mean(acre_soy, na.rm = T),
            soy_share = mean(share_soy, na.rm = T),
            sd_soy = sd(share_soy, na.rm = T)) %>% 
  arrange(desc(sd_corn))

##acreage share change by province
ggplot(data = dat, aes(x = year) ) + 
  geom_smooth(aes(y = share_soy, color = "soy")) + 
  geom_smooth(aes(y = share_corn, color = "corn"))+
  geom_point(aes(y = share_soy, color = "soy")) +
  geom_point(aes(y = share_corn, color = "corn")) +
  facet_wrap(~province)

##acreage share change in Heilongjiang
ggplot(data = dat %>% filter(province == "Heilongjiang"), aes(x = year) ) + 
  geom_smooth(aes(y = share_soy, color = "soy")) + 
  geom_smooth(aes(y = share_corn, color = "corn"))+
  geom_point(aes(y = share_soy, color = "soy")) +
  geom_point(aes(y = share_corn, color = "corn")) +
  facet_wrap(~city)

###share change per year
share_change <- dat %>% arrange(city, year) %>% 
  group_by(city) %>% 
  mutate(pct_soy = (share_soy/lag(share_soy)-1)*100,
         pct_corn = (share_corn/lag(share_corn)-1)*100
  ) 

ggplot(data = share_change %>% filter(province == "Heilongjiang")) +
  geom_line(aes(x = factor(year), y = pct_corn), color = "blue", group = 1) + 
  geom_col(aes(x = factor(year), y = pct_corn, fill = pct_corn > 0))+
  geom_point(aes(x = factor(year), y = pct_corn)) +
  facet_wrap(~city)+
  theme_classic() +
  xlab("Year")+
  ylab("% Change in Corn Share Per Year")+
  scale_fill_manual(
    values = c("TRUE" = "green", "FALSE" = "red"),
    labels = c("Negative", "Positive"),
    name = NULL
  )

##by soybean weight


##by state-owned farmland 
ggplot(data = dat %>% filter(province == "Heilongjiang"), aes(x = year)) + 
  geom_point(aes(y = share_soy, color = "soy")) + 
  geom_point(aes(y = share_corn, color = "corn"))+
  facet_wrap(~city)


dat %>% group_by(year) %>% 
  summarise(soy_price = mean(soy_price_real), 
            corn_price = mean(corn_price_real))





#fit tobit: province level

fit1 <- tobit(share_corn ~ corn_sp_lag + soy_sp_lag, data = acre,
              left = 0, right = 1)
summary(fit1)


#fit tobit
#soybean weight


#corn
tobit(share_corn ~ corn_sp_lag + soy_sp_lag + policy_soyweight +  policy_nonsoyweight, data = dat,
left = 0, right = 1) %>% summary()

tobit(share_corn ~ corn_sp_lag + soy_sp_lag + policy_soyweight + policy_soy , data = dat,
      left = 0, right = 1) %>% summary()

tobit(share_corn ~ corn_sp_lag + soy_sp_lag + policy_soyweight_city + policy_soy , data = dat,
      left = 0, right = 1) %>% summary()

tobit(share_corn ~ corn_sp_lag + soy_sp_lag + policy_soyweight_city , data = dat,
      left = 0, right = 1) %>% summary()

#this one:soy weight
tobit(share_corn ~ corn_sp_lag + soy_sp_lag + policy_soyweight_city, data = dat,
      left = 0, right = 1) %>% summary()

tobit(share_corn ~ corn_sp_lag + soy_sp_lag + I(soy_weight_city*policy16), data = dat,
      left = 0, right = 1) %>% summary()

tobit(share_corn ~ corn_sp_lag + soy_sp_lag + I(soy_weight_city*policy19), data = dat,
      left = 0, right = 1) %>% summary()

tobit(share_corn ~ corn_sp_lag + soy_sp_lag + I(soy_weight_city*policy_add), data = dat,
      left = 0, right = 1) %>% summary()

tobit(share_corn ~ corn_sp_lag + soy_sp_lag + I(soy_weight_city*target_soy), data = dat,
      left = 0, right = 1) %>% summary()

tobit(share_corn ~ corn_sp_lag + soy_sp_lag + I(soy_weight_city*soy_subsidy), data = dat,
      left = 0, right = 1) %>% summary()

###subgroup
tobit(share_corn ~ corn_sp_lag + soy_sp_lag + policy_soy, data = dat %>% filter(soyhigh == 1),
      left = 0, right = 1) %>% summary()

tobit(share_corn ~ corn_sp_lag + soy_sp_lag + policy_soy, data = dat %>% filter(soyhigh == 0),
      left = 0, right = 1) %>% summary()

#state own farmland
tobit(share_corn ~ corn_sp_lag + soy_sp_lag + policy_soyweight_city + stateprop , data = dat,
      left = 0, right = 1) %>% summary()

tobit(share_corn ~ corn_sp_lag + soy_sp_lag + policy_soyweight_city + I(policy_soy * stateprop) , data = dat,
      left = 0, right = 1) %>% summary()

tobit(share_corn ~ corn_sp_lag + soy_sp_lag + policy_soyweight_city * stateprop , data = dat,
      left = 0, right = 1) %>% summary()

tobit(share_corn ~ corn_sp_lag + soy_sp_lag + I(policy_soy * stateprop) , data = dat,
      left = 0, right = 1) %>% summary()


tobit(share_corn ~ corn_sp_lag + soy_sp_lag + policy_soyweight_city, data = dat %>% filter(sofhigh == 1),
      left = 0, right = 1) %>% summary()
tobit(share_corn ~ corn_sp_lag + soy_sp_lag + policy_soyweight_city, data = dat %>% filter(sofhigh == 0),
      left = 0, right = 1) %>% summary()


#control variables
tobit(share_corn ~ corn_profit_gross + soy_profit_gross + policy_soyweight_city, data = dat,
      left = 0, right = 1) %>% summary()


tobit(share_corn ~ I(corn_profit_gross + corn_subsidy) + I(soy_profit_gross + soy_subsidy)  + policy_soyweight_city, data = dat,
      left = 0, right = 1) %>% summary()

tobit(share_corn ~ corn_profit_gross  + soy_profit_gross  + policy_soyweight_city, data = dat,
      left = 0, right = 1) %>% summary()

#soybean
tobit(share_soy ~ corn_sp_lag + soy_sp_lag + policy_soyweight_city, data = dat,
      left = 0, right = 1) %>% summary()

tobit(share_soy ~ corn_price_real + soy_price_real + policy_soyweight_city, data = dat,
      left = 0, right = 1) %>% summary()

#profit with subsidy
tobit(share_corn ~ corn_profit_gross + soy_profit_gross + policy_soyweight_city, data = dat,
      left = 0, right = 1) %>% summary()




tobit(share_corn ~ corn_sp_lag+ soy_sp_lag, data = dat,
      left = 0, right = 1) %>% summary()


tobit(share_corn ~ corn_futureprice+ soy_futureprice + policy_soy, data = dat %>% 
        filter(province %in% c("Heilongjiang", "Inner mongolia","Jilin","Liaoning")),
      left = 0, right = 1) %>% summary()

tobit(share_corn ~ corn_sp_lag+ soy_sp_lag + policy_soy, data = dat %>% 
        filter(province %in% c("Heilongjiang", "Inner mongolia","Jilin","Liaoning")),
      left = 0, right = 1) %>% summary()
#soy
tobit(share_soy ~ corn_sp_lag + soy_sp_lag + policy_soy*soy_weight, data = dat,
      left = 0, right = 1) %>% summary()






#state-owned farmland
tobit(share_corn ~ corn_sp_lag + soy_sp_lag + policy_soy*stateprop, data = citycomb,
              left = 0, right = 1) %>% summary()




fit2 <- tobit(share_corn ~ corn_futureprice + soy_futureprice, data = citycomb,
              left = 0, right = 1) #not significant
summary(fit2)


fit3 <- tobit(share_corn ~ corn_profit_gross +  soy_profit_gross, data = citycomb,
              left = 0, right = 1) #not significant
summary(fit3)


#spatial effects
ggplot(data = acre, aes(x = year, y = share_soy,)) + 
  geom_point()+
  facet_wrap(~region)

ggplot(data = acre, aes(x = year, y = share_corn,)) + 
  geom_point()+
  facet_wrap(~region)

ggplot(data = acre, aes(x = year)) + 
  geom_point(aes(y = share_soy, color = "soy"))+
  geom_point(aes(y = share_fruit, color = "fruit"))+
  facet_wrap(~region)

ggplot(data = acre %>% filter(region == "Heilongjiang"), aes(x = factor(year))) + 
  geom_line(aes(y = share_corn, color = "corn"), group = 1)+
  geom_line(aes(y = share_rice, color = "rice"), group = 1)+
  geom_line(aes(y = share_wheat, color = "wheat"), group = 1)+
  geom_point(aes(y = share_corn))+
  geom_point(aes(y = share_rice))+
  geom_point(aes(y = share_wheat))+
  xlab("Year") +
  ylab("Acreage Share")+
  theme_bw()

ggplot(data = acre %>% filter(region %in% c("Anhui", "Liaoning") ), aes(x = factor(year) )) + 
  geom_line(aes(y = share_corn, color = "corn"), group = 1)+
  geom_line(aes(y = share_soy, color = "soybean"), group = 1)+
  geom_line(aes(y = share_rice, color = "rice"), group = 1)+
  geom_line(aes(y = share_wheat, color = "wheat"),  group = 1)+
  geom_line(aes(y = share_vege, color = "vege"),  group = 1)+
  geom_line(aes(y = share_fruit, color = "fruit"),  group = 1)+
  xlab("Year") +
  ylab("Acreage Share")+
  theme_bw()+
  facet_wrap(~region)+
  scale_color_manual(values = c("corn" = "red", 
                                "soybean" = "blue", 
                                "rice" = "green", 
                                "wheat" = "purple", 
                                "vege" = "orange", 
                                "fruit" = "black"))

                     
                     
ggplot(data = acre %>% filter(region == "Anhui"), aes(x = factor(year)))+
  geom_line(aes(y = acre_total), group = 1)+
  xlab("Year")+
  ylab("Crop sown acre in Anhui")+
  theme_classic()



ggplot(data = acre %>% filter(region == c("Liaoning", "Anhui")), aes(x = factor(year)))+
  geom_line(aes(y = acre_total), group = 1)+
  xlab("Year")+
  ylab("Crop sown acre in total")+
  theme_classic()+
  facet_wrap(~region)



#geom_point(aes(y = share_wheat, color = "wheat"))+
#geom_point(aes(y = share_vege, color = "vege"))+
#  geom_point(aes(y = share_rice, color = "rice"))+


#statefarm
statefarm <- acre %>% group_by(region) %>% summarise(statefarm = mean(acre_state),
                                        sown_total = mean(acre_total),
                                        statefarmprop = mean(stateprop)) %>% 
 arrange(desc(statefarm)) %>% 
 arrange(desc(statefarmprop)) %>% filter(region %in% c("Heilongjiang", "Inner Mongolia", "Jilin","Liaoning",
                                                       "Shandong", "Henan", "Hebei", "Anhui", "Nation"))

#crop weight:2017-2021
struc <- acre %>% filter(year > 2016)

struc %>% group_by(region) %>% summarise(corn = round(mean(corn_weight)*100, 2),
                                        soy = round(mean(soy_weight)*100, 2),
                                        rice = round(mean(rice_weight)*100, 2),
                                        wheat = round(mean(wheat_weight)*100, 2),
                                        vege = round(mean(vege_weight)*100, 2),
                                        fruit = round(mean(fruit_weight)*100, 2),
                                        oil = round(mean(oil_weight)*100, 2) ) %>% 
  arrange(desc(corn)) 

#write_xlsx(crop_weight, "/Users/xiaolanwan/Documents/GitHub/ChinaAg/Xiaolan/Result/weight.xlsx")



#plant stucture, 2017-2021
plant_struc <- struc %>% group_by(region) %>% summarise(corn = round(mean(share_corn)*100, 2),
                                        soybean = round(mean(share_soy)*100, 2),
                                        rice = round(mean(share_rice)*100, 2),
                                        wheat = round(mean(share_wheat)*100, 2),
                                        vege = round(mean(share_vege)*100, 2),
                                        fruit = round(mean(share_fruit)*100, 2),
                                        oil = round(mean(share_oil)*100, 2)) %>% 
  arrange(desc(corn))
 
 
plant_struc_state <- plant_struc %>% left_join(statefarm, by = "region")

#write_xlsx(plant_struc_state, "/Users/xiaolanwan/Documents/GitHub/ChinaAg/Xiaolan/Result/struc_state.xlsx")

#change detection
 acre %>% filter(year %in% c(2015, 2016)) %>% 
   group_by(region) %>% arrange(year) %>% 
   summarize(soybean = (last(acre_soy)/first(acre_soy)-1)*100,
             corn = (last(acre_corn)/first(acre_corn)-1)*100,
             rice = (last(acre_rice)/first(acre_rice)-1)*100,
             wheat = (last(acre_wheat)/first(acre_wheat)-1)*100,
             vege = (last(acre_vege)/first(acre_vege)-1)*100,
             fruit = (last(acre_fruit)/first(acre_fruit)-1)*100,
             oil = (last(acre_oil)/first(acre_oil)-1)*100) %>% 
   arrange(desc(soybean))
  

acre %>% filter(year %in% c(2016, 2017)) %>% 
  group_by(region) %>% arrange(year) %>% 
    summarize(soybean = (last(acre_soy)/first(acre_soy)-1)*100,
              corn = (last(acre_corn)/first(acre_corn)-1)*100,
              rice = (last(acre_rice)/first(acre_rice)-1)*100,
              wheat = (last(acre_wheat)/first(acre_wheat)-1)*100,
              vege = (last(acre_vege)/first(acre_vege)-1)*100,
              fruit = (last(acre_fruit)/first(acre_fruit)-1)*100,
              oil = (last(acre_oil)/first(acre_oil)-1)*100) %>% 
  arrange(desc(soybean))


acre %>% filter(year %in% c(2015, 2016)) %>% 
  group_by(region) %>% arrange(year) %>% 
  summarize(soybean = (last(acre_soy)/first(acre_soy)-1)*100,
            corn = (last(acre_corn)/first(acre_corn)-1)*100,
            rice = (last(acre_rice)/first(acre_rice)-1)*100,
            wheat = (last(acre_wheat)/first(acre_wheat)-1)*100,
            vege = (last(acre_vege)/first(acre_vege)-1)*100,
            fruit = (last(acre_fruit)/first(acre_fruit)-1)*100,
            oil = (last(acre_oil)/first(acre_oil)-1)*100) %>% 
  arrange(desc(soybean))

acre %>% filter(year %in% c(2016, 2020)) %>% 
  group_by(region) %>% arrange(year) %>% 
  summarize(soybean = (last(acre_soy)/first(acre_soy)-1)*100,
            corn = (last(acre_corn)/first(acre_corn)-1)*100,
            rice = (last(acre_rice)/first(acre_rice)-1)*100,
            wheat = (last(acre_wheat)/first(acre_wheat)-1)*100,
            vege = (last(acre_vege)/first(acre_vege)-1)*100,
            fruit = (last(acre_fruit)/first(acre_fruit)-1)*100,
            oil = (last(acre_oil)/first(acre_oil)-1)*100) %>% 
  arrange(desc(soybean))


#share change
acre %>% filter(year %in% c(2015, 2016)) %>% 
  group_by(region) %>% arrange(year) %>% 
  summarize(soybean = (last(share_soy)/first(share_soy)-1)*100,
            corn = (last(share_corn)/first(share_corn)-1)*100,
            rice = (last(share_rice)/first(share_rice)-1)*100,
            wheat = (last(share_wheat)/first(share_wheat)-1)*100,
            vege = (last(share_vege)/first(share_vege)-1)*100,
            fruit = (last(share_fruit)/first(share_fruit)-1)*100,
            oil = (last(share_oil)/first(share_oil)-1)*100) %>% 
  arrange(desc(soybean))


#main provinces
main <- acre %>% filter(location %in% c(0,1,2))

ggplot(data = main, aes(x = year)) + 
  geom_point(aes(y = share_soy, color = "soy"))+
  geom_point(aes(y = share_corn, color = "corn"))+
  geom_point(aes(y = share_vege, color = "vege"))+
  facet_wrap(~region)

ggplot(data = main, aes(x = year)) + 
  geom_point(aes(y = share_vege, color = "vege"))+
  geom_point(aes(y = share_fruit, color = "fruit"))+
  geom_point(aes(y = share_oil, color = "oil"))+
  facet_wrap(~region)


#share change per year
share_change <- main %>% arrange(region, year) %>% 
  group_by(region) %>% 
 mutate(pct_soy = (share_soy/lag(share_soy)-1)*100,
        pct_corn = (share_corn/lag(share_corn)-1)*100,
        pct_rice = (share_rice/lag(share_rice)-1)*100,
        pct_wheat = (share_wheat/lag(share_wheat)-1)*100,
        pct_vege = (share_vege/lag(share_vege)-1)*100,
        pct_fruit = (share_fruit/lag(share_fruit)-1)*100,
        pct_oil = (share_oil/lag(share_oil)-1)*100,
        ) 

#share_change[share_change$region == "Nation","pcentchange"]


#soybean
ggplot(data = share_change) +
  geom_line(aes(x = factor(year), y = pct_soy), color = "blue", group = 1) + 
  geom_col(aes(x = factor(year), y = pct_soy, fill = pct_soy > 0))+
  geom_point(aes(x = factor(year), y = pct_soy)) +
  facet_wrap(~region)+
  theme_classic() +
  xlab("Year")+
  ylab("% Change in Soybean Share Per Year")+
  scale_fill_manual(
    values = c("TRUE" = "green", "FALSE" = "red"),
    labels = c( "Negative","Positive"),
    name = NULL
  )

#corn
ggplot(data = share_change) +
  geom_line(aes(x = factor(year), y = pct_corn), color = "blue", group = 1) + 
  geom_col(aes(x = factor(year), y = pct_corn, fill = pct_corn > 0))+
  geom_point(aes(x = factor(year), y = pct_corn)) +
  facet_wrap(~region)+
  theme_classic() +
  xlab("Year")+
  ylab("% Change in Corn Share Per Year")+
  scale_fill_manual(
    values = c("TRUE" = "green", "FALSE" = "red"),
    labels = c("Negative", "Positive"),
    name = NULL
  )

#rice
ggplot(data = share_change) +
  geom_line(aes(x = factor(year), y = pct_rice), color = "blue", group = 1) + 
  geom_col(aes(x = factor(year), y = pct_rice, fill = pct_rice > 0))+
  geom_point(aes(x = factor(year), y = pct_rice)) +
  facet_wrap(~region)+
  theme_classic() +
  xlab("Year")+
  ylab("% Change in Rice Share Per Year")+
  scale_fill_manual(
    values = c("TRUE" = "green", "FALSE" = "red"),
    labels = c("Negative", "Positive"),
    name = NULL
  )

#wheat
ggplot(data = share_change) +
  geom_line(aes(x = factor(year), y = pct_wheat), color = "blue", group = 1) + 
  geom_col(aes(x = factor(year), y = pct_wheat, fill = pct_wheat > 0))+
  geom_point(aes(x = factor(year), y = pct_wheat)) +
  facet_wrap(~region)+
  theme_classic() +
  xlab("Year")+
  ylab("% Change in Wheat Share Per Year")+
  scale_fill_manual(
    values = c("TRUE" = "green", "FALSE" = "red"),
    labels = c("Negative", "Positive"),
    name = NULL
  )

#vege
ggplot(data = share_change) +
  geom_line(aes(x = factor(year), y = pct_vege), color = "blue", group = 1) + 
  geom_col(aes(x = factor(year), y = pct_vege, fill = pct_vege > 0))+
  geom_point(aes(x = factor(year), y = pct_vege)) +
  facet_wrap(~region)+
  theme_classic() +
  xlab("Year")+
  ylab("% Change in Vegetable Share Per Year")+
  scale_fill_manual(
    values = c("TRUE" = "green", "FALSE" = "red"),
    labels = c("Negative", "Positive"),
    name = NULL
  )


#fruit
ggplot(data = share_change) +
  geom_line(aes(x = factor(year), y = pct_fruit), color = "blue", group = 1) + 
  geom_col(aes(x = factor(year), y = pct_fruit, fill = pct_fruit > 0))+
  geom_point(aes(x = factor(year), y = pct_fruit)) +
  facet_wrap(~region)+
  theme_classic() +
  xlab("Year")+
  ylab("% Change in Fruit Share Per Year")+
  scale_fill_manual(
    values = c("TRUE" = "green", "FALSE" = "red"),
    labels = c("Negative", "Positive"),
    name = NULL
  )

#oil
ggplot(data = share_change) +
  geom_line(aes(x = factor(year), y = pct_oil), color = "blue", group = 1) + 
  geom_col(aes(x = factor(year), y = pct_oil, fill = pct_oil > 0))+
  geom_point(aes(x = factor(year), y = pct_oil)) +
  facet_wrap(~region)+
  theme_classic() +
  xlab("Year")+
  ylab("% Change in Oil Share Per Year")+
  scale_fill_manual(
    values = c("TRUE" = "green", "FALSE" = "red"),
    labels = c("Negative", "Positive"),
    name = NULL
  )




 # geom_text(aes(x = factor(year), y = pct_soy, label=sprintf("%.2f%%", pct_soy)))


ggplot(data = share_change %>% filter(region == "Heilongjiang")) +
  geom_line(aes(x = factor(year), y = pct_soy), color = "blue", group = 1) + 
  geom_col(aes(x = factor(year), y = pct_soy, fill = pct_soy > 0))+
  geom_point(aes(x = factor(year), y = pct_soy)) +
  theme_classic()




#share change
main %>% filter(year %in% c(2015, 2016)) %>% 
  group_by(region) %>% arrange(year) %>% 
  summarize(soybean = (last(share_soy)/first(share_soy)-1)*100,
            corn = (last(share_corn)/first(share_corn)-1)*100,
            rice = (last(share_rice)/first(share_rice)-1)*100,
            wheat = (last(share_wheat)/first(share_wheat)-1)*100,
            vege = (last(share_vege)/first(share_vege)-1)*100,
            fruit = (last(share_fruit)/first(share_fruit)-1)*100,
            oil = (last(share_oil)/first(share_oil)-1)*100) #%>% 
  #arrange(desc(soybean))



#share change
main %>% filter(year %in% c(2015, 2016)) %>% 
  group_by(region) %>% arrange(year) %>% 
  summarize(soybean = (last(share_soy)/first(share_soy)-1)*100,
            corn = (last(share_corn)/first(share_corn)-1)*100,
            rice = (last(share_rice)/first(share_rice)-1)*100,
            wheat = (last(share_wheat)/first(share_wheat)-1)*100,
            vege = (last(share_vege)/first(share_vege)-1)*100,
            fruit = (last(share_fruit)/first(share_fruit)-1)*100,
            oil = (last(share_oil)/first(share_oil)-1)*100) %>% 
  arrange(desc(soybean))

acre %>% filter(region == "Heilongjiang")
