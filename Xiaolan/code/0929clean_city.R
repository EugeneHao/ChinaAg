library(tidyverse)
setwd("/Users/xiaolanwan/Documents/GitHub/ChinaAg/")


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




#check data
acre %>% group_by(region) %>% summarise(weight_corn = mean(corn_weight),
                                        weight_soy = mean(soy_weight),
                                        weight_state = mean(stateprop))

#select variables
colnames(acre)[1]<- "province"

acre <- acre %>% select(province, year, corn_weight, soy_weight, acre_state, stateprop,
                        policy_soy,target_soy,policy16,policy19,policy21) %>% 
  filter(province != "Nation") %>% 
  as.data.frame()



#save data
write.csv(acre, "Xiaolan/data clean/province_weight.csv")

#merge with clean data
cityclean <- readRDS("~/Documents/GitHub/ChinaAg/From Hao/data/citylevel/cityclean.rds")

citycomb <- cityclean %>% filter(year > 2009) %>% #filter out years before 2010
  left_join(acre, by = c("province","year"))

saveRDS(citycomb, "Xiaolan/data clean/citycomb.rds")





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

citycomb <- citycomb %>% left_join(price, by = "year") %>% 
  as.data.frame()

saveRDS(citycomb, "Xiaolan/data clean/citycomb.rds")
