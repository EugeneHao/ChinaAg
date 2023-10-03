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
names(hlj) <- c("region", "year", "acre_soy","acre_wheat","acre_total",
                 "acre_oil", "acre_corn", "acre_fruit", "acre_rice", "acre_vege")
hlj$province <- rep("Heilongjiang", times = 195)

##check data

hlj <- hlj %>% mutate(share_soy = acre_soy/acre_total,
                        share_wheat = acre_wheat/acre_total,
                        share_oil = acre_oil/acre_total,
                        share_corn = acre_corn/acre_total,
                        share_fruit = acre_fruit/acre_total,
                        share_rice = acre_rice/acre_total,
                        share_vege = acre_vege/acre_total)





#fit tobit

fit1 <- tobit(share_corn ~ policy_soy*stateprop, data = acre,left = 0, right = 1)
summary(fit1)

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
