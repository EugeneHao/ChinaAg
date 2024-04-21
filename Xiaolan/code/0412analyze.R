library(tidyverse)
library("AER")
setwd("/Users/xiaolanwan/Documents/GitHub/ChinaAg/")
citycomb <- readRDS("Xiaolan/data clean/citycomb.rds")


#corn
city_corn <- citycomb %>% 
  mutate(share_corn100 = round(share_corn*100, digits = 1)) %>% 
  select(province,city, year, share_corn100) %>% 
  spread(year, share_corn100)

city_corn %>% filter(province == "Hebei") %>% 
  mutate(
  r10 = `2010`,
  r11 = `2011` - `2010`,
  r12 = `2012` - `2011`, 
  r13 = `2013` - `2012`, 
  r14 = `2014` - `2013`, 
  r15 = `2015` - `2014`, 
  r16 = `2016` - `2015`, 
  r17 = `2017` - `2016`, 
  r18 = `2018` - `2017`, 
  r19 = `2019` - `2018`, 
  r20 = `2020` - `2019`, 
  r21 = `2021` - `2020`, 
  ) %>% select(city, r10:r21)


#soy
city_soy <- citycomb %>% 
  select(province,city, year, share_soy) %>% 
  spread(year, share_soy)

city_soy %>% filter(province == "Hebei") %>% 
  mutate(
    r10 = `2010`,
    r11 = `2011` - `2010`,
    r12 = `2012` - `2011`, 
    r13 = `2013` - `2012`, 
    r14 = `2014` - `2013`, 
    r15 = `2015` - `2014`, 
    r16 = `2016` - `2015`, 
    r17 = `2017` - `2016`, 
    r18 = `2018` - `2017`, 
    r19 = `2019` - `2018`, 
    r20 = `2020` - `2019`, 
    r21 = `2021` - `2020`, 
  ) %>% select(city, r10:r21)




