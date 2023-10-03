library(tidyverse)
library("AER")
setwd("/Users/xiaolanwan/Documents/GitHub/ChinaAg/")

#prepare variables

citycomb <- readRDS("Xiaolan/data clean/citycomb.rds")

##compute soybean weight for cities
soynation <- readRDS("~/Documents/GitHub/ChinaAg/Xiaolan/data clean/soynation.rds")

citycomb <- citycomb %>% left_join(soynation, by = "year") 

citycomb %>% mutate(soy_weight = acre_soy/soy_nation)

#summary data



#regression:soybean weight


#SOF





