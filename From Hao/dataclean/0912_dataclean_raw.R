library(tidyverse)
setwd("/Users/sunhao/Documents/GitHub/ChinaAg/")


# 1. acre_vege

acre_vege <- readxl::read_xls("From Hao/rawdata/update/acre_vege.xls")
names(acre_vege) <- c("index", "region", "year", "value")
acre_vege$region <- factor(acre_vege$region, 
                         labels = c("Yunnan", "Neimenggu", "Jilin", "Sichuan", "Anhui", "Shandong", "Shanxi", "Guangxi",
                                    "Xinjiang", "Jiangsu", "Hebei", "Henan", "Hubei", "Gansu", "Guizhou", "Liaoning",
                                    "Shaanxi", "Heilongjiang"))
acre_vege$year <- as.numeric(acre_vege$year)
acre_vege$value <- as.numeric(acre_vege$value)
acre_vege <- acre_vege %>% tidyr::spread(key = index, value) 

# 2. acre0821

acre0821 <- readxl::read_xls("From Hao/rawdata/update/acre0821.xls")
names(acre0821) <- c("index", "region", "year", "value")
acre0821$region <- factor(acre0821$region, 
                          labels = c("Yunnan", "Neimenggu", "Jilin", "Sichuan", "Anhui", "Shandong", "Shanxi", "Guangxi",
                                     "Xinjiang", "Jiangsu", "Hebei", "Henan", "Hubei", "Gansu", "Guizhou", "Liaoning",
                                     "Shaanxi", "Heilongjiang"))
acre0821$year <- as.numeric(acre0821$year)
acre0821$value <- as.numeric(acre0821$value)
acre0821 <- acre0821 %>% tidyr::spread(key = index, value) 

# 3. cost0821

cost0821 <- rbind(readxl::read_xls("/Users/sunhao/Documents/GitHub/ChinaAg/From Hao/rawdata/update/cost0821-1.xls"),
                 readxl::read_xls("/Users/sunhao/Documents/GitHub/ChinaAg/From Hao/rawdata/update/cost0821-2.xls"))

names(cost0821) <- c("index", "region", "crop", "year", "value")
cost0821$region <- factor(cost0821$region, 
                          labels = c("Yunnan", "Neimenggu", "Jilin", "Sichuan", "Anhui", "Shandong", "Shanxi", "Guangxi",
                                     "Xinjiang", "Jiangsu", "Hebei", "Henan", "Hubei", "Gansu", "Guizhou", "Liaoning",
                                     "Shaanxi", "Heilongjiang"))
cost0821$year <- as.numeric(cost0821$year)
cost0821$value <- as.numeric(cost0821$value)
cost0821 <- cost0821 %>% unite("new_key", crop, index, sep = "_") %>% 
  tidyr::spread(key = new_key, value) 


# 4. oil

oil <- rbind(readxl::read_xls("From Hao/rawdata/update/oil_yunnan.xls"),
             readxl::read_xls("From Hao/rawdata/update/oil.xls"))
names(oil) <- c("index", "region", "crop", "year", "value")
oil$region <- factor(oil$region, 
                     labels = c("Yunnan", "Neimenggu", "Jilin", "Sichuan", "Anhui", "Shandong", "Guangxi",
                                "Xinjiang", "Jiangsu", "Hebei", "Henan", "Hubei", "Gansu", "Guizhou", "Liaoning",
                                "Shaanxi"))   # miss "Shanxi" and "Heilongjiang"
oil$year <- as.numeric(oil$year)
oil$value <- as.numeric(oil$value)
oil <- oil %>% unite("new_key", crop, index, sep = "_") %>% 
  tidyr::spread(key = new_key, value) 

# 5. orchid

orchid <- readxl::read_xls("From Hao/rawdata/update/orchid.xls") 
names(orchid) <- c("index", "region", "crop", "year", "value")
orchid <- orchid %>% filter(region!= "江西")
orchid$region <- factor(orchid$region, 
                     labels = c("Sichuan", "Shandong", "Shanxi", "Guangxi",
                                "Hebei", "Henan", "Hubei", "Gansu", "Liaoning",
                                "Shaanxi"))   # miss "Yunnan", "Neimenggu", "Jilin", "Anhui", "Xinjiang", "Jiangsu", "Guizhou", and "Heilongjiang"
orchid$year <- as.numeric(orchid$year)
orchid$value <- as.numeric(orchid$value)
orchid <- orchid %>% unite("new_key", crop, index, sep = "_") %>% 
  tidyr::spread(key = new_key, value) 


# 6. vege

vege <- rbind(readxl::read_xls("From Hao/rawdata/update/vege1.xls"),
              readxl::read_xls("From Hao/rawdata/update/vege2.xls"))
names(vege) <- c("index", "region", "crop", "year", "value")
vege$region <- factor(vege$region, 
                      labels = c("Yunnan", "Neimenggu", "Jilin", "Sichuan", "Anhui", "Shandong", "Shanxi", "Guangxi",
                                 "Xinjiang", "Jiangsu", "Hebei", "Henan", "Hubei", "Gansu", "Guizhou", "Liaoning",
                                 "Shaanxi", "Heilongjiang"))
vege$year <- as.numeric(vege$year)
vege$value <- as.numeric(vege$value)
vege <- vege %>% unite("new_key", crop, index, sep = "_") %>% 
  tidyr::spread(key = new_key, value) 


province_raw <- left_join(acre_vege, vege) %>% left_join(., acre0821) %>% 
  left_join(., cost0821) %>% left_join(., oil) %>% left_join(., orchid)

write.csv(province_raw, "From Hao/data/rawcleandata_18/province_raw.csv")



# costavg0821 (national) which includes average price (national)
costavg0821 <- readxl::read_xls("From Hao/rawdata/update/costavg0821.xls")
names(costavg0821) <- c("index", "region", "crop", "year", "value")
costavg0821$year <- as.numeric(costavg0821$year)
costavg0821$value <- as.numeric(costavg0821$value)

costavg0821 <- costavg0821 %>% unite("new_key", crop, index, sep = "_") %>% 
  tidyr::spread(key = new_key, value) 

# average (national)

average <- readxl::read_xls("From Hao/rawdata/update/average.xls")
names(average) <- c("index", "region", "crop", "year", "value")
average$year <- as.numeric(average$year)
average$value <- as.numeric(average$value)

average <- average %>% unite("new_key", crop, index, sep = "_") %>% 
  tidyr::spread(key = new_key, value) 


# future price (national)
fuprice <- readRDS("From Hao/data/heilongjiang/future_price.rds") %>% 
  filter(month == "03") %>% select(year, crop, ex_price) %>% 
  mutate(year = as.numeric(year))

my_fuprice <- data.frame(year = rep(2001:2021, 4), 
                         crop = rep(c("corn", "rice", "soybean", "wheat"), each = 21)) %>% 
  left_join(., fuprice) %>% spread(crop, ex_price) %>% 
  "colnames<-"(c("year", paste(c("corn", "rice", "soybean", "wheat"), "fp", sep = "_")))


national_raw <- left_join(costavg0821, my_fuprice) %>% left_join(., average) 
write.csv(national_raw, "From Hao/data/rawcleandata_18/national_raw.csv")
