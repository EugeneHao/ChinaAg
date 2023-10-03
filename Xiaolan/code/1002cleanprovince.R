#hao data
library(tidyverse)

prov18 <- readRDS("~/Documents/GitHub/ChinaAg/From Hao/data/18provinces_1002/multidat_wide.rds")


#national corn and soybean acreage:2001-2021

soy_nation <- read_excel("Xiaolan/raw data/soy_sannong.xls")
names(soy_nation) <- c("index", "year", "value")
soy_nation$year <- as.numeric(soy_nation$year)
soy_nation$value <- as.numeric(soy_nation$value)
soy_nation <- soy_nation %>% tidyr::spread(key = index, value) 
names(soy_nation) <- c("year", "soy_nation", "corn_nation")
saveRDS(soy_nation, "Xiaolan/data clean/soynation.rds")

prov18 <- prov18 %>% left_join(soy_nation, by = "year")
prov18 <- prov18 %>% mutate(soyweight = soybean_acreage/soy_nation)


#policy indicator variables:2001-2021
po <- c(rep(0, each = 15), 1,1,1,1,1,0)
prov18$policy_soy <- rep(po, times = 18)

saveRDS(prov18, "Xiaolan/data clean/prov.rds")
#regressions
tobit(corn_share ~ corn_saleprice_lag + soybean_saleprice_lag + I(policy_soy*soyweight), data = prov18,
      left = 0, right = 100) %>% summary()




