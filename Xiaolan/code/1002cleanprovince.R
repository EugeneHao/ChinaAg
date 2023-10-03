#hao data

prov18 <- readRDS("~/Documents/GitHub/ChinaAg/From Hao/data/18provinces_1002/multidat_wide.rds")


#national corn and soybean acreage:2001-2021

soy_nation <- read_excel("Xiaolan/raw data/soy_sannong.xls")
names(soy_nation) <- c("index", "year", "value")
soy_nation$year <- as.numeric(soy_nation$year)
soy_nation$value <- as.numeric(soy_nation$value)
soy_nation <- soy_nation %>% tidyr::spread(key = index, value) 
names(soy_nation) <- c("year", "soy_nation", "corn_nation")

prov18 <- prov18 %>% left_join(soy_nation, by = "year")


#policy indicator variables:2001-2021
po <- c(rep(0, each = 15), 1,1,1,1,1,0)
prov18$policy_soy <- rep(po, times = 18)


#regressions



