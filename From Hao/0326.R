library(tidyverse)
rawdat <- readxl::read_xlsx("rawdata/mypredata.xlsx")

# ex_pro_acre: expected profit per acre
# ex_pro_acre3: expected profit 
# ps_pro_acre: 
# ex_price: future price (yuan/kg in that year)
# price: historical price (yuan/kg, price in that year)
# cost: yuan/kg
# subsidy/mu: (yuan/mu)
# yield_mu: kg/mu

names(rawdat)[10] <- "subsidy_mu"

# we use the data from 2002 to 2021 
dat <- rawdat %>% select(crop, year, share, ex_price, price, cost, subsidy_mu, yield_mu) %>%
  filter(year < 2022, year > 2001)

### Step 1: impute cost for missing values in 2001 and 2002 for all 4 crops ####
# it seems that the cost has a linear relationship with year
dat %>% ggplot(aes(x = year, y = cost, col = crop)) + geom_point() 

# define a new variable `cost_im` to denote imputed cost 
dat$cost_im <- dat$cost 

# impute the missing cost 
for(i in unique(dat$crop))
{
  lm_cost <- lm(cost ~ year, dat %>% filter(crop == i))
  dat$cost_im[is.na(dat$cost_im) & dat$crop == i] <- 
    predict(lm_cost, newdata = dat %>% filter(crop == i, is.na(cost)))
}



### Step 2: let us impute the ex_price ####
# first, let us look at the scatterplot of (ex_price - price) against year 
# it is clear that ex_price is not randomly arround the true price 
dat %>% ggplot(aes(x = year, y = ex_price - price, col = crop)) + geom_point() + 
  facet_wrap(~crop, nrow = 2)

dat$ex_price_im <- dat$ex_price
for(i in unique(dat$crop))
{
  lm_exprice <- lm(ex_price ~ price, dat = dat %>% filter(crop == i))
  dat$ex_price_im[is.na(dat$ex_price) & (dat$crop == i)] <- 
    predict(lm_exprice, newdata = dat %>% filter(crop == i, is.na(ex_price)))
}

### Step 3: create lag variables: ####

# (1) future price: ex_price_lag_im
# note: wheat price is also not known in 2001, so we use 2002 price to impute 
dat$ex_price_lag_im <- dat$ex_price
rawdat$price[rawdat$crop == "wheat" & rawdat$year == 2001] <-
  rawdat$price[rawdat$crop == "wheat" & rawdat$year == 2002]

for(i in unique(dat$crop))
{
  lm_exprice <- lm(ex_price ~ price, dat = dat %>% filter(crop == i))
  tmp <- rawdat
  tmp$ex_price_im <- tmp$ex_price
  tmp$ex_price_im[is.na(tmp$ex_price) & (tmp$crop == i)] <- 
    predict(lm_exprice, newdata = tmp %>% filter(crop == i, is.na(ex_price)))
  dat$ex_price_lag_im[dat$crop == i] <- tmp %>% filter(crop == i, year < 2021) %>% "$"(ex_price_im)
}

# (2) cost: cost_lag

dat$cost_lag_im <- dat$cost
for(i in unique(dat$crop))
{
  lm_cost <- lm(cost ~ year, dat %>% filter(crop == i))
  tmp <- rawdat
  tmp$cost_im <- tmp$cost
  tmp$cost_im[is.na(tmp$cost_im) & tmp$crop == i] <- 
    predict(lm_cost, newdata = tmp %>% filter(crop == i, is.na(cost)))
  dat$cost_lag_im[dat$crop == i] <- tmp %>% filter(crop == i, year < 2021) %>% "$"(cost_im)
}

# (3) subsidy_mu: subsidy_mu_lag (may not be necessary)

dat$subsidy_mu_lag <- rawdat %>% filter(year < 2021) %>% "$"(subsidy_mu)

# further more: I suppose the farmer may be concern about the year after the first year with subsity
dat$subsidy_mu_lag2 <- dat$subsidy_mu_lag
for(i in nrow(dat):1)
{
  if(dat$year[i] > 2003) 
  {
    dat$subsidy_mu_lag2[i] <- (dat$subsidy_mu[i-1] * 0.67 + dat$subsidy_mu[i-2] * 0.33)
  } else
  {
    dat$subsidy_mu_lag2[i] <- 0
  }
}
  
# (4) yield: yield_lag
# note that wheat yield is not available for 2001 so we impute wheat yield in 2001 by wheat yield in 2002 

rawdat$yield_mu[rawdat$crop == "wheat" & rawdat$year == 2001] <-
  rawdat$yield_mu[rawdat$crop == "wheat" & rawdat$year == 2002]

dat$yield_mu_lag <-  rawdat %>% filter(year < 2021) %>% "$"(yield_mu)

saveRDS(dat, "data/dat.rds")


### Step 4: define the profit ####

# Try 1: profit = (ex_price_im - cost_im) * yield_mu + subsidy_mu_lag

g1 <- ggplot(data = dat, aes(y = (ex_price_im - cost_im) * yield_mu_lag + subsidy_mu_lag2, 
                       x = year, group = crop, color = crop)) + 
  geom_point(size = 2)+
  geom_line(size = 1)+
  xlab("Year") + ylab("Expected Profit (yuan/mu)") + # ggtitle("Expected Profits, current yield, lagged subsidy")+
  labs(group = "crop")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(legend.title=element_blank(),legend.position="right") +
  scale_shape_discrete(name = "", labels = c("Corn","Rice","Soybean", "Wheat")) +
  # scale_linetype_discrete(name = "", labels = c("Corn","Rice", "Soybean","Wheat")) +
  geom_vline(aes(xintercept = 2016), color = "red", linetype = "dashed")+
  geom_hline(aes(yintercept = 0), color = "grey")+
  scale_x_continuous(breaks=seq(2000,2022,5))+
  scale_y_continuous(breaks=seq(-300,1000, 200))+
  scale_color_manual(name = "", values = c("#E69F00","#56B4E9", "#009E73","purple"), labels = c("Corn","Rice","Soybean","Wheat")) + 
  theme_bw()

dat_share <- dat %>% select(year, crop, share)
dat_share <- rbind(dat_share, 
                   dat %>% group_by(year) %>% summarise(crop = "other",
                                     share = 1 - sum(share)))

g2 <- ggplot(data = dat_share, aes(y = 100 * share, 
                             x = year, group = crop, color = crop)) + 
  geom_point(size = 2)+
  geom_line(size = 1)+
  xlab("Year") + ylab("Share (%) ") + # ggtitle("Expected Profits, current yield, lagged subsidy")+
  labs(group = "crop")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(legend.title=element_blank(),legend.position="right") +
  scale_shape_discrete(name = "", labels = c("Corn", "Other","Rice","Soybean", "Wheat")) +
  # scale_linetype_discrete(name = "", labels = c("Corn", "Other","Rice", "Soybean","Wheat")) +
  geom_vline(aes(xintercept = 2016), color = "red", linetype = "dashed")+
  geom_hline(aes(yintercept = 0), color = "grey")+
  scale_x_continuous(breaks=seq(2000,2022,5))+
  scale_y_continuous(breaks=seq(0, 50, 5))+
  scale_color_manual(name = "", values = c("#E69F00", "grey","#56B4E9", "#009E73","purple"), 
                     labels = c("Corn", "Other","Rice","Soybean","Wheat")) + 
  theme_bw()

ggpubr::ggarrange(g1, g2, ncol = 1)

# we know that the first year with subsidy for each crop is: 
# rice: 2018; corn: 2016; soybean: 2014; wheat: none

# let's answer the following questions: 

# 1. why corn share decreased in 2016? 
# because corn profit decrease a lot from 2015 to 2016. The expected profit of corn is even worse than soybean

# 2. why corn share still decreased in 2017 even if it had subsidy in 2016? 
# because the farmers were not sure if the subsidy would still exist in 2017. 
# Actually, the subsidy for corn in 2016 was 133.46 but only 25.00 in 2017 which also reduced a lot. 
# We should consider the subsidy in the current year for regression 

# 3. why corn share increased in 2018? 
# because the expected profit for corn is higher than soybean

# 4. why corn share decreased in 2019? 
# One possible reason is that the in 2018, the subsidy for corn was 25.00 but 320 for soybean. 
# The yield and price have high variability. Considering the subsidy for soybean kept increasing but subsidy for corn kept decreasing,
# the farmers have reason to crop soybean for a stable and high subsidy. 

# 5. why corn share decreased in 2020? 
# Because the expected profit for corn is lo0er than soybean, which is mainly from previous subsidy difference

# 6. why corn share increased a lot in 2021? 
# Because the expected profit for corn is higher than soybean. We see that the future price for corn and soybean in 2021 both 
# increased a lot, but the cost per kilogram for soybean also increased a lot in 2021. 
# In addition, the corn purchase price in 2020 was the highest in the history (we should consider that effect in the regression)
# The price increase can be partly explained by the hog farming recovery since COVID.

# 7. why corn share decreased in 2008? 