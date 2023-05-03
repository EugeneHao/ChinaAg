multidat <- readRDS("From Hao/data/dongbei/multidat_nolag_V2.rds") %>% 
  arrange(region, crop)

library(tidyverse)

# update price 
multidat <- multidat %>% mutate(price = price/yield)

# we first need to consider two profit variables 

# 1. true profit in the current year 
multidat <- multidat %>% 
  mutate(profit_true = yield * (price - cost) + subsidy)

# 2. expected profit in the current year 
# we consider: 
# (1) yield: average past 3 years 
# (2) price: ex_price in the current year 
# (3) cost: cost in the current year 
# (4) subsidy: weighted subsidy = last subsidy * 0.67 + second subsidy * 0.33 

multidat$yield_lag <- get_lag(multidat, "yield", 3, weight = c(0.34, 0.33, 0.33))
multidat$subsidy_lag <- get_lag(multidat, "subsidy", 2, weight = c(0.67, 0.33))

multidat <- multidat %>% mutate(profit_ex = yield_lag * (ex_price - cost) + subsidy_lag)

# 3. add lag profits as two covariates 

multidat$profit_ex_lag <- get_lag(multidat, "profit_ex", 1, weight = 1)
multidat$profit_true_lag <- get_lag(multidat, "profit_true", 1, weight = 1)

# 4. try SUR-HEAR model 
# we have 6 provinces, from 2002 to 2020 to do regression 

# Step 1:
# (1.1) compute log(s_i/s_0)

otherdat <- multidat %>% group_by(region, year) %>% summarise(other = 100 - sum(share))

regdat <- left_join(multidat, otherdat) 

# for 0 share, we update it to 0.01
regdat$share[regdat$share == 0] <- 0.1
regdat <- regdat %>% mutate(y = log(share/other))
  
# extract predict data
preddat <- regdat %>% filter(year == 2021)

# remove year 2021 from regdat

regdat <- regdat %>% filter(year < 2021)

saveRDS(regdat, "From Hao/data/dongbei/regdat_V2.rds")
saveRDS(preddat, "From Hao/data/dongbei/preddat_V2.rds")


### TRY SUR-HEAR #### 

# (1.2) OLS for each crop to get residuals 
# use: profit_ex, profit_true_lag, subsidy_lag, ex_price, yield_lag as covarites 

u_jt <- list()
for(i in 1:length(unique(regdat$crop)))
{
  u_jt[[i]] <- lm(y ~ profit_ex + profit_true_lag + ex_price + yield_lag, 
                  data = regdat %>% filter(crop == unique(regdat$crop)[i]))$residuals
}
names(u_jt) <- unique(regdat$crop)

# (1.3) calculate rho
rho_ij <- matrix(0, nrow = 4, ncol = length(unique(regdat$region)))   # 4 crops and 6 provinces 
for(i in 1:4)
  for(j in 1:length(unique(regdat$region)))
  {
    u_tmp <- u_jt[[i]][((j-1)*20+1):(j*20)]      # T = 20 
    rho_ij[i, j] <- sum(u_tmp[-1] * u_tmp[-20])/sum(u_tmp^2)
  }
colnames(rho_ij) <- unique(regdat$region)
rownames(rho_ij) <- unique(regdat$crop)
 
# Step 2: 
# (2.1) correct the response for autocorrelation
ratio <- NULL
for(i in 1:length(rho_ij))
{
  ratio <- c(ratio, sqrt(1 - rho_ij[i]^2), 1 - rep(rho_ij[i], 19))
}
regdat$y2 <- regdat$y * ratio

# Step 3: 

u_jt2 <- list()
for(i in 1:length(unique(regdat$crop)))
{
  u_jt2[[i]] <- lm(y2 ~ profit_ex + profit_true_lag + ex_price + yield_lag, 
                  data = regdat %>% filter(crop == unique(regdat$crop)[i]))$residuals
}
names(u_jt2) <- unique(regdat$crop)

# Step 4: 
s_ij <- matrix(0, nrow = 4, ncol = length(unique(regdat$region)))   # 4 crops and 6 provinces 
for(i in 1:4)
  for(j in 1:length(unique(regdat$region)))
  {
    u_tmp <- u_jt2[[i]][((j-1)*20+1):(j*20)]      # T = 20 
    s_ij[i, j] <- sqrt(mean(u_tmp^2))             # take square root!
  }
colnames(s_ij) <- unique(regdat$region)
rownames(s_ij) <- unique(regdat$crop)

# Step 5: 

regdat_new <- regdat
ratio <- rep(s_ij, each = 20)
regdat_new$y3 <- regdat_new$y2 / ratio
regdat_new <- regdat_new %>% 
  mutate(profit_ex = profit_ex/ratio, subsidy_lag = subsidy_lag/ratio,
         ex_price = ex_price/ratio, yield_lag = yield_lag/ratio)

# Step 6: 

library(systemfit)

# (6.1) prepare suitable data for SUR model 

reg_corn <- regdat_new %>% filter(crop == "corn") %>% 
  select(region, year, y3, profit_ex, profit_true_lag, ex_price, yield_lag) %>% 
  "colnames<-"(c("region", "year", paste(c("y", paste0("x", 1:4)), "corn", sep = "_")))

reg_rice <- regdat_new %>% filter(crop == "rice") %>% 
  select(region, year, y3, profit_ex, profit_true_lag, ex_price, yield_lag) %>% 
  "colnames<-"(c("region", "year", paste(c("y", paste0("x", 1:4)), "rice", sep = "_")))

reg_soy <- regdat_new %>% filter(crop == "soybean") %>% 
  select(region, year, y3, profit_ex, profit_true_lag, ex_price, yield_lag) %>% 
  "colnames<-"(c("region", "year", paste(c("y", paste0("x", 1:4)), "soy", sep = "_")))

reg_wheat <- regdat_new %>% filter(crop == "wheat") %>% 
  select(region, year, y3, profit_ex, profit_true_lag, ex_price, yield_lag) %>% 
  "colnames<-"(c("region", "year", paste(c("y", paste0("x", 1:4)), "wheat", sep = "_")))

regdat_final <- left_join(reg_corn, reg_rice) %>% left_join(., reg_soy) %>% left_join(., reg_wheat) 

eq_corn <- paste("y_corn ~", paste(paste(paste0("x", c(1, 2, 3, 4)), "corn", sep = "_"), collapse = " + ")) %>% as.formula()
eq_rice <- paste("y_rice ~", paste(paste(paste0("x", c(1, 2, 3, 4)), "rice", sep = "_"), collapse = " + ")) %>% as.formula()
eq_soy <- paste("y_soy ~", paste(paste(paste0("x", c(1, 2, 3, 4)), "soy", sep = "_"), collapse = " + ")) %>% as.formula()
eq_wheat <- paste("y_wheat ~", paste(paste(paste0("x", c(1, 2, 3, 4)), "wheat", sep = "_"), collapse = " + ")) %>% as.formula()

system <- list(corn = eq_corn, rice = eq_rice, soy = eq_soy, wheat = eq_wheat)

fitsur <- systemfit( system, "SUR", data = regdat_final, maxit = 100 )


# Step 7: prediction 

# (7.1)  get prediction data profit_ex, profit_true_lag, subsidy_lag, ex_price, yield_lag

preddat_final <- data.frame(region = preddat$region, crop = preddat$crop,
                            x1 = preddat$profit_ex /as.vector(s_ij), 
                            x2 = preddat$profit_true_lag /as.vector(s_ij), 
                            x3 = preddat$ex_price /as.vector(s_ij), 
                            x4 = preddat$yield_lag /as.vector(s_ij))

# (7.2) get coefficients 
surcoef <- fitsur$coefficients %>% matrix(., ncol = 4) %>% "colnames<-"(unique(preddat$crop)) %>% 
  "rownames<-"(c("Intercept", paste0("x", c(1,2,3,4))))

# (7.3) get y3_pred
y3_pred <- NULL
for(i in 1:4)
{
  y3_pred <- cbind(y3_pred, 
                   preddat_final %>% filter(crop == unique(preddat$crop)[i]) %>% 
                     "["(, 3:ncol(preddat_final)) %>% as.matrix() %>% cbind(1,.) %>% 
                     "%*%"(surcoef[,i]) %>% as.vector())
}
y3_pred <- y3_pred %>% t() %>% "rownames<-"(unique(preddat$crop)) %>% 
  "colnames<-"(unique(preddat$region))


y2_pred <- y3_pred * s_ij
y_pred <- y2_pred/sqrt(1-rho_ij^2)

share_est <- round(sweep(exp(y_pred), MARGIN = 2, FUN = "/", STATS = colSums(exp(y_pred)) + 1) * 100, 2)

share_true <- preddat$share %>% matrix(., nrow = 4)
