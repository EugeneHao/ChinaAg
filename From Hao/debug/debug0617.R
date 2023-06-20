# debug: why v2 and v3 have different regression results 

############ V2 ######
regdat <- readRDS("/Users/sunhao/Documents/GitHub/ChinaAg/From Hao/data/dongbei/regdat_V2.rds") %>% 
  mutate(ex_revenue = yield_lag * ex_price + subsidy_lag, 
         tcost = yield_lag * cost,
         rcratio = (yield_lag * ex_price + subsidy_lag)/(yield_lag * cost)) 

regdat <- regdat %>% 
  mutate(share_wheat = regdat %>% filter(crop == "wheat") %>% "$"(share) %>% 
           matrix(., nrow = 20) %>% # "+"(400) %>% log() %>% 
           kronecker(rep(1, 4), .) %>% as.vector()) %>% 
  mutate(y2 = log(share/(other + share_wheat)))   # get new y2 since now we combine other and wheat

preddat <- readRDS("/Users/sunhao/Documents/GitHub/ChinaAg/From Hao/data/dongbei/preddat_V2.rds") %>% 
  mutate(ex_revenue = yield_lag * ex_price + subsidy_lag,
         tcost = yield_lag * cost,
         rcratio = (yield_lag * ex_price + subsidy_lag)/(yield_lag * cost)) 

preddat <- preddat %>% 
  mutate(share_wheat = preddat %>% filter(crop == "wheat") %>% "$"(share) %>% 
           matrix(., nrow = 1) %>% # "+"(400) %>% log() %>% 
           kronecker(rep(1, 4), .) %>% as.vector()) %>% 
  mutate(y2 = log(share/(other + share_wheat))) 

# true share: 
share_true <- preddat$share %>% matrix(., nrow = 4) %>% "["(-4,)


# add expected profit for each crop 
regdat <- regdat %>% 
  mutate(pfex_corn = regdat %>% filter(crop == "corn") %>% "$"(profit_ex) %>% 
           matrix(., nrow = 20) %>% # "+"(400) %>% log() %>% 
           kronecker(rep(1, 4), .) %>% as.vector(), 
         pfex_rice = regdat %>% filter(crop == "rice") %>% "$"(profit_ex) %>% 
           matrix(., nrow = 20) %>% # "+"(400) %>% log() %>% 
           kronecker(rep(1, 4), .) %>% as.vector(), 
         pfex_soy = regdat %>% filter(crop == "soybean") %>% "$"(profit_ex) %>% 
           matrix(., nrow = 20) %>% # "+"(400) %>% log() %>% 
           kronecker(rep(1, 4), .) %>% as.vector(), 
         pfex_wheat = regdat %>% filter(crop == "wheat") %>% "$"(profit_ex) %>% 
           matrix(., nrow = 20) %>% # "+"(400) %>% log() %>% 
           kronecker(rep(1, 4), .) %>% as.vector()
  )


# same for the predicted data 
preddat <- preddat %>% 
  mutate(pfex_corn = preddat %>% filter(crop == "corn") %>% "$"(profit_ex) %>% 
           matrix(., nrow = 1) %>% # "+"(400) %>% log() %>% 
           kronecker(rep(1, 4), .) %>% as.vector(), 
         pfex_rice = preddat %>% filter(crop == "rice") %>% "$"(profit_ex) %>% 
           matrix(., nrow = 1) %>% # "+"(400) %>% log() %>% 
           kronecker(rep(1, 4), .) %>% as.vector(), 
         pfex_soy = preddat %>% filter(crop == "soybean") %>% "$"(profit_ex) %>% 
           matrix(., nrow = 1) %>% #"+"(400) %>% log() %>% 
           kronecker(rep(1, 4), .) %>% as.vector(), 
         pfex_wheat = preddat %>% filter(crop == "wheat") %>% "$"(profit_ex) %>% 
           matrix(., nrow = 1) %>% #"+"(400) %>% log() %>% 
           kronecker(rep(1, 4), .) %>% as.vector()
  )

regdat <- regdat %>% filter(!region %in% c("Shandong", "Hebei"))
preddat <- preddat %>% filter(!region %in% c("Shandong", "Hebei"))

regdat_V2 <- rbind(regdat, preddat)

########## V3 ##################
regdat_new <- readRDS("/Users/sunhao/Documents/GitHub/ChinaAg/From Hao/data/dongbei/regdat_V3.rds") 
preddat_new <- readRDS("/Users/sunhao/Documents/GitHub/ChinaAg/From Hao/data/dongbei/preddat_V3.rds")

# add expected profit for each crop 
regdat_new <- regdat_new %>% 
  mutate(pfex_corn = regdat_new %>% filter(crop == "corn") %>% "$"(profit_ex) %>% 
           matrix(., nrow = 20) %>% # "+"(400) %>% log() %>% 
           kronecker(rep(1, 4), .) %>% as.vector(), 
         pfex_rice = regdat_new %>% filter(crop == "rice") %>% "$"(profit_ex) %>% 
           matrix(., nrow = 20) %>% # "+"(400) %>% log() %>% 
           kronecker(rep(1, 4), .) %>% as.vector(), 
         pfex_soy = regdat_new %>% filter(crop == "soybean") %>% "$"(profit_ex) %>% 
           matrix(., nrow = 20) %>% # "+"(400) %>% log() %>% 
           kronecker(rep(1, 4), .) %>% as.vector(), 
         pfex_wheat = regdat_new %>% filter(crop == "wheat") %>% "$"(profit_ex) %>% 
           matrix(., nrow = 20) %>% # "+"(400) %>% log() %>% 
           kronecker(rep(1, 4), .) %>% as.vector()
  )


# same for the predicted data 
preddat_new <- preddat_new %>% 
  mutate(pfex_corn = preddat_new %>% filter(crop == "corn") %>% "$"(profit_ex) %>% 
           matrix(., nrow = 1) %>% # "+"(400) %>% log() %>% 
           kronecker(rep(1, 4), .) %>% as.vector(), 
         pfex_rice = preddat_new %>% filter(crop == "rice") %>% "$"(profit_ex) %>% 
           matrix(., nrow = 1) %>% # "+"(400) %>% log() %>% 
           kronecker(rep(1, 4), .) %>% as.vector(), 
         pfex_soy = preddat_new %>% filter(crop == "soybean") %>% "$"(profit_ex) %>% 
           matrix(., nrow = 1) %>% #"+"(400) %>% log() %>% 
           kronecker(rep(1, 4), .) %>% as.vector(), 
         pfex_wheat = preddat_new %>% filter(crop == "wheat") %>% "$"(profit_ex) %>% 
           matrix(., nrow = 1) %>% #"+"(400) %>% log() %>% 
           kronecker(rep(1, 4), .) %>% as.vector()
  )

regdat_new <- regdat_new %>% filter(!region %in% c("Shandong", "Hebei"))
preddat_new <- preddat_new %>% filter(!region %in% c("Shandong", "Hebei"))

regdat_V2_new <- rbind(regdat_new, preddat_new)



g1 <- regdat_V2 %>% select(region, year, crop, y2, pfex_corn, pfex_rice, pfex_soy)

g2 <- regdat_V2_new %>% mutate(y2 = y - log(other)) %>%
  select(region, year, crop, y2, pfex_corn, pfex_rice, pfex_soy)

identical(g1, g2)

regdat_V2_new$policy <- regdat_V2_new$subsidy > 0
  
lm_soy_pf <- lm((y - log(other)) ~  0 + region  + pfex_corn + pfex_rice + pfex_soy + policy,
                data = regdat_V2_new %>% filter(crop == "soybean"))
summary(lm_soy_pf)
