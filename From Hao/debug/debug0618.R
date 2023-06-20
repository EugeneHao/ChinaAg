g4 <- readRDS("/Users/sunhao/Documents/GitHub/ChinaAg/From Hao/data/dongbei/regdat_V4.rds")
g2 <- readRDS("/Users/sunhao/Documents/GitHub/ChinaAg/From Hao/data/dongbei/regdat_V2.rds")

max(g2$other - g4$other)

g2 <- g2 %>% mutate(share_wheat = g2 %>% filter(crop == "wheat") %>% "$"(share) %>% 
                      matrix(., nrow = 20) %>% # "+"(400) %>% log() %>% 
                      kronecker(rep(1, 4), .) %>% as.vector()) %>% 
  mutate(y2 = log(share/(other + share_wheat))) %>% 
  mutate(pfex_corn = g2 %>% filter(crop == "corn") %>% "$"(profit_ex) %>% 
           matrix(., nrow = 20) %>% # "+"(400) %>% log() %>% 
           kronecker(rep(1, 4), .) %>% as.vector(), 
         pfex_rice = g2 %>% filter(crop == "rice") %>% "$"(profit_ex) %>% 
           matrix(., nrow = 20) %>% # "+"(400) %>% log() %>% 
           kronecker(rep(1, 4), .) %>% as.vector(), 
         pfex_soy = g2 %>% filter(crop == "soybean") %>% "$"(profit_ex) %>% 
           matrix(., nrow = 20) %>% # "+"(400) %>% log() %>% 
           kronecker(rep(1, 4), .) %>% as.vector(), 
         pfex_wheat = g2 %>% filter(crop == "wheat") %>% "$"(profit_ex) %>% 
           matrix(., nrow = 20) %>% # "+"(400) %>% log() %>% 
           kronecker(rep(1, 4), .) %>% as.vector(),
         policy = subsidy > 0
  )

max(abs(g4$y2 - g2$y2))
which.max(abs(g4$y2 - g2$y2))

g4$y2[225:227]
g2$y2[225:227]

g4 <- g4 %>% filter(crop == "soybean") %>% select(region, crop, year, y2, pfex_corn, pfex_rice, pfex_soy, policy)
g2 <- g2 %>% filter(crop == "soybean") %>% select(region, crop, year, y2, pfex_corn, pfex_rice, pfex_soy, policy)

View(cbind(g4, g2))



# check multidat 
multi_old <- readRDS("From Hao/data/dongbei/multidat_nolag_V2.rds")
View(multi_old)
