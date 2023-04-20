# result_0415_backup

```{r}

pd_corn <- predict(lm_corn, newdata = preddat %>% filter(crop == "corn"))
pd_rice <- predict(lm_rice, newdata = preddat %>% filter(crop == "rice"))
pd_soy <- predict(lm_soy, newdata = preddat %>% filter(crop == "soybean"))
pd_wheat <- predict(lm_wheat, newdata = preddat %>% filter(crop == "wheat"))

y_pred <- rbind(pd_corn, pd_rice, pd_soy, pd_wheat)

share_est <- round(sweep(exp(y_pred), MARGIN = 2, FUN = "/", STATS = colSums(exp(y_pred)) + 1) * 100, 2)
share_est %>% data.frame() %>% "colnames<-"(unique(regdat$region)) %>% 
  "rownames<-"(unique(regdat$crop)) %>% pander::pander() 



lm_corn_simple <- lm(y ~ 0 + profit_ex + profit_true_lag + region,
                     data = regdat %>% filter(crop == "corn"))

lm_rice_simple <- lm(y ~ 0 + profit_ex + profit_true_lag + region,
                     data = regdat %>% filter(crop == "rice"))

lm_soy_simple <- lm(y ~ 0 + profit_ex + profit_true_lag + region,
                    data = regdat %>% filter(crop == "soybean"))

lm_wheat_simple <- lm(y ~ 0 + profit_ex + profit_true_lag + region,
                      data = regdat %>% filter(crop == "wheat"))

pd_corn_simple <- predict(lm_corn_simple, newdata = preddat %>% filter(crop == "corn"))
pd_rice_simple <- predict(lm_rice_simple, newdata = preddat %>% filter(crop == "rice"))
pd_soy_simple <- predict(lm_soy_simple, newdata = preddat %>% filter(crop == "soybean"))
pd_wheat_simple <- predict(lm_wheat_simple, newdata = preddat %>% filter(crop == "wheat"))

y_pred_simple <- rbind(pd_corn_simple, pd_rice_simple, pd_soy_simple, pd_wheat_simple)

share_est_simple <- round(sweep(exp(y_pred_simple), MARGIN = 2, FUN = "/", STATS = colSums(exp(y_pred_simple)) + 1) * 100, 2)
share_est_simple
#              1     2     3
# pd_corn  55.33 44.30 29.07
# pd_rice  18.95 22.39 31.10
# pd_soy    6.96  4.72 28.73
# pd_wheat  0.06  0.19  0.90

```


```{r}
# For region, use random effect
library(lme4)
lm_corn_rand <- lmer(y ~ pfex_corn + pfex_rice + pfex_soy + pfex_wheat + profit_true_lag + (1|region),
                     data = regdat %>% filter(crop == "corn"))

lm_rice_rand <- lmer(y ~ pfex_corn + pfex_rice + pfex_soy + pfex_wheat + profit_true_lag + (1|region),
                     data = regdat %>% filter(crop == "rice"))

lm_soy_rand <- lmer(y ~  pfex_corn + pfex_rice + pfex_soy + pfex_wheat + profit_true_lag + (1|region),
                    data = regdat %>% filter(crop == "soybean"))

lm_wheat_rand <- lmer(y ~ 0 + pfex_corn + pfex_rice + pfex_soy + pfex_wheat + profit_true_lag + (1|region),
                      data = regdat %>% filter(crop == "wheat"))

pd_corn_rand <- predict(lm_corn_rand, newdata = preddat %>% filter(crop == "corn"))
pd_rice_rand <- predict(lm_rice_rand, newdata = preddat %>% filter(crop == "rice"))
pd_soy_rand <- predict(lm_soy_rand, newdata = preddat %>% filter(crop == "soybean"))
pd_wheat_rand <- predict(lm_wheat_rand, newdata = preddat %>% filter(crop == "wheat"))

y_pred_rand <- rbind(pd_corn_rand, pd_rice_rand, pd_soy_rand, pd_wheat_rand)

share_est_rand <- round(sweep(exp(y_pred_rand), MARGIN = 2, FUN = "/", STATS = colSums(exp(y_pred_rand)) + 1) * 100, 2)
share_est_rand

```


```{r}

# Use SUR Regression 

library(systemfit)
library(lme4)
cor(cbind(lm_corn$residuals, lm_rice$residuals, lm_soy$residuals, lm_wheat$residuals))
# seems that corn and rice should be together

regdat_final <- regdat %>% 
  mutate(
    # regionest = rbind(lm_corn$coefficients[-(1:5)], lm_rice$coefficients[-(1:5)], 
    #                   lm_soy$coefficients[-(1:5)], lm_wheat$coefficients[-(1:5)]) %>% 
    #   as.vector() %>% rep(., each = 20)
    regionest = rbind(ranef(lm_corn_rand) %>% unlist(), ranef(lm_rice_rand) %>% unlist(),
                      ranef(lm_soy_rand) %>% unlist(), ranef(lm_wheat_rand) %>% unlist()) %>% 
      as.vector() %>% rep(., each = 20)
  ) %>% 
  mutate(y = y - regionest) %>%   # remove the region est 
  mutate(y_corn = regdat %>% filter(crop == "corn") %>% "$"(y) %>% matrix(., nrow = 20) %>% 
           kronecker(rep(1, 4), .) %>% as.vector(), 
         y_rice = regdat %>% filter(crop == "rice") %>% "$"(y) %>% matrix(., nrow = 20) %>% 
           kronecker(rep(1, 4), .) %>% as.vector(), 
         y_soy = regdat %>% filter(crop == "soybean") %>% "$"(y) %>% matrix(., nrow = 20) %>% 
           kronecker(rep(1, 4), .) %>% as.vector(), 
         y_wheat = regdat %>% filter(crop == "wheat") %>% "$"(y) %>% matrix(., nrow = 20) %>% 
           kronecker(rep(1, 4), .) %>% as.vector(),
         pftrue_corn = regdat %>% filter(crop == "corn") %>% "$"(profit_true_lag) %>% matrix(., nrow = 20) %>% 
           kronecker(rep(1, 4), .) %>% as.vector(), 
         pftrue_rice = regdat %>% filter(crop == "rice") %>% "$"(profit_true_lag) %>% matrix(., nrow = 20) %>% 
           kronecker(rep(1, 4), .) %>% as.vector(), 
         pftrue_soy = regdat %>% filter(crop == "soybean") %>% "$"(profit_true_lag) %>% matrix(., nrow = 20) %>% 
           kronecker(rep(1, 4), .) %>% as.vector(), 
         pftrue_wheat = regdat %>% filter(crop == "wheat") %>% "$"(profit_true_lag) %>% matrix(., nrow = 20) %>% 
           kronecker(rep(1, 4), .) %>% as.vector()) %>% 
  filter(crop == "corn")

eq_corn <- y_corn ~ pfex_corn + pfex_rice + pfex_soy + pfex_wheat + pftrue_corn   # + pftrue_corn
eq_rice <- y_rice ~ pfex_corn + pfex_rice + pfex_soy + pfex_wheat + pftrue_rice
eq_soy <- y_soy ~ pfex_corn + pfex_rice + pfex_soy + pfex_wheat + pftrue_soy
eq_wheat <- y_wheat ~ pfex_corn + pfex_rice + pfex_soy + pfex_wheat + pftrue_wheat

system <- list(corn = eq_corn, rice = eq_rice, soy = eq_soy, wheat = eq_wheat)

fitsur <- systemfit( system, "SUR", data = regdat_final, maxit = 10)

preddat_final <- preddat %>% 
  mutate(
    # regionest = rbind(lm_corn$coefficients[-(1:5)], lm_rice$coefficients[-(1:5)], 
    #                   lm_soy$coefficients[-(1:5)], lm_wheat$coefficients[-(1:5)]) %>% 
    #   as.vector() 
    regionest = rbind(ranef(lm_corn_rand) %>% unlist(), ranef(lm_rice_rand) %>% unlist(),
                      ranef(lm_soy_rand) %>% unlist(), ranef(lm_wheat_rand) %>% unlist()) %>% 
      as.vector()
  )

surcoef <- fitsur$coefficients %>% matrix(., ncol = 4)  %>% "[" (c(2:6, 1), ) # col = crop 

preddat_final$y_est <- 0
for(i in 1:nrow(preddat_final))
{
  id <- which(unique(preddat_final$crop) == preddat_final$crop[i])
  preddat_final$y_est[i] <- preddat_final$pfex_corn[i] * surcoef[1, id] + preddat_final$pfex_rice[i] * surcoef[2, id] + 
    preddat_final$pfex_soy[i] * surcoef[3, id] + preddat_final$pfex_wheat[i] * surcoef[4, id] + 
    preddat_final$profit_true_lag[i] * surcoef[5, id] + preddat_final$regionest[i] + surcoef[6, id]
}

y_pred_sur <- preddat_final$y_est %>% matrix(., nrow = 4)

share_est_sur <- round(sweep(exp(y_pred_sur), MARGIN = 2, FUN = "/", STATS = colSums(exp(y_pred_sur)) + 1) * 100, 2)
share_est_sur

```

