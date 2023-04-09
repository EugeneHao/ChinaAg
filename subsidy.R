
library(tidyverse)
library(readxl)
library(lubridate)
acre <- 6.07028433


#Planting season                  Harvest season    
#330:玉米 Corn                    Late April                        October
#328:粳稻 Rice                    Early May                         October
#335:黄大豆1号 Soybean            Early May                         October
#365:强筋小麦                     Mid March                         July
#412:早籼稻

#Future price
future_wheat <- read_excel("AgData/raw data/Future_wheat_strong.xls")
future_soybean <- read_excel("AgData/raw data/Future_soybean1.xls")
future_corn <- read_excel("AgData/raw data/Future_corn.xls")
future_rice <- read_excel("AgData/raw data/Future_Jrice.xls")
future_Erice <- read_excel("AgData/raw data/Future_Erice.xls")

corn_soy <- rbind(future_corn,future_soybean) 

mycorn_soy_11 <- corn_soy %>% filter(`交割月_SetMon`=="11",`交易所()_ExchCd`==13)

d6 <- rbind(mycorn_soy_11,future_rice,future_wheat, future_Erice) 

d6 <- d6 %>% filter(`交割月_SetMon`=="11") %>% 
  mutate(time = `截止日期_EndDt`) %>% 
  separate(time, into = c("year", "month","day"), sep = "-") %>% 
  filter(year == `交割年_SetYr`) #filter 当年交割

d6$截止日期_EndDt<- as.Date(d6$截止日期_EndDt)
d6$`品种代码()_OptCd`<- as.character(d6$`品种代码()_OptCd`)
d7 <- d6[d6$截止日期_EndDt > as.Date("2000-12-31") & d6$截止日期_EndDt<as.Date("2023-3-8") ,] #filter data time 

###Future price for November delivery
fig_future_nov <- ggplot(data = d7, aes(x = `截止日期_EndDt`, y = `结算价(元/吨)_SetPr`/7, group = `品种代码()_OptCd`, color = `品种代码()_OptCd`))+
  geom_line(size = 1.2)+
  xlab("") + ylab("Future price ($/ton)") + ggtitle("Future Price for November Delivery")+
  scale_y_continuous(breaks=seq(100,1000,100))+
  scale_x_date(breaks = seq(from = as.Date("2005-01-01"), to = as.Date("2022-12-31"), by = "year"),
               labels = format(seq(from = as.Date("2005-04-01"), to = as.Date("2023-3-1"), by = "year"), "%m/%d/%Y"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(legend.title=element_blank(),legend.position="top") +
  scale_color_manual(name = "", values = c("#56B4E9","#E69F00", "#009E73","purple","red"), labels = c("Rice","Corn","Soybean","Wheat","Early Rice"))
#scale_color_discrete(name = "", labels = c("Corn","Soybean", "Rice"))

##Monthly average
month_avg <- d6 %>%
  group_by(year_month = format(`截止日期_EndDt`, "%Y-%m"), `品种代码()_OptCd`) %>%
  summarize(avg_value = mean(`结算价(元/吨)_SetPr`)) #monthly average


march_avg <- month_avg %>% 
  mutate(time = year_month) %>% 
  separate(year_month, into = c("year", "month"), sep = "-") %>% 
  filter(month == "03") #filter March

month_avg_11 <- d7 %>%
  group_by(year_month = format(`截止日期_EndDt`, "%Y-%m"), `品种代码()_OptCd`) %>%
  summarize(avg_value = mean(`结算价(元/吨)_SetPr`)) #monthly average

march_avg_11 <- month_avg_11 %>% 
  mutate(time = year_month) %>% 
  separate(year_month, into = c("year", "month"), sep = "-") %>% 
  filter(month == "03") #filter March

names(march_avg_11) <- c("year","month","code","price_ton_nov","time")
march_avg_11$year <- as.numeric(march_avg_11$year)


#March price for November delivery
ggplot(data = march_avg_11, aes(x = time, y = price_ton_nov/7, color = code, group = code))+
  geom_line()+
  geom_point()+
  xlab("") + ylab("Future price ($/ton)") + ggtitle("Future price in March for November delivery")+
  scale_y_continuous(breaks=seq(100,1000,100))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_color_manual(name = "", values = c("#56B4E9","#E69F00", "#009E73","purple","red"), labels = c("Rice","Corn","Soybean","Wheat","Early Rice"))


####remove early rice
march_hei <- march_avg_11 %>% filter(code != "412")

ggplot(data = march_hei, aes(x = time, y = price_ton_nov/7, color = code,group = code))+
  geom_line()+
  geom_point()+
  xlab("") + ylab("Future price ($/ton)") + ggtitle("Future price in March for November delivery")+
  scale_y_continuous(breaks=seq(100,1000,100))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_color_manual(name = "", values = c("#56B4E9","#E69F00", "#009E73","purple"), labels = c("Rice","Corn","Soybean","Wheat"))


###expected profits
cost_hei <- read_excel("AgData/raw data/cost_hei.xls", sheet = 2)
subsidy <- read_excel("AgData/raw data/subsidy_hei.xlsx", sheet = 2)

cost_hei$数值 <- as.numeric(cost_hei$数值)
cost_hei$时间 <- as.numeric(cost_hei$时间)

mycost_hei <- cost_hei %>% 
  pivot_wider(names_from = `指标`, values_from = `数值`)

names(mycost_hei) <- c("region","type","year","yield_mu_kg","cost_mu")

 mycost_hei %>% mutate(cost_kg = `cost_mu`/`yield_mu_kg`)

df <- data.frame(type = c("大豆","小麦","玉米","粳稻","早稻"), 
                 crop = c("soybean","wheat", "corn","rice", "Erice"), 
                 code = c("335","365","330","328", "412"))

mycost_hei <- mycost_hei %>% left_join(df, by = "type") #cost data with future code


profit <- mycost_hei %>% left_join(march_avg_11, key = c("code","year")) %>% #combine cost with future price
  left_join(subsidy, key = "crop")

exprofit<- profit %>% mutate(price_ex_kg = price_ton_nov/1000,
                  cost_kg = cost_mu/yield_mu_kg,
                  subsidy_kg = `subsidy/mu`/yield_mu_kg,
                  exprofit_kg = price_ex_kg - cost_kg+subsidy_kg,
                  exprofit_mu = (price_ex_kg - cost_kg+subsidy_kg)*yield_mu_kg,
                  exprofit_acre_dollor = exprofit_mu*6.07028433/7)


#actual profit
ggplot(data = exprofit, aes(y = exprofit_acre_dollor, x = year, group = crop, color = crop, linetype = crop,shape = crop)) + 
  geom_point(size = 2.5)+
  geom_line(size = 1.2)+
  xlab("Year") + ylab("Net Profit ($/acre)")+ ggtitle("Expected Profits per acre")+
  theme_bw() +
  labs(group = "crop")+
  theme(legend.title=element_blank(),legend.position="right") +
  scale_shape_discrete(name = "", labels = c("Corn","Rice","Soybean", "Wheat")) +
  scale_linetype_discrete(name = "", labels = c("Corn","Rice", "Soybean","Wheat")) +
  geom_vline(aes(xintercept = 2016), color = "red", linetype = "dashed")+
  geom_hline(aes(yintercept = 0), color = "grey")+
  scale_x_continuous(breaks=seq(2001,2021,2))+
  scale_color_manual(name = "", values = c("#E69F00","#56B4E9", "#009E73","purple"), labels = c("Corn","Rice","Soybean","Wheat"))



#subsidy data
#expected profit #November
ggplot(data = profit, aes(y = (price_nov-cost)/7*`yield/mu`*6.07028433 , x = year, group = crop, color = crop, linetype = crop,shape = crop)) + 
  geom_point(size = 2.5)+
  geom_line(size = 1.2)+
  xlab("Year") + ylab("Net Profit ($/acre)")+
  theme_bw() +
  labs(group = "crop")+
  theme(legend.title=element_blank(),legend.position="right") +
  scale_shape_discrete(name = "", labels = c("Corn","Rice","Soybean", "Wheat")) +
  scale_linetype_discrete(name = "", labels = c("Corn","Rice", "Soybean","Wheat")) +
  geom_vline(aes(xintercept = 2016), color = "red", linetype = "dashed")+
  geom_hline(aes(yintercept = 0), color = "grey")+
  scale_x_continuous(breaks=seq(2000,2020,2))+
  scale_y_continuous(breaks=seq(-300,400,100))+
  scale_color_manual(name = "", values = c("#E69F00","#56B4E9", "#009E73","purple"), labels = c("Corn","Rice","Soybean","Wheat"))


##hei
raw_hei <- read_excel("AgData/raw data/price_cost_hei.xls")
raw_hei$数值 <- as.numeric(raw_hei$数值)
raw_hei$时间 <- as.numeric(raw_hei$时间)
raw_hei <- raw_hei %>% 
  pivot_wider(names_from = `指标`, values_from = `数值`) 

names(raw_hei) <- c("region", "type","year","yield_mu","price_50kg","cost_50kg") #rename raw data

df <- data.frame(type = c("大豆","小麦","玉米","粳稻"), 
                 crop = c("soybean","wheat", "corn","rice"), 
                 code = c("335","365","330","328"))

raw_hei <- raw_hei %>% left_join(df, key = "type") %>%  #match cost data with future code
 left_join(subsidy, key = c("crop","year")) #match subsidy data

raw_hei <- raw_hei %>% 
  mutate(price = price_50kg/50,
         cost = cost_50kg/50,
         subsidy = `subsidy/mu`/yield_mu,
         price_lag1 = lag(price, 1),
         cost_lag1 = lag(cost,1),
         subsidy_lag1 = lag(subsidy,1),
         subsidy_mu_lag1 = lag(`subsidy/mu`,1),
         yield_mu_lag1 = lag(yield_mu,1),
         yield_mu_lag2 = lag(yield_mu,2)) #per kg

raw_dat <- raw_hei %>% 
  left_join(march_avg_11, key = c("code","year")) %>% #match future price
  mutate(ex_price = price_ton_nov/1000,
         ex_pro_lag1 = (ex_price- cost_lag1)*yield_mu_lag1+`subsidy/mu`,
         ex_pro_lag2 = (ex_price- cost_lag1)*yield_mu_lag2+`subsidy/mu`,
         ex_pro_lag3 = (ex_price- cost_lag1)*yield_mu_lag1+`subsidy/mu`,
         pro_actual_lag1 = (price_lag1- cost_lag1)*yield_mu_lag1+`subsidy/mu`,
         ex_pro_acre = ex_pro_lag1 * acre/7,
         ex_pro_acre2 = ex_pro_lag2 * acre/7,#lag2 yield
         ex_pro_acre3 = ex_pro_lag3 * acre/7,#lag subsidy
         ex_pro_acre4 = ((ex_price- cost_lag1)*yield_mu+`subsidy/mu`)* acre/7,
         ps_pro_acre = pro_actual_lag1 * acre/7)

names(hei_acre) <- c("crop", "region","year", "value","share","total_crop")
hei_acre$crop <- as.character(hei_acre$crop)

raw_dat<- raw_dat %>% left_join(hei_acre, by = c("crop","year")) 


#save the data
dta_pre <- raw_dat %>% 
  select(c("crop","code","year","share","ex_price","price","cost",`subsidy/mu`,"yield_mu")) %>% 
  mutate_if(is.numeric, ~round(., 2))

write_xlsx(dta_pre, "mypredata.xlsx")
write_xlsx(month_avg, "month_avg.xlsx")

lm(data = dta_pre, share ~ ex_price + cost + `subsidy/mu` + yield_mu + crop ) %>% summary()




###expected profits，Future price, lagged cost and yield, current subsidy
ggplot(data = raw_dat, aes(y = ex_pro_acre, x = year, group = crop, color = crop, linetype = crop,shape = crop)) + 
  geom_point(size = 2.5)+
  geom_line(size = 1.2)+
  xlab("Year") + ylab("Net Profit ($/acre)")+ggtitle("Expected Profits in March Using Future Prices")+
  #theme_bw() +
  labs(group = "crop")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(legend.title=element_blank(),legend.position="right") +
  scale_shape_discrete(name = "", labels = c("Corn","Rice","Soybean", "Wheat")) +
  scale_linetype_discrete(name = "", labels = c("Corn","Rice", "Soybean","Wheat")) +
  geom_vline(aes(xintercept = 2016), color = "red", linetype = "dashed")+
  geom_hline(aes(yintercept = 0), color = "grey")+
  scale_x_continuous(breaks=seq(2000,2022,1))+
  scale_y_continuous(breaks=seq(-300,500,100))+
  scale_color_manual(name = "", values = c("#E69F00","#56B4E9", "#009E73","purple"), labels = c("Corn","Rice","Soybean","Wheat"))


###Future price, lag subsidy
ggplot(data = raw_dat, aes(y = ex_pro_acre3, x = year, group = crop, color = crop, linetype = crop,shape = crop)) + 
  geom_point(size = 2.5)+
  geom_line(size = 1.2)+
  xlab("Year") + ylab("Net Profit ($/acre)")+ggtitle("Expected Profits, Last-year Subsidy")+
  #theme_bw() +
  labs(group = "crop")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(legend.title=element_blank(),legend.position="right") +
  scale_shape_discrete(name = "", labels = c("Corn","Rice","Soybean", "Wheat")) +
  scale_linetype_discrete(name = "", labels = c("Corn","Rice", "Soybean","Wheat")) +
  geom_vline(aes(xintercept = 2016), color = "red", linetype = "dashed")+
  geom_hline(aes(yintercept = 0), color = "grey")+
  scale_x_continuous(breaks=seq(2000,2022,1))+
  scale_y_continuous(breaks=seq(-300,500,100))+
  scale_color_manual(name = "", values = c("#E69F00","#56B4E9", "#009E73","purple"), labels = c("Corn","Rice","Soybean","Wheat"))

###Future price, lag subsidy, current yield
ggplot(data = raw_dat, aes(y = ex_pro_acre4, x = year, group = crop, color = crop, linetype = crop,shape = crop)) + 
  geom_point(size = 2.5)+
  geom_line(size = 1.2)+
  xlab("Year") + ylab("Net Profit ($/acre)")+ggtitle("Expected Profits, current yield, lagged subsidy")+
  #theme_bw() +
  labs(group = "crop")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(legend.title=element_blank(),legend.position="right") +
  scale_shape_discrete(name = "", labels = c("Corn","Rice","Soybean", "Wheat")) +
  scale_linetype_discrete(name = "", labels = c("Corn","Rice", "Soybean","Wheat")) +
  geom_vline(aes(xintercept = 2016), color = "red", linetype = "dashed")+
  geom_hline(aes(yintercept = 0), color = "grey")+
  scale_x_continuous(breaks=seq(2000,2022,1))+
  scale_y_continuous(breaks=seq(-300,500,100))+
  scale_color_manual(name = "", values = c("#E69F00","#56B4E9", "#009E73","purple"), labels = c("Corn","Rice","Soybean","Wheat"))



###expected profits，last price, lagged cost and yield
ggplot(data = raw_dat, aes(y = ps_pro_acre, x = year, group = crop, color = crop, linetype = crop,shape = crop)) + 
  geom_point(size = 2.5)+
  geom_line(size = 1.2)+
  xlab("Year") + ylab("Net Profit ($/acre)")+ggtitle("Expected Profits Using Lagged Prices")+
  theme_bw() +
  labs(group = "crop")+
  theme(legend.title=element_blank(),legend.position="right") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_shape_discrete(name = "", labels = c("Corn","Rice","Soybean", "Wheat")) +
  scale_linetype_discrete(name = "", labels = c("Corn","Rice", "Soybean","Wheat")) +
  geom_vline(aes(xintercept = 2016), color = "red", linetype = "dashed")+
  geom_hline(aes(yintercept = 0), color = "grey")+
  scale_x_continuous(breaks=seq(2001,2022,1))+
  scale_y_continuous(breaks=seq(-300,400,100))+
  scale_color_manual(name = "", values = c("#E69F00","#56B4E9", "#009E73","purple"), labels = c("Corn","Rice","Soybean","Wheat"))







###Combine the three lines
ggplot(data = raw_dat, aes(y = pro_actual_lag1, x = year, group = crop, color = crop, linetype = crop,shape = crop)) + 
  geom_point(size = 2.5)+
  geom_line(aes(x = year, y = pro_actual_lag1, color = "red"), size = 1.2)+
  geom_line(aes(x = year, y = ex_pro_lag1, color = "blue"), size = 1.2)+
  xlab("Year") + ylab("Net Profit ($/acre)")+ggtitle("Expected Profits in March Using Past Prices")+
  theme_bw() +
  labs(group = "crop")+
  theme(legend.title=element_blank(),legend.position="right") +
  scale_shape_discrete(name = "", labels = c("Corn","Rice","Soybean", "Wheat")) +
  scale_linetype_discrete(name = "", labels = c("Corn","Rice", "Soybean","Wheat")) +
  geom_vline(aes(xintercept = 2016), color = "red", linetype = "dashed")+
  geom_hline(aes(yintercept = 0), color = "grey")+
  scale_x_continuous(name = "Actual",
                     sec.axis = sec_axis(~./100,name = "Future"))+
  scale_y_continuous(breaks=seq(-300,400,100))+
  scale_color_manual(name = "", values = c("#E69F00","#56B4E9", "#009E73","purple"), labels = c("Corn","Rice","Soybean","Wheat"))



###### Plot expected_profit (use last year price), expected_profit (use expected price) and share for each crop
ggplot(data = raw_dat) + 
  # geom_point(aes(x = year, y = pro_actual_lag1), size = 0.5) + 
  geom_point(aes(x = year, y = ex_pro_lag1), size = 0.5) + 
  geom_point(aes(x = year, y = (share - 0.2) * 1000), size = 0.5) + 
  # geom_line(aes(x = year, y = pro_actual_lag1, color = "Profit Using Previous Price"), size = 1)+
  geom_line(aes(x = year, y = (share - 0.2) * 1000, color = "Share (%)"), size = 1)+
  geom_line(aes(x = year, y = ex_pro_lag1, color = "Profit Using Future Price"), size = 1)+
  scale_y_continuous(
    name = "Expected Profit",
    sec.axis = sec_axis(~ . /10 + 20, name = "Acreage Share")) + 
  scale_x_continuous(breaks=seq(2001,2021,2))+
  facet_wrap(.~crop, nrow = 2) + 
  theme(legend.position='bottom')

###plot expected profits using historical price
ggplot(data = raw_dat) + 
  # geom_point(aes(x = year, y = pro_actual_lag1), size = 0.5) + 
  geom_point(aes(x = year, y = pro_actual_lag1), size = 0.5) + 
  geom_point(aes(x = year, y = (share - 0.2) * 1000), size = 0.5) + 
  # geom_line(aes(x = year, y = pro_actual_lag1, color = "Profit Using Previous Price"), size = 1)+
  geom_line(aes(x = year, y = (share - 0.2) * 1000, color = "Acreage Share (%)"), size = 1)+
  geom_line(aes(x = year, y = pro_actual_lag1, color = "Profit using Historical Price"), size = 1)+
  scale_y_continuous(
    name = "Expected Profits",
    sec.axis = sec_axis(~ . /10 + 20, name = "Acreage Share")) + 
  scale_x_continuous(breaks=seq(2001,2021,2))+
  facet_wrap(.~crop, nrow = 2) + 
  theme(legend.position='bottom')



ggplot(data) +
  geom_line(aes(x = x, y = y1, color = "y1"), size = 1) +
  geom_line(aes(x = x, y = y2 * conversion_factor, color = "y2"), size = 1) +
  scale_y_continuous(
    name = "Y1",
    sec.axis = sec_axis(~ . / conversion_factor, name = "Y2")
  ) +
  labs(color = "Variable", x = "X") +
  theme_minimal()





