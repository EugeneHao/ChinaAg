library(tidyverse)
library(readxl)
library(lubridate)


                                #Planting season                  Harvest season    
#330:玉米 Corn                    Late April                        October
#328:粳稻 Rice                    Early May                         October
#335:黄大豆1号 Soybean            Early May                         October
#365:强筋小麦                     Mid March                         July
#412:早籼稻


#Planting pattern in Northeastern

future_corn <- read_excel("AgData/raw data/Future_corn.xls")

d1 <- future_corn %>% filter(`交易所()_ExchCd`==13, `交割月_SetMon`=="09")
d1$截止日期_EndDt<- as.Date(d1$截止日期_EndDt)

###Corn historical future price
ggplot(data = d1, aes(x = `截止日期_EndDt`, y = `结算价(元/吨)_SetPr`/7/39.3679))+
  geom_line()+
  xlab("") + ylab("Future price ($/Bushel)") + ggtitle("Maturity September")+
  scale_y_continuous(breaks=seq(4,12,1))+
  scale_x_date(breaks = seq(from = as.Date("2005-01-01"), to = as.Date("2022-12-31"), by = "year"),
               labels = format(seq(from = as.Date("2005-04-01"), to = as.Date("2022-12-31"), by = "year"), "%m/%d/%Y"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


###Corn April month average
monthly_avg <- d1 %>%
  group_by(year_month = format(`截止日期_EndDt`, "%Y-%m")) %>%
  summarize(avg_value = mean(`结算价(元/吨)_SetPr`)) #monthly average

april_avg <- monthly_avg %>% 
  mutate(time = year_month) %>% 
  separate(year_month, into = c("year", "month"), sep = "-") %>% 
  filter(month == "04") #filter Aprli


ggplot(data = april_avg, aes(x = time, y = avg_value/7/39.3679))+
  geom_line(group = 1)+
  geom_point()+
  xlab("") + ylab("Future price ($/Bushel)") + ggtitle("Maturity September")+
  scale_y_continuous(breaks=seq(4,12,1))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### add Soybean
future_soybean <- read_excel("AgData/raw data/Future_soybean1.xls")
d2 <- rbind(future_corn,future_soybean) 

d2 <- d2 %>% filter(`交割月_SetMon`=="09",`交易所()_ExchCd`==13) %>% #13 DCE
  mutate(time = `截止日期_EndDt`) %>% 
  separate(time, into = c("year", "month","day"), sep = "-") %>% 
  filter(year == `交割年_SetYr`, 截止日期_EndDt > 2004-12-31)

d2$截止日期_EndDt<- as.Date(d2$截止日期_EndDt)
d2$`品种代码()_OptCd`<- as.character(d2$`品种代码()_OptCd`)
d3 <- d2[d2$截止日期_EndDt>as.Date("2004-12-31") & d2$截止日期_EndDt<as.Date("2022-12-31") ,] #filter data time 

ggplot(data = d3, aes(x = `截止日期_EndDt`, y = `结算价(元/吨)_SetPr`/7, group = `品种代码()_OptCd`, color = `品种代码()_OptCd`))+
  geom_line(size = 1.5)+
  xlab("") + ylab("Future price ($/ton)") + ggtitle("Maturity September")+
  scale_y_continuous(breaks=seq(100,1000,100))+
  scale_x_date(breaks = seq(from = as.Date("2005-01-01"), to = as.Date("2022-12-31"), by = "year"),
               labels = format(seq(from = as.Date("2005-04-01"), to = as.Date("2022-12-31"), by = "year"), "%m/%d/%Y"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(legend.title=element_blank(),legend.position="top") +
  scale_color_discrete(name = "", labels = c("Corn","Soybean"))

###add rice:粳稻 
future_soybean <- read_excel("AgData/raw data/Future_soybean1.xls")
future_corn <- read_excel("AgData/raw data/Future_corn.xls")
future_rice <- read_excel("AgData/raw data/Future_Jrice.xls")
future_Erice <- read_excel("AgData/raw data/Future_Erice.xls")


corn_soy <- rbind(future_corn,future_soybean) 

mycorn_soy <- corn_soy %>% filter(`交割月_SetMon`=="09",`交易所()_ExchCd`==13)

d4 <- rbind(mycorn_soy,future_rice) 

d4 <- d4 %>% filter(`交割月_SetMon`=="09") %>% 
  mutate(time = `截止日期_EndDt`) %>% 
  separate(time, into = c("year", "month","day"), sep = "-") %>% 
  filter(year == `交割年_SetYr`, 截止日期_EndDt > 2004-12-31)

d4$截止日期_EndDt<- as.Date(d4$截止日期_EndDt)
d4$`品种代码()_OptCd`<- as.character(d4$`品种代码()_OptCd`)
d5 <- d4[d4$截止日期_EndDt > as.Date("2004-12-31") & d4$截止日期_EndDt<as.Date("2022-12-31") ,] #filter data time 

ggplot(data = d5, aes(x = `截止日期_EndDt`, y = `结算价(元/吨)_SetPr`/7, group = `品种代码()_OptCd`, color = `品种代码()_OptCd`))+
  geom_line(size = 1.5)+
  xlab("") + ylab("Future price ($/ton)") + ggtitle("Maturity September")+
  scale_y_continuous(breaks=seq(100,1000,100))+
  scale_x_date(breaks = seq(from = as.Date("2005-01-01"), to = as.Date("2022-12-31"), by = "year"),
               labels = format(seq(from = as.Date("2005-04-01"), to = as.Date("2022-12-31"), by = "year"), "%m/%d/%Y"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(legend.title=element_blank(),legend.position="top") #+
  #scale_color_discrete(name = "", labels = c("Corn","Soybean", "Rice"))





###add wheat：4 crops: Maturity in September
future_wheat <- read_excel("AgData/raw data/Future_wheat_strong.xls")
future_soybean <- read_excel("AgData/raw data/Future_soybean1.xls")
future_corn <- read_excel("AgData/raw data/Future_corn.xls")
future_rice <- read_excel("AgData/raw data/Future_Jrice.xls")
future_Erice <- read_excel("AgData/raw data/Future_Erice.xls")

corn_soy <- rbind(future_corn,future_soybean) 

mycorn_soy <- corn_soy %>% filter(`交割月_SetMon`=="09",`交易所()_ExchCd`==13)

d4 <- rbind(mycorn_soy,future_rice,future_Erice,future_wheat) 

d4 <- d4 %>% filter(`交割月_SetMon`=="09") %>% 
  mutate(time = `截止日期_EndDt`) %>% 
  separate(time, into = c("year", "month","day"), sep = "-") %>% 
  filter(year == `交割年_SetYr`, 截止日期_EndDt > 2004-12-31) #filter 当年交割

d4$截止日期_EndDt<- as.Date(d4$截止日期_EndDt)
d4$`品种代码()_OptCd`<- as.character(d4$`品种代码()_OptCd`)
d5 <- d4[d4$截止日期_EndDt > as.Date("2004-12-31") & d4$截止日期_EndDt<as.Date("2022-12-31") ,] #filter data time 


fig_future_sep <- ggplot(data = d5, aes(x = `截止日期_EndDt`, y = `结算价(元/吨)_SetPr`/7, group = `品种代码()_OptCd`, color = `品种代码()_OptCd`))+
  geom_line(size = 1.2)+
  xlab("") + ylab("Future price ($/ton)") + ggtitle("Future Price for September Delivery")+
  scale_y_continuous(breaks=seq(100,1000,100))+
  scale_x_date(breaks = seq(from = as.Date("2005-01-01"), to = as.Date("2022-12-31"), by = "year"),
               labels = format(seq(from = as.Date("2005-04-01"), to = as.Date("2022-12-31"), by = "year"), "%m/%d/%Y"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(legend.title=element_blank(),legend.position="top") +
 scale_color_manual(name = "", values = c("#56B4E9","#E69F00", "#009E73","purple","red"), labels = c("Rice","Corn","Soybean","Wheat","Early Rice"))
#scale_color_discrete(name = "", labels = c("Corn","Soybean", "Rice","Early Rice"))

ggsave("Figure/fig_future_sep.png",width = 8, height = 6)

###add wheat：4 crops: Maturity in November
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
  filter(year == `交割年_SetYr`, 截止日期_EndDt > 2004-12-31) #filter 当年交割

d6$截止日期_EndDt<- as.Date(d6$截止日期_EndDt)
d6$`品种代码()_OptCd`<- as.character(d6$`品种代码()_OptCd`)
d7 <- d6[d6$截止日期_EndDt > as.Date("2004-12-31") & d6$截止日期_EndDt<as.Date("2022-12-31") ,] #filter data time 


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

ggsave("Figure/fig_future_nov.png",width = 8, height = 6)


###Monthly averaged trend
month_avg <- d5 %>%
  group_by(year_month = format(`截止日期_EndDt`, "%Y-%m"), `品种代码()_OptCd`) %>%
  summarize(avg_value = mean(`结算价(元/吨)_SetPr`)) #monthly average

april_avg <- month_avg %>% 
  mutate(time = year_month) %>% 
  separate(year_month, into = c("year", "month"), sep = "-") %>% 
  filter(month == "04") #filter Aprli

march_avg <- month_avg %>% 
  mutate(time = year_month) %>% 
  separate(year_month, into = c("year", "month"), sep = "-") %>% 
  filter(month == "03") #filter March

###Monthly trend, September maturity
fig_sep <- ggplot(data = april_avg, aes(x = time, y = avg_value/7, color = `品种代码()_OptCd`,group = `品种代码()_OptCd`))+
  geom_line()+
  geom_point()+
  xlab("") + ylab("Future price ($/ton)") + ggtitle("Maturity September")+
  scale_y_continuous(breaks=seq(100,1000,100))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_color_manual(name = "", values = c("#56B4E9","#E69F00", "#009E73","purple"), labels = c("Rice","Corn","Soybean","Wheat"))
  
ggsave("Figure/fig_sep.png",width = 6, height = 4)


###Monthly trend, November maturity for March planting
month_avg_11 <- d7 %>%
  group_by(year_month = format(`截止日期_EndDt`, "%Y-%m"), `品种代码()_OptCd`) %>%
  summarize(avg_value = mean(`结算价(元/吨)_SetPr`)) #monthly average

april_avg_11 <- month_avg_11 %>% 
  mutate(time = year_month) %>% 
  separate(year_month, into = c("year", "month"), sep = "-") %>% 
  filter(month == "03") #filter March

fig_nov <- ggplot(data = april_avg_11, aes(x = time, y = avg_value/7, color = `品种代码()_OptCd`,group = `品种代码()_OptCd`))+
  geom_line()+
  geom_point()+
  xlab("") + ylab("Future price ($/ton)") + ggtitle("Future price in March for November delivery")+
  scale_y_continuous(breaks=seq(100,1000,100))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_color_manual(name = "", values = c("#56B4E9","#E69F00", "#009E73","purple","red"), labels = c("Rice","Corn","Soybean","Wheat","Early Rice"))

ggsave("Figure/fig_nov.png",width = 6, height = 4)


### replace with future price
### net profit/acre = price * quantity/acre - cost/acre
### expected profit/acre = future price * quantity/acre - cost/acre 
###                     


cost_benefit <- read_excel("AgData/raw data/cost_benefit.xls")
cost_benefit$数值 <- as.numeric(cost_benefit$数值)
cost_benefit$时间 <- as.numeric(cost_benefit$时间)

mycost <- cost_benefit %>% 
  pivot_wider(names_from = `指标`, values_from = `数值`)

names(mycost) <- c("type","year","yield/mu","cost/mu","profit/mu","price/50kg","cost/50kg","profit/50kg")

mycost <- mycost %>% mutate(`cost_actual/50kg` = `price/50kg`-`profit/50kg`)

cb_kg <- mycost %>% select(-`cost/mu`,-`profit/mu`) %>% #original is per 50kg
  mutate(price = `price/50kg`/50, #per kg
         cost = `cost/50kg`/50,
         profit = `profit/50kg`/50) %>% 
  select(-`price/50kg`,-`cost/50kg`,-`profit/50kg`) %>% 
  mutate(yield_acre = `yield/mu`*6.07028433) 

df <- data.frame(type = c("大豆","小麦","玉米","稻谷"), 
           crop = c("soybean","wheat", "corn","rice"), 
           code = c("335","365","330","328"))
midcb <- cb_kg %>% left_join(df, by = "type")

#September
names(april_avg) <- c("year","month","code","price/ton/sep","time")
april_avg$year <- as.numeric(april_avg$year)

#Novermber
names(april_avg_11) <- c("year","month","code","price/ton/nov","time")
april_avg_11$year <- as.numeric(april_avg_11$year)

dta_plot <- midcb %>% left_join(april_avg_11, key = "code") %>% 
  mutate(price_nov = `price/ton/nov`/1000) %>% 
  left_join(april_avg, key ="code") %>% 
  mutate(price_sep = `price/ton/sep`/1000) 

#actual profit
actual_profit <- ggplot(data = dta_plot, aes(y = (price-cost)/7*`yield_acre`  , x = year, group = crop, color = crop, linetype = crop,shape = crop)) + 
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
  scale_color_manual(name = "", values = c("#E69F00","#56B4E9", "#009E73","purple"), labels = c("Corn","Rice","Soybean","Wheat"))

ggsave("Figure/actual_profit.png",width = 8, height = 4)

#expected profit #September

fig_expprofit_sep <- ggplot(data = dta_plot, aes(y = (price_sep-cost)/7*yield_acre , x = year, group = crop, color = crop, linetype = crop,shape = crop)) + 
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

ggsave("Figure/fig_expprofit_sep.png",width = 8, height = 4)

#expected profit #November
fig_expprofit_nov <- ggplot(data = dta_plot, aes(y = (price_nov-cost)/7*`yield/mu`*6.07028433 , x = year, group = crop, color = crop, linetype = crop,shape = crop)) + 
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

ggsave("Figure/fig_expprofit_nov.png",width = 8, height = 4)
###subsidy





