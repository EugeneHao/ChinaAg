library(tidyverse)
setwd("/Users/xiaolanwan/Documents/GitHub/ChinaAg")


dta$corn_acre <- as.numeric(dta$corn_acre)
dta$price <- as.numeric(dta$price)

plot(x = dta$year, y = dta$price)
plot(x = dta$corn_acre[dta$region == "Nation"])
plot(x = dta$corn_acre[dta$region == "Heilongjiang"])

dta_n <- dta[dta$region == "Nation",]
dta_h <- dta[dta$region == "Heilongjiang",]

price_lag <- dta_n$price[-21]
price_lag1 <- c(NA, price_lag)

lm(corn_acre ~ price, data = dta) %>% summary() #current price and corn acreage, not significant
lm(dta_n$corn_acre ~ price_lag1) %>% summary() #last price and nation acreage, very significant
lm(dta_h$corn_acre ~ price_lag1) %>% summary() #last price and heilongjiang acreage, very significant



###Start from here
###plot price trend for corn and rice
library(tidyverse)
dta_p <- read_excel("~/Desktop/2021 Fall/Project/2.Agricultural Product/Data/price_Jan17.xls")
dta_p <- dta_p[,-1]
dta_p$value <- as.numeric(dta_p$value)
wide_p<- pivot_wider(dta_p, names_from = index, values_from = value)

riceandcorn <- dta_p[dta_p$index=="cornp"|dta_p$index=="ricep",]
riceandcorn

#plot(x = riceandcorn$time, y = riceandcorn$value,  pch = riceandcorn$index)
#lines(x = riceandcorn$time, y = riceandcorn$value, col = "red")

### price trend -- corn and rice
ggplot(data = riceandcorn, aes(y = value/50, x = time, group = index,  shape = index, linetype = index)) + 
  geom_point()+
  geom_line()+
  xlab("Year") + ylab("Price (yuan/kg)")+
  theme_bw() +
  labs(group = "type")+
  theme(legend.title=element_blank(),legend.position="right") +
  scale_shape_discrete(name = "type", labels = c("Corn","Rice")) +
  scale_linetype_discrete(name = "type", labels = c("Corn","Rice")) +
  geom_vline(aes(xintercept = "2016"), color = "red", linetype = "dashed")

### Price trend --- Corn, rice, soybean
new <- read_excel("~/Desktop/2021 Fall/Project/2.Agricultural Product/Data/price_1.30.xls")
new$value <- as.numeric(new$value)
new$time <-as.numeric(new$time)
write.csv(new, file = "price_hjl.csv") #save price data


ggplot(data = new, aes(y = value/50/7*25.4, x = time, group = type,  shape = type, linetype = type)) + 
  geom_point(size = 2.5)+
  geom_line(size = 1.2)+
  xlab("Year") + ylab("Price ($/Bushel)")+
  theme_bw() +
  labs(group = "type")+
  theme(legend.title=element_blank(),legend.position="right") +
  #scale_shape_discrete(name = "type", labels = c("Corn","Rice","Soybean")) +
  #scale_linetype_discrete(name = "type", labels = c("Corn","Rice", "Soybean")) +
  geom_vline(aes(xintercept = 2016), color = "red", linetype = "dashed")+
  scale_x_continuous(breaks=seq(2000,2020,2))
  
###price trend: corn, rice, soybean, and wheat
ptdata <- read_excel("AgData/raw data/1.profit.xls")
ptdata$year <- as.numeric(ptdata$year)
ptdata$value <- as.numeric(ptdata$value)

ptonly <- ptdata %>% filter(indicator == "price") %>% as.data.frame()

ggplot(data = ptonly, aes(y = value/50/7, x = year, group = type, color = type, shape = type, linetype = type)) + 
  geom_point(size = 2.5)+
  geom_line(size = 1.2)+
  xlab("Year") + ylab("Price ($/kg)")+
  theme_bw() +
  labs(group = "type")+
  theme(legend.title=element_blank(),legend.position="right") +
  #scale_shape_discrete(name = "type", labels = c("Corn","Rice","Soybean")) +
  #scale_linetype_discrete(name = "type", labels = c("Corn","Rice", "Soybean")) +
  geom_vline(aes(xintercept = 2016), color = "red", linetype = "dashed")+
  scale_x_continuous(breaks=seq(2000,2020,2))


###Net profits: corn, rice, soybean, and wheat

net_pf <- ptdata %>% filter(indicator == "netprofit") %>% as.data.frame()

ggplot(data = net_pf, aes(y = value/7*6.07028433 , x = year, group = type, color = type, shape = type, linetype = type)) + 
  geom_point(size = 2.5)+
  geom_line(size = 1.2)+
  xlab("Year") + ylab("Net Profit ($/acre)")+
  theme_bw() +
  labs(group = "type")+
  theme(legend.title=element_blank(),legend.position="right") +
  #scale_shape_discrete(name = "type", labels = c("Corn","Rice","Soybean")) +
  #scale_linetype_discrete(name = "type", labels = c("Corn","Rice", "Soybean")) +
  geom_vline(aes(xintercept = 2016), color = "red", linetype = "dashed")+
  geom_hline(aes(yintercept = 0), color = "grey")+
  scale_x_continuous(breaks=seq(2000,2020,2))
  





 ### Acreage share for corn, rice, and soybean -- Northeastern area
dta <- read_excel("~/Desktop/2021 Fall/Project/2.Agricultural Product/Data/acre_Jan17.xls", sheet = 3)
dta$value <- as.numeric(dta$value)


aggre <- dta %>% 
  group_by(index, time) %>% #calculate aggregate acreage for heilong, jilin,liao
  summarise(aggr = sum(value)) %>% 
  ungroup %>% as.data.frame()
 
newdat <- dta %>% left_join(aggre, by = c("index","time")) 
c1 <- aggre[aggre$index == "grainacre", 3] #get total grain acreage in North
newdat <- cbind(newdat, graintotal = rep(c1, each = 12))
 
newdat <- newdat %>% mutate(share = aggr/graintotal) #get share value
 
write.csv(newdat, file = "newshare.csv") #to add other share
 
dtshare <- read_excel("~/Desktop/2021 Fall/Project/2.Agricultural Product/Data/acre_Jan17.xls", sheet = 6)

dtshare$index <- factor(dtshare$index, levels = c("Other", "Soybean", "Rice","Corn")) #Order the legend

ggplot(data = dtshare, aes(x = time, y = share*100, fill = index)) + 
  geom_col()+
  theme_bw()+
  xlab("Year") + 
  ylab("Acreage share (%)") +
  theme(legend.title=element_blank(),legend.position="bottom") +
  scale_fill_manual(values = c("#999999", "#009E73","#56B4E9",  "#E69F00"))+
  scale_x_continuous( breaks=seq(2000,2020,2))




### regression "HJL"
head(new) #price trend for corn, rice, soybean 
reg1 <- read_csv("~/Desktop/2021 Fall/Project/2.Agricultural Product/Data/reg_hlj.csv") #reg data

lag <- reg1$price[-c(63)] 
reg1$lag1 <- c(NA,lag) #create lagged price




m1 <- lm(share ~ lag1 + I(index)  , data = reg1)
summary(m1)

### X variables: price, labor, fertilizer
acre_14province <- read_excel("~/Desktop/2021 Fall/Project/2.Agricultural Product/Data/acre_Jan17.xls", sheet = 8) #acreage share
variables <- read_excel("~/Desktop/2021 Fall/Project/2.Agricultural Product/Data/variables-price-labor-fertilizer.xls") #X variabels

vari_wide <- variables %>% pivot_wider(names_from = indicator, values_from = value) #pivot X variables into wide data


usethis <- vari_wide %>% left_join(acre_14province, by =c("index","region","time") ) %>% 
  as.data.frame() #combine acreage share y and x variables

hjl <- usethis[usethis$region %in% c("Heilongjiang", "Jilin", "Liaoning"),] 
head(hjl)

variables$region["Heilongjiang"]
hjl <- variables[variables$region %in% c("Heilongjiang", "Jilin", "Liaoning"),]
vari_wide[variables$region %in% c("Heilongjiang", "Jilin", "Liaoning"),]

hjl %>% spread(key = indicator, value = value) -> view()
hjl[,-c("indi_chinese","region_chinese")]

### Acreage share for corn, rice, and soybean -- Nation
share_nation<- usethis[usethis$region=="Nation",]  #filter nation obs

#insert a subgroup
share_nation1 <- share_nation %>% group_by(time) %>% 
  mutate(other = 1-sum(share)) %>% # create other share
  ungroup %>% as.data.frame() %>% #ungroup "time"
  pivot_wider(names_from = index, values_from = share) %>% #wide crop type
  pivot_longer(c("corn", "rice","soybean", "other"), names_to = "index", values_to = "share") #pivot to longer data


na_ac<- read_excel("~/Desktop/2021 Fall/Project/2.Agricultural Product/Data/national acre_2.6.xls", sheet = 3)
naac1 <- na_ac %>% pivot_wider(names_from = index, values_from = value) %>% 
  pivot_longer(c("soybeanacre":"riceacre"), names_to = "index", values_to = "value") %>% #get grain acreage column
  mutate(share = value/grainacre) %>% #get share column
  group_by(time) %>%  mutate(other = 1-sum(share)) %>%  #create other share
  ungroup %>% as.data.frame()


###plot national acreage trend lines
ggplot(data = naac1, aes(y = value/1000, x = time, group = index, color = index, linetype = index)) + 
  geom_point()+
  geom_line(size = 1.2)+
  xlab("Year") + ylab("Planted Acreage (Million hectares)")+
  theme_bw() +
  labs(group = "index") + 
  scale_linetype_discrete(name = "", labels = c("Corn","Rice", "Soybean")) +
  theme(legend.title=element_blank(),legend.position="right") +
  scale_x_continuous( breaks=seq(2000,2020,2)) + #X轴刻度
  scale_color_manual(name = "", values = c("#E69F00","#56B4E9", "#009E73"), labels = c("Corn","Rice","Soybean"))
  #geom_vline(aes(xintercept = "2016"), color = "red", linetype = "dashed")
  #scale_shape_discrete(name = "", labels = c("Corn","Rice","Soybean")) +
  #scale_color_discrete(name = "", labels = c("Corn","Rice","Soybean")) + 

plot(naac1$index)



###National Acreage for corn, rice, soybean, and wheat
dtacre<- read_excel("AgData/raw data/3.acreage.xls")
dtacre$value <- as.numeric(dtacre$value)
dtacre$year <-as.numeric(dtacre$year)

dtacre2 <- dtacre %>% filter(indicator!="grain") %>% as.data.frame()


ggplot(data = dtacre2, aes(y = value/1000, x = year, group = indicator, color = indicator)) + 
  geom_point()+
  geom_line(size = 1.5)+
  xlab("Year") + ylab("Planted Acreage (Million hectares)")+
  theme_bw() +
  labs(group = "index") + 
  scale_linetype_discrete(name = "", labels = c("Corn","Rice", "Soybean","Wheat")) +
  theme(legend.title=element_blank(),legend.position="right") +
  scale_x_continuous( breaks=seq(2000,2020,2)) + #X轴刻度
  scale_color_manual(name = "", values = c("#E69F00","#56B4E9", "#009E73","purple"), labels = c("Corn","Rice","Soybean","Wheat"))
#geom_vline(aes(xintercept = "2016"), color = "red", linetype = "dashed")
#scale_shape_discrete(name = "", labels = c("Corn","Rice","Soybean")) +
#scale_color_discrete(name = "", labels = c("Corn","Rice","Soybean")) + 


###National Acreage share for corn, rice, soybean, and wheat
dtacre1 <- dtacre %>% filter(indicator=="grain")

mid <- cbind(dtacre2, dtacre1) 
acre_share <- mid[,c(1,2,3,6)] 
names(acre_share) <- c("type","year","value","grain")
acre_share$share = acre_share$value/acre_share$grain
acre_share$type <- factor(acre_share$type, levels = c("soybean", "wheat", "rice","corn"))

ggplot(data = acre_share, aes(x = year, y = share*100, fill = type)) + 
  geom_col()+
  theme_bw()+
  xlab("Year") + 
  ylab("Acreage share (%)") +
  theme(legend.title=element_blank(),legend.position="bottom") +
  scale_fill_manual(values = c("#009E73","purple","#56B4E9",  "#E69F00"),labels = c("Soybean","Wheat","Corn","Rice"))+
  scale_x_continuous( breaks=seq(2000,2020,2))+
  scale_y_continuous( breaks=seq(0,100,10))


###Heilongjiang,Jilin,Liaonign
dt_acre_Hei<- read_excel("AgData/raw data/4.ne.xls")

dt_acre_Hei$value <- as.numeric(dt_acre_Hei$value)
dt_acre_Hei$year <-as.numeric(dt_acre_Hei$year)

mid1 <- dt_acre_Hei[dt_acre_Hei$type=="grain",]
mid2 <- dt_acre_Hei[dt_acre_Hei$type!="grain",]

mid3 <- cbind(mid2,mid1)
mid4 <- mid3[,c(1,2,3,4,8)]
names(mid4) <- c("type","region","year","value","grain")

### four provinces
mid4$value <- ifelse(is.na(mid4$value), 0, mid4$value) #replace NA with 0

Acre_Four<- mid4 %>% group_by(type,year) %>% 
  summarise(grain_sum = sum(grain),
            value_sum = sum(value)) %>% ungroup() %>% 
  mutate(share = value_sum/grain_sum)

Acre_Four$type <- factor(Acre_Four$type, levels = c("wheat","soybean","rice","corn"))

ggplot(data = Acre_Four, aes(x = year, y = share*100, fill = type)) + 
  geom_col()+
  theme_bw()+
  xlab("Year") + 
  ylab("Acreage share (%)") +
  theme(legend.title=element_blank(),legend.position="bottom") +
  scale_fill_manual(values = c("purple","#009E73","#56B4E9",  "#E69F00"),labels = c("Wheat","Soybean","Corn","Rice"))+
  scale_x_continuous( breaks=seq(2000,2020,2))+
  scale_y_continuous( breaks=seq(0,100,10))

###Three provinces

Acre_Three <- mid4 %>% filter(region!="Inner mogolia") %>% 
  group_by(type,year) %>% 
  summarise(grain_sum = sum(grain),
            value_sum = sum(value)) %>% ungroup() %>% 
  mutate(share = value_sum/grain_sum)

Acre_Three$type <- factor(Acre_Three$type, levels = c("wheat","soybean","rice","corn"))

ggplot(data = Acre_Three, aes(x = year, y = share*100, fill = type)) + 
  geom_col()+
  theme_bw()+
  xlab("Year") + 
  ylab("Acreage share (%)") +
  theme(legend.title=element_blank(),legend.position="bottom") +
  scale_fill_manual(values = c("purple","#009E73","#56B4E9",  "#E69F00"),labels = c("Wheat","Soybean","Rice","Corn"))+
  scale_x_continuous( breaks=seq(2000,2020,2))+
  scale_y_continuous( breaks=seq(0,100,10))

###Heilongjiang Acreage
d2021 <- data.frame(type = c("rice", "wheat", "corn","soybean"),
                    region = rep("Heilongjiang",4),
                    year = rep(2021, 4),
                    value = c(3867.4,67.3,6524.2,3887.8),
                    grain = rep(NA, 4))


#Heilongjiang
hei_acre <- read_excel("AgData/raw data/hei.xls")

#hei_acre$type <- factor(hei_acre$type, levels = c("wheat","soybean","rice","corn"))
hei_acre$year <- as.numeric(hei_acre$year)
hei_acre$value <- as.numeric(hei_acre$value)

#create other share
share_other <- hei_acre %>% group_by(year) %>% 
  summarise(other = 1-sum(share)) 

other_hei <- data.frame(type = as.factor(share_other$other),
           region = "Heilongjiang",
           year = share_other$year,
           share = share_other$other)

hei_acre_p <- rbind(hei_acre %>% select(type, region, year, share), other_hei)

#Acreage Share in Heilongjiang
hei_acre_p$type <- factor(hei_acre_p$type, levels = c("wheat","soybean","rice","corn","other"))

 ggplot(data = hei_acre_p, aes(x = year, y = share*100, group = type, color = type))+
  geom_point()+
  geom_line(size = 1.2)+
  scale_x_continuous(breaks=seq(2001,2021,1))+
   theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(x = "Year", y = "Share (%)", title = "Acreage Share in Heilongjiang")+
  scale_color_manual(name = "", values = c("purple","#009E73","#56B4E9","#E69F00","grey"),
                     labels = c("Wheat","Soybean","Rice","Corn","Other"))


 
 #share bar plot
 ggplot(data = hei_acre, aes(x = year, y = share*100, fill = type)) + 
   geom_col()+
   theme_bw()+
   xlab("Year") + 
   ylab("Acreage share (%)") +
   theme(legend.title=element_blank(),legend.position="bottom") +
   scale_fill_manual(values = c("purple","#009E73","#56B4E9","#E69F00"),labels = c("Wheat","Soybean","Rice","Corn"))+
   scale_x_continuous( breaks=seq(2001,2021,2))+
   scale_y_continuous( breaks=seq(0,100,10))

#Acreage
ggplot(data = hei_acre, aes(x = year, y = value*0.001, group = type, color = type))+
  geom_point()+
  geom_line(size = 1.2)+
  scale_x_continuous(breaks=seq(2001,2021,2))+
  labs(x = "Year", y = "Million hectare", title = "Planted Acreage in Heilongjiang")+
  scale_color_manual(name = "", values = c("#E69F00","#56B4E9","#009E73","purple"),labels = c("Corn","Rice","Soybean","Wheat"))

#others
hei_acre %>% group_by(year) %>% 
  summarise(other_share = 1-sum(share)) 




