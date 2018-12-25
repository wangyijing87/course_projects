library("pacman")
pacman::p_load(tidyverse, dplyr,TTR,corrplot)


###########      Data Preparation     ################
bikedata <- read.csv("/Users/wangyijing/Documents/courses/5102_Data_analytics/day4/day.csv")
names(bikedata)
summary(bikedata)
str(bikedata)

#plot the graph of target variable "cnt"
ts <- ts(bikedata$cnt,frequency=365,start=c(2000,1,1)) 
plot(ts,ylab = "Count of Bike", main = "Change of Demand of Bike with Time")

bikedata <- bikedata[,-1] %>% 
  mutate(dteday = as.Date(dteday, "%Y-%m-%d"), season = as.factor(season), yr = as.factor(yr), 
         mnth = as.factor(mnth), holiday = as.factor(holiday), weekday = as.factor(weekday),
         workingday = as.factor(workingday), weathersit = as.factor(weathersit))

#checcking duplcated data
sum(duplicated(bikedata))

#checking missing value
sapply(bikedata[,1:15],function(df){
  +sum(is.na(df==TRUE))/length(df)
})

#check missing records 
count(bikedata)
bikedata %>%
  group_by(mnth,yr) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(x=as.factor(mnth),y=count))+
  geom_bar(stat = "identity",fill="#678f8d")+
  facet_grid(~yr)+
  labs(x="Month", y="Number of Records", title="Checking Missing Records of 2010 and 2011")+
  geom_text(aes(label=count),vjust=-0.2)


#####outliers ---tbd
Temperature <- ts(bikedata$atemp, frequency=365, start= c(2000,1,1))
plot(Temperature)
tem <- ts(bikedata$temp, frequency=365, start= c(2000,1,1))
hum <- ts(bikedata$hum, frequency=365, start= c(2000,1,1))
wind <- ts(bikedata$windspeed,frequency=365, start=c(2000,1,1))
plot(tem)

#checking correlation
corrplot(cor(bikedata[sapply(bikedata, is.numeric)]), method = "number", type='upper')

#create new variable pre_cnt_2 & pre_cnt_7
bikedata <- bikedata %>%
  mutate(pre_cnt_2 = lag(cnt, n=2), pre_cnt_7 = lag(cnt, n=8))


#create trend variable
bikedata$Trend <- as.integer(SMA(bikedata$cnt, n=7))
bikedata <- bikedata %>% 
  mutate(Trend = lag(Trend, n=2))


########   Simple Method(Benchmark)    ########
#bikedata = bikedata%>%
#mutate(revenue = ifelse(cnt>pre_cnt,pre_cnt,cnt)*3)
bikedata <- bikedata %>%
  mutate(profit_2 = (ifelse(cnt>pre_cnt_2,pre_cnt_2,cnt)*3 - pre_cnt_2*2),
         profit_7 = (ifelse(cnt>pre_cnt_7,pre_cnt_7,cnt)*3 - pre_cnt_7*2))

bikedata <- bikedata %>% 
  na.omit()

#detect outliers of time series


#check stationary -> non-stationary
acf(ts)
#remove seasonality
acf(diff(ts,lag=1))

# library(tseries)
# adf.test(diff(ts),alternative='stationary',k=0)
# ma = auto.arima(traindata$cnt)
# plot(forecast(ma,h=12))



#create a trend variable


decom = stl(ts,s.window = "periodic")
sts = decom$time.series
plot(sts[,"trend"])
trend =as.vector(as.numeric(sts[,"trend"]))

bikedata = bikedata %>% 
  mutate(Trend = trend)


#single model?????????profit, ??????ensemble model??????profit

#since its time series data, we split the data first, then do data preparation
traindata <- bikedata[which(bikedata$yr==0),]
testdata <- bikedata[which(bikedata$yr==1),]



#error
rmse(pred_rwf$mean,test)#92.66636
