pacman::p_load(caret,stringr,lubridate,ggplot2,corrplot,dplyr,base)
#read data and combine the two tables into one
sensordata1 <- read.csv("~/Documents/courses/assignment /5101_day8 iot sensor/assignment_data_prep/Assignment_Data.csv")
sensordata2 <- read.csv("~/Documents/courses/assignment /5101_day8 iot sensor/assignment_data_prep/Assignment_Data2.csv")

#combine the two datasets into one
sensordata <- rbind(sensordata1,sensordata2)

#see the structure of new dataset
summary(sensordata)
str(sensordata)

#show the rows of new dataset
nrow(sensordata)
write.csv(sensordata,file = "~/Documents/sensordata.csv")


#####################  Duplication Detection #########################

#explore the duplication of the whole data
sum(duplicated(sensordata))
#explore the duplication of the date_time
sum(duplicated(sensordata[,1]))
#explore the duplication of datas excluding the date_time
sum(duplicated(sensordata[,-1]))
#explore the duplication of datas excluding the date_time and unitid
sum(duplicated(sensordata[,-c(1,2)]))



############## Missing Value and Records Detection #####################

#detect the missing value of the dataset
sapply(sensordata, function(df){
+sum(is.na(df==TRUE))/length(df)
})


#detect the missing records
table(sensordata$Month,sensordata$Day)
#create four new columns
sensordata <- sensordata %>%
  mutate(Month=month(as.Date(date_time)),Day=day(date_time),Hour=hour(date_time),Minute=minute(date_time),Weekday=weekdays(as.Date(date_time)))


#indentify which day has the missing records of the two month
sensordata %>%
  group_by(Day,Month)%>%
  dplyr::summarise(count=n())%>%
  filter(count<1440)%>%
  ggplot(aes(x=as.factor(Day),y=count))+
  geom_bar(stat = "identity",fill="lightblue")+
  facet_grid(~Month)+
  labs(x="Day of Month",y="Number of Records of Each Day",title="Detect Missing Records of Each Day")+
  geom_text(aes(label=count),vjust=-0.2)+
  theme(plot.title = element_text(hjust = 0.5))


#how many missing records
60*24*(29+30)-nrow(sensordata)
#show the percentage of missing records
(60*24*(29+30)-nrow(sensordata))/nrow(sensordata)


#####################   Outliers Detection #####################
#using boxplot to find outliers,but the results is not clear
boxplot(sensordata[,c(3:8)])
#use scaled datas find the outliers
sensordata_scale <- scale(sensordata[,c(3:8)])
boxplot(sensordata_scale)
#using 3IQR to find the outliers
boxplot(sensordata_scale,range=3)


#boxplot the VOC, Co2, Noise to find what outliers should be deleted
boxplot(sensordata$VOC,col="#B0C4DE",range = 3)
boxplot(sensordata$Co2,col = "#B0C4DE",range = 3)
boxplot(sensordata$Noise,col="#B0C4DE",range = 3)


#process the outliers, transferring them into NA 
ql <- quantile(sensordata$Humidity,probs = 0.25)
qu <- quantile(sensordata$Humidity,probs = 0.75)
interql_qu=qu-ql
max <- qu+3*interql_qu
min <- ql-3*interql_qu
sensordata$Humidity[sensordata$Humidity>max |sensordata$Humidity<min] <- NA

summary(sensordata)

#show the proportion of NA 
sapply(sensordata, function(df){
  +sum(is.na(df==TRUE))/length(df)
})

#due to the low proportion of NA we decided to delete the outliers of Humidity 
sensordata <- sensordata %>% 
  filter(complete.cases(.))
summary(sensordata)



################ Other Findings ############################
#see the correlationship between variables
corrplot(cor(sensordata[,-1][sapply(sensordata[,-1], is.numeric)]), method = "number", type='upper')


#Findings:
#(1)	The sensors are placed in an office with regular working time
#(2)	There is a centralized HVAC (heating, ventilation and air conditioning) system in this space
#(3)	The timestamp is 7h15m ahead of real time

#explore the light trends in one week
sensordata %>%
  filter(Month==3,Day<=7) %>% 
  ggplot(aes(x=Hour,y=Light))+
  geom_smooth(col="#1874CD")+
  facet_grid(~Day)+
  theme(panel.grid.major.x =element_blank(), panel.grid.minor.x = element_blank())+
  labs(title="The trend of Light in One Week")+
  theme(plot.title = element_text(hjust = 0.5))


#explore noise trend in one week
sensordata%>%
  filter(Month==3,Day<=7) %>% 
  ggplot(aes(x=Hour,y=Noise))+
  geom_smooth(col="#1874CD")+
  ylim(limits=c(50,60))+
  facet_grid(~Day)+
  theme(panel.grid.major.x =element_blank(), panel.grid.minor.x = element_blank())+
  labs(title="The trend of Noise in One Week")+
  theme(plot.title = element_text(hjust = 0.5))


#adjust the time to the local time of the sensor,create a new column
time_diff <- 7*60*60+15*60
sensordata <- sensordata %>% 
  mutate(Datetime_New=(as.POSIXct(date_time)-time_diff))


#create Month,Day,Hour,Minute,Weekday columns of New Datetime
sensordata <- sensordata%>%
  mutate(Month_New=month(as.Date(Datetime_New)),Day_New=day(Datetime_New),Hour_New=hour(Datetime_New),Minute_New=minute(Datetime_New),Weekday_New=weekdays(Datetime_New))
summary(sensordata)



#we group the weekday and weekend for visualization convenience
sensordata$Weekday_Weekend <- sensordata$Weekday_New
sensordata$Weekday_Weekend[sensordata$Weekday_Weekend %in% c("Saturday","Sunday")] <- "Weekend"
sensordata$Weekday_Weekend[sensordata$Weekday_Weekend %in% c("Monday","Tuesday","Wednesday","Thursday","Friday")] <- "Weekday"
summary(sensordata)


#Findings:
# (4)	There is a curtained window in the wall

#the noise on each sunday of March
sensordata %>%
  filter(Month_New==3,Weekday_New=="Sunday") %>% 
  ggplot(aes(x=Hour_New,y=Light))+
  geom_point(col="#1874CD")+
  facet_grid(~Day_New)+
  theme(panel.grid.major.x =element_blank(), panel.grid.minor.x = element_blank())+
  labs(x="Adjusted Hour",title="The trend of Light on Weekday and Weekend")+
  theme(plot.title = element_text(hjust = 0.5))


#Findings:
#(5)	The HVAC system never completely shuts down during off-office hours
#(6)	Sensors are placed on the ceiling or on the top of the walls
#(7)	The air processing module will be automatically turned on at around 8 AM and turned off at around 6PM on weekdays, and it will stay off on weekends

# The Trend of Noise with Adjusted Hour in Weekday and Weekend
sensordata %>%
  ggplot(aes(x=as.factor(Hour_New),y=Noise))+
  geom_boxplot(col="#1874CD")+
  theme(panel.grid.major.x =element_blank(), panel.grid.minor.x = element_blank())+
  labs(x="Adjusted Hour",title="The trend of Noise")+
  theme(plot.title = element_text(hjust = 0.5))+
  ylim(limits=c(40,70))


#The Trend of VOC with Adjusted Hour in Weekday and Weekend
sensordata %>%
  ggplot(aes(x=as.factor(Hour_New),y=VOC))+
  geom_boxplot(col="#1874CD")+
  facet_grid(~Weekday_Weekend)+
  theme(panel.grid.major.x =element_blank(), panel.grid.minor.x = element_blank())+
  labs(x="Adjusted Hour",title="The trend of VOC ")+
  ylim(limits=c(300,350))+
  theme(plot.title = element_text(hjust = 0.5))


# The Trend of Temperature with Adjusted Hour in Weekday and Weekend
sensordata%>%
  ggplot(aes(x=as.factor(Hour_New),y=Temperature))+
  geom_boxplot(col="#1874CD")+
  facet_grid(~Weekday_Weekend)+
  theme(panel.grid.major.x =element_blank(), panel.grid.minor.x = element_blank())+
  labs(x="Adjusted Hour",title="The trend of Temperature ")+
  theme(plot.title = element_text(hjust = 0.5))


# The Trend of Humidity with Adjusted Hour in Weekday and Weekend
sensordata%>%
  ggplot(aes(x=as.factor(Hour_New),y=Humidity))+
  geom_boxplot(col="#1874CD")+
  facet_grid(~Weekday_Weekend)+
  theme(panel.grid.major.x =element_blank(), panel.grid.minor.x = element_blank())+
  labs(x="Adjusted Hour",title="The trend of Humidity")+
  theme(plot.title = element_text(hjust = 0.5))



#Findings:
#(8)	All of the sensors record data at the same time but only one sensor will upload data
#(9)	The sensor system backs up or downloads data on the last day of each month

#Records recorded on 1st March, 10PM (revised time)
sensordata%>%
  filter(Month_New==3, Day_New==1,Hour_New==22) %>% 
  ggplot(aes(x=as.factor(Minute_New),y=unitid,col=unitid))+
  geom_point()+
  labs(x="Adjusted Minute",y="Sensor",title="Records recorded on 1st March, 10PM (revised time)")+
  theme(plot.title = element_text(hjust = 0.5))


#Findings:
#(10)	The sensors probably calibrated measured VOC concentrations to CO2 - equivalent ppm-values

#the relationship between Co2 and VOC
sensordata%>%
  ggplot(aes(x=VOC,y=Co2))+
  geom_point()+
  labs(title="The relationship between Co2 and VOC")+
  theme(plot.title = element_text(hjust = 0.5))



#Findings:
#(11)	Power outage or network disconnection occurred a couple times during the 2 months, each lasting around 15 minutes to 1 hour

#calculate the interval minutes of every two records
sensordata <- sensordata%>%
  mutate(NextTime = lead(date_time))

sensordata <- sensordata %>% 
  mutate(Interval_Minutes=round(interval(date_time,NextTime)/dminutes(1),2))


#adjusted March 30 doesn't have missing records, the time without records due to the operation: download
sensordata %>% 
  filter(Interval_Minutes>5,Day_New!=30) %>%
  ggplot(aes(x=as.factor(Hour_New),y=Interval_Minutes))+
  geom_bar(stat = "identity",fill="#1874CD")+
  facet_grid(Month_New~Day_New)+
  labs(x="Adjusted Hour",title="The Minutes of No Records")+
  geom_text(aes(label=Interval_Minutes),vjust=-0.5)+
  theme(plot.title = element_text(hjust = 0.5))


#explore the time of Wi-Fi connection problem respectively
#on 15pm, March 10
sensordata%>%
  filter(Month_New==3,Day_New==10, Hour_New==15) %>% 
  ggplot(aes(x=as.factor(Minute_New),y=unitid))+
  geom_point(col="#1874CD")+
  facet_grid(Hour_New~Day_New)+
  labs(x="15pm, 10th March")

#on 2-3am, March 26
sensordata%>%
  filter(Month_New==3,Day_New==26, Hour_New==3 | Hour_New==2) %>% 
  ggplot(aes(x=as.factor(Minute_New),y=unitid))+
  geom_point(col="#1874CD")+
  facet_grid(Hour_New~Day_New)+
  labs(x="2-3am, 26th March")

#on 15pm, March 28
sensordata%>%
  filter(Month_New==3,Day_New==28, Hour_New==15) %>% 
  ggplot(aes(x=as.factor(Minute_New),y=unitid))+
  geom_point(col="#1874CD")+
  facet_grid(Hour_New~Day_New)+
  labs(x="15pm, 28th March")

#on 2-3am, April 22
sensordata%>%
  filter(Month_New==4,Day_New==22, Hour_New==2|Hour_New==3) %>% 
  ggplot(aes(x=as.factor(Minute_New),y=unitid))+
  geom_point(col="#1874CD")+
  facet_grid(Hour_New~Day_New)+
  labs(x="2-3am, 22th April")


#Findings:
#(12) More findings related to light

#the light trend on Adjusted March and April
sensordata%>%
  filter(Month_New!=2)%>%
  ggplot(aes(x=Hour_New,y=Light))+
  geom_smooth(col="#1874CD")+
  facet_grid(Month_New~Day_New)+
  theme(panel.grid.major.x =element_blank(), panel.grid.minor.x = element_blank())+
  labs(x="Adjusted Hour",title="The trend of Light")+
  theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_blank())

