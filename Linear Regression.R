#librarys
pacman::p_load(magrittr,ggplot2,Rmisc,dplyr,corrplot,stats,car,e1071,leaps)
#--------------------------------read data-----------------------
setwd("/Users/wangyijing/Desktop")
DIR <- "Linear Regression"
FILE <- "2017v2.csv" 
housedata <- read.csv(file.path(DIR,FILE),na.strings = c(".", "NA", "", "?"),header = TRUE)
names(housedata)
str(housedata) #43217

#-------------------------data prep---------------------------
#duplicate
sum(duplicated(housedata)) #0

#missing value
sapply(housedata, function(df){
  sum(is.na(df==TRUE))/length(df)
})
housedata <- na.omit(housedata)#42006
summary(housedata)

#wrong data: Ladder ratio
table(housedata$Ladder.ratio)
housedata <- filter(housedata,housedata$Ladder.ratio<=3)#42005

#wrong data, records with 7 or more floors and no elecator
wrongdata <- which(housedata$Floor>6 & housedata$Elevator=="no")
View(housedata[wrongdata,])
housedata <- housedata[-wrongdata,]

#wrong data, total price less than 50
housedata <- filter(housedata,housedata$Total.price>50)
str(housedata)#40160 obs.
#--------------------------variable selection--------------------------
#variable selection
housedata <- housedata[,c(9,11:18,20:23,25:26)] 
str(housedata)#40160 obs.

#-------------------------data exploration---------------------------
#correlation between numeric variables
corrplot(cor(housedata[sapply(housedata, is.numeric)]), method = "number", type='upper')
pairs(housedata[sapply(housedata, is.numeric)])

#correlation of categorical variables & target
var_names <- names(housedata[7:14][sapply(housedata[7:14], is.factor)])
gp <- lapply(var_names, function(x) {
  ggplot(housedata, aes(x = eval(parse(text = x)), y = Total.price)) + 
    geom_boxplot() +
    labs(y = x, x="", title = paste(x, "VS Total Price", sep = " ")) +
    theme_bw() +
    theme(axis.text = element_text(size = rel(1)),
          axis.text.x = element_text(size=10,angle=45,vjust = 0.5),
          axis.title = element_text(size = rel(1.1)),
          plot.title = element_text(size = rel(1.2), hjust = 0.5)) +
    guides(fill = 'none')
})
multiplot(plotlist = gp[1:6], cols = 3)

housedata %>%
  ggplot( ) + 
  geom_boxplot(aes(x = factor(District), y = Total.price,fill=factor(District),alpha=0.4)) +
  labs(x="District", y="Total.price" )


#---------------------------model building-------------------------
#split the data using 7:3 ratio
n <- nrow(housedata)
set.seed(100)
trainindex <- sample(1:n,0.7*n)
traindata <- housedata[trainindex,]
testdata <- housedata[-trainindex,]
nrow(traindata)
nrow(traindata)/n #0.7

########  first model ###########
model <- lm(Total.price~.,data=traindata)
summary(model) #Adjusted R-squared:  0.7382
par(oma=c(0,0,0,0),mfrow=c(2,2))
plot(model)

########  second model ###########
#transform the Total.price using log function
traindata <- mutate(traindata,logTotalPrice=log(traindata$Total.price))
summary(traindata$logTotalPrice)

model <- lm(logTotalPrice~.-Total.price,data=traindata)
summary(model) #Adjusted R-squared:  0.761 
par(oma=c(0,0,0,0),mfrow=c(2,2))
plot(model)


#delete the high influential points
cooksd <- cooks.distance(model)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels
influential <- as.numeric(names(cooksd)[(cooksd > 4*mean(cooksd,na.rm=TRUE))])  # influential row numbers
View(traindata[influential,])
influential <- na.omit(influential)
traindata <- traindata[-influential,]
str(traindata) #27317


########### third model ################
model <- lm(logTotalPrice~.-Total.price,data=traindata)
summary(model) #Adjusted R-squared:  0.8032
par(oma=c(0,0,0,0),mfrow=c(2,2))
plot(model)

########### fourth model ################
#drop Floor.Type, building.structure with insignificance
model <- lm(logTotalPrice~.-Total.price -Floor.Type-building.structure,data=traindata)
summary(model) #Adjusted R-squared:  0.8015
par(oma=c(0,0,0,0),mfrow=c(2,2))
plot(model)

#using all-subset regression to select the variables
model <- regsubsets(logTotalPrice~Square+Living.Room+Drawing.room+Kitchen+Bathroom+Floor+Type
                    +renovation.condition+Ladder.ratio+Elevator+Subway+District, 
                    traindata,nbest = 2, nvmax=11) 
par(oma=c(8,0,0,0),mfrow=c(1,1),cex=0.8)
plot(model,scale="adjr2")

############ Final model ################
#variable selected:Square,Living.Room,Drawing.room, Elevator, Floor, Ladder.ratio,Subway, District
FinalModel <- lm(logTotalPrice ~ Square+Living.Room+Elevator+Subway+Ladder.ratio+District,
                 data=traindata)
summary(FinalModel) #Adjusted R-squared: 0.7919
par(oma=c(0,0,0,0),mfrow=c(2,2))
plot(FinalModel)

# t-test
confint(FinalModel) 
#check for the indepedence of residuals
durbinWatsonTest(FinalModel)
#check for the normality of residuals
hist(residuals(FinalModel))
skewness(residuals(FinalModel)) #-0.1127705
#check for multi-collinearity
vif(FinalModel)


#-----------------------test Model-----------------------------
#log the Total.price
testdata <- mutate(testdata,logTotalPrice=log(testdata$Total.price)) 
lm.pred <- predict(FinalModel, testdata, interval = "prediction",level = 0.95)
testdata <- cbind(testdata,lm.pred)
str(testdata)

#calculate the adjusted R-square
SSE <- sum((testdata$logTotalPrice - testdata$fit) ^ 2)
SST <- sum((testdata$logTotalPrice - mean(testdata$logTotalPrice)) ^ 2)
1 - SSE/SST #0.7518875

#calculate the mape
mape=mean(abs(testdata$logTotalPrice-testdata$fit)/testdata$logTotalPrice)
mape # 0.02702911

