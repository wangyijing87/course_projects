library(corrplot)
library(caret)
library(reshape2)
library(stringr)
library(caTools)
library(tidyverse)
library(ROCR)
library(pROC)

#read raw datasymmetric
data <- read.csv('~/Documents/bank-additional-full-discrete.csv')

#show the prop of the subscribe
data %>%
  group_by(y) %>%
  summarise(count_level = n(),
            percentage = n()/nrow(data))%>%
  ggplot(aes(x = as.factor(y), y = count_level,fill=as.factor(y) )) + 
  geom_bar(stat='identity') +
  geom_text(aes(label=round(percentage,4)),vjust = 2)+
  labs(x="Target Value y", y="Count",fill="Target Value y")

summary(data)

#set initial seed
set.seed(123)

#split the data  (for the large volume of data, we decided to use 0.7 as splitratio )
splitData <- sample.split(data$y, SplitRatio = 0.7)
train <- data[splitData,]

#export the train data so that we can use it in JMP and SPSS to explore the data
write.csv(train,file = "~/Documents/train_data.csv")

#creat test data
test <- data[!splitData,] 
write.csv(test,file="~/Documents/test_data.csv")

#verify the splitratio
nrow(train)/nrow(data)

#transfer "unknown" to "na" for processing the data conveniently.
traindata <- read.csv('~/Documents/train_data.csv',na.strings = "unknown")

#show the structure and number of rows of traindata
str(traindata)
nrow(traindata)

#show the prop of the  subscribe in traindata
traindata %>%
  group_by(y) %>%
  summarise(count_level = n(),
            percentage = n()/nrow(traindata))%>%
  ggplot(aes(x = as.factor(y), y = count_level,fill=as.factor(y) )) + 
  geom_bar(stat='identity') +
  geom_text(aes(label=round(percentage,4)),vjust = 2) +labs(x="Target value y",y="Count",title="Proportion of y", fill="Target value y")
#show the total preformance
summary(traindata)



# remove variables from dataset, we had analyzed the variables from jmp and spss,and also remove the NO.variable that created from spliting the data
#include the variables of the default, duration, pdays 
traindata <- traindata[,-c(1,6,12,14)]
summary(traindata)

#testing and showing the prop of na of each variable
prop_na <- traindata %>%
map(~ mean(is.na(.)))  

prop_na[prop_na > 0]

# due to the low prop of na,we can remove "unknown" values from dataset
traindata <- traindata %>%
  filter(complete.cases(.))

summary(traindata)
nrow(traindata)

#see the relationship between any two numeric variables
corrplot(cor(traindata[sapply(traindata, is.numeric)]), method = "number", type='upper')


#using train data to creat our model,first we use the rest of processed variables 
model <- glm(traindata$y ~ . , data = traindata, family = binomial)
summary(model)

##first extract variables,using stepwise method due to its insignificance and the result of jmp
model <- glm(traindata$y ~ . -age -education -marital -housing -loan  -previous -euribor3m -nr.employed,
             data = traindata, family = binomial)
summary(model)

##first extract variables,using stepwise method due to its insignificance
#0.1019705  Accuracy : 0.7696     Kappa : 0.2749     Sensitivity : 0.65750         
model <- glm(traindata$y ~ . -age -job -education -marital -housing -loan -month -day_of_week -previous -euribor3m -nr.employed,
             data = traindata, family = binomial)
summary(model)

##secondly we delete one of the correlated variables(cons.price.idx)
#-cons.price.idx   0.1294294  Accuracy : 0.7805   Kappa : 0.2762   Sensitivity : 0.6229   
model <- glm(traindata$y ~ . -cons.price.idx -age -job -education -marital -housing -loan -month -day_of_week -previous -euribor3m -nr.employed,
             data = traindata, family = binomial)
summary(model)

#leave the cons.price.idx ,remove the emp.var.rate to compare,and after trail we decided to leave the emp.var.rate 
# 0.1437833  Accuracy : 0.8866  Sensitivity : 0.31701   Kappa : 0.3244 
model <- glm(traindata$y ~ . -emp.var.rate -age -job -education -marital -housing -loan -month -day_of_week -previous -euribor3m -nr.employed,
             data = traindata, family = binomial)
summary(model)

#remove the cons.conf.idx variable to see if there is a huge change
#0.1358824 Accuracy : 0.7532  Kappa : 0.2502  Sensitivity : 0.64978   ?????????cci
model <- glm(traindata$y ~ . -cons.conf.idx -cons.price.idx -age -job -education -marital -housing -loan -month -day_of_week -previous -euribor3m -nr.employed,
             data = traindata, family = binomial)
summary(model)

#after many trails we decided to remove these variables
#0.1294294    Accuracy : 0.7805   Kappa : 0.2762 Sensitivity : 0.6229
#test Accuracy : 0.7696       Kappa : 0.3068  Sensitivity : 0.6726   
model <- glm(traindata$y ~ .  -cons.price.idx -age -job -education -marital -housing -loan -month -day_of_week -previous -euribor3m -nr.employed,
             data = traindata, family = binomial)
summary(model)

#we cut the age into five groups to test if age has a significant effect on the model
traindata$ageGroup <-cut(traindata$age,c(0,20,40,60,80,100))
table(traindata$ageGroup)
#actually there is little change, so we remove the agegroup
model <- glm(traindata$y ~ .  -ageGroup -cons.price.idx -age -job -education -marital -housing -loan -month -day_of_week -previous -euribor3m -nr.employed,
             data = traindata, family = binomial)
summary(model)

#then we test the job, group the job into two groups,"yes" or "no" represent the meanings that people who have income or not
traindata$jobGroup <- traindata$job
levels(traindata$jobGroup) <- c("yes", "yes", "yes", "yes", "yes", "no", "yes", "yes", "no","yes", "no")
table(traindata$job)
summary(traindata)
#?????????
traindata$jobGroup <- traindata$job
levels(traindata$jobGroup) <- c("yes", "yes", "yes", "yes", "yes", "no", "yes", "yes", "no","yes","yes","yes")
table(traindata$jobGroup)

#  0.1211845 Accuracy : 0.7727   0.2715  0.63838 
#test   0.7602   0.2995  0.68787
model<-glm(traindata$y~ +jobGroup +poutcome +cons.conf.idx +contact +campaign +emp.var.rate,
           data=traindata,family = binomial)
summary(model)
formula(model)


#train the model
trainPredict <- predict(model, newdata = traindata, type = 'response')

#show the roc curves
auc <- colAUC(trainPredict, traindata$y, plotROC = TRUE)
legend(0.1,0.9, round(auc,4), title= 'AUC', cex=.5)
abline(a=0,b=1,lwd=2,lty=2,col="red")

# to calculate the threshold
my_roc <- roc(traindata$y, trainPredict)
coords(my_roc, "best", ret = "threshold")

#using the calculated threshold to show the confusionmatrix
p_class <- ifelse(trainPredict >0.1350837, "yes","no")
confusionMatrix(p_class, traindata$y, positive = 'yes')


#import testdata and preprocess, transfer "unknown" to "na" for processing the data conveniently.
testdata <- read.csv('~/Documents/test_data.csv',na.strings = "unknown")
summary(testdata)

testdata <- testdata %>%
  filter(complete.cases(.))
#also group the job into two groups
testdata$jobGroup <- testdata$job
levels(testdata$jobGroup) <- c("yes", "yes", "yes", "yes", "yes", "no", "yes", "yes", "no","yes", "no")
table(testdata$jobGroup)

#test the predict
testPredict = predict(model, newdata = testdata, type = 'response')
p_class2 = ifelse(testPredict >0.1211845  , "yes","no")
confusionMatrix(p_class2, testdata$y, positive = 'yes')

#show the test roc curves
auc <- colAUC(testPredict, testdata$y, plotROC = TRUE)
legend(0.1,0.9, round(auc,4), title= 'AUC', cex=.5)
abline(a=0,b=1,lwd=2,lty=2,col="red")




##############
table(traindata$pdays)
