library(corrplot)
library(psych)
library(caTools)
library(factoextra)
library(stringr)
library(dplyr)
library(ggplot2)
library(nnet)
library(data.table)
library(cluster)
#read the red-wine data
winedata <- read.csv("~/Documents/balabala/winequality-red.csv")

#show the stucture and the range of values of each variables
summary(winedata)
str(winedata)
#see if there are duplications of data
sum(duplicated(winedata))

#see if there are highly correlated varialbes
corrplot(cor(winedata[sapply(winedata, is.numeric)]), method = "number", type='upper')

#to see if there is na
sapply(winedata, function(df){
  +sum(is.na(df==TRUE))/length(df)
})

#show the distribution of the quality
winedata%>%
  group_by(quality)%>%
  summarise(count=n())%>%
  ggplot(aes(x=quality,y=count))+
  geom_bar(stat = "identity",fill="#759fbd")+
  labs(x="Quality",y="Frequency")

nrow(winedata)

# to see if there are outliers
plot(winedata[,-12])
boxplot(winedata[,-12],cex.axis=0.7)
boxdata <- scale(winedata[,-12])
#the value range of total.sulfur.dioxide is so large, so we use the scaled data to see the outliers
boxplot(boxdata[,-12],cex.axis=0.7)

#process the outliers, transferring them into NA 
f_outlier <- function(x,na.rm=TRUE){
  ql <- quantile(x,probs = 0.25)
  qu <- quantile(x,probs = 0.75)
  interql_qu=qu-ql
  max <- qu+1.5*interql_qu
  min <- ql-1.5*interql_qu
  x[x>max |x<min] <- NA
  x
}

data <- apply(winedata[,-12], 2, f_outlier)
data <- data.frame(data)
nrow(data)
#show the proportion of NA of each variable
sapply(data, function(df){
  +sum(is.na(df==TRUE))/length(df)
})

#deleting the NA and combine the data and quality variable into a new dataset
winedata <- data%>%
  cbind(quality=winedata[,12])%>%
  na.omit()

nrow(winedata)
View(winedata)

#show the distribution of quality after processing the data
winedata%>%
  group_by(quality)%>%
  summarise(count=n())%>%
  ggplot(aes(x=quality,y=count))+
  geom_bar(stat = "identity",fill="#759FBD")+
  labs(x="Quality",y="Frequency")

#export the processed data which will be used in another tools
write.csv(winedata,file = "~/Documents/redwine_newdata.csv")


#*********  PCA  ********************

#show the scree plot to see how many components should be reserved according to the eigen>1
fa.parallel(winedata[,-12],fa="pc",n.iter = 100,show.legend = FALSE,main ="scree plot")
#use another function prcomp to show the value of eigen and variance
winedata.pr <- prcomp(winedata[,-12],scale=TRUE,center = TRUE)
summary(winedata.pr)
winedata.pr.eigen <- get_eigenvalue(winedata.pr)
winedata.pr.eigen

#show the variance can be explained by each components
fviz_eig(winedata.pr,addlabels = TRUE,xlab="Components",ylim=c(0,50))
#show the cumulative variance can be explained by components
barchart(winedata.pr.eigen$cumulative.variance.percent[1:6],col="#709aBD",names.arg=c("1","2","3","4","5","6"),
         xlab="Number of Components",ylab="Explained Cumulative Variance Proportion")

#calculate the components
pc <- principal(winedata[,-12],nfactors = 4,rotate="none",scores = TRUE)
pc$loadings

#using varimax rotation to rotate the result, which can help us name the components
rc <- principal(winedata[,-12],nfactors = 4,rotate = "varimax",score=TRUE)
rc$loadings

#calculate the component scores of each component 
for (i in 1:4) {
  pc$scores[,i]=pc$scores[,i]*sqrt(pc$values[i])
}
pc$scores=data.frame(pc$scores)

#profile the 4 components
old_names=c("PC1","PC2","PC3","PC4")
new_names=c("Acidity","Fermentation.Products","Sulfur.Dioxide","Flavour")
setnames(pc$scores,old=old_names,new=new_names)
View(pc$scores)

#combine the 4 components with raw data
winedata <- winedata%>%
  cbind(pc$scores)
View(winedata)


#****************  K-means Cluste  r*****************

#split the newdata includes 4 components using stratified sampling
newdata <- winedata[,13:16]

set.seed(123)
#randomly split the cluster into two dataset(50:50)
n=nrow(newdata)
trainindex <- sample(1:n,0.5*n)
dep_data <- newdata[trainindex,]
val_data <- newdata[-trainindex,]
nrow(dep_data)

#export the development and validation datasets so that we can use it in JMP to explore the data
write.csv(dep_data,file = "~/Documents/dep_data.csv")
write.csv(val_data,file="~/Documents/val_data.csv")


#using the k-means clutering method and compare the centriods or profiles of each subset with raw dataset
#The simplest method is to look at the within groups sum of squares
wss <- (nrow(newdata)-1)*sum(apply(newdata,2,var))
for (i in 2:6)
  wss[i] <- sum(kmeans(newdata,centers=i)$withness)
plot(1:6, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")

#show the centers of each cluster of the raw dataset
winedata.clustraw <- kmeans(newdata,2,nstart=25)
winedata.clustraw
winedata.clustraw$size
winedata.clustraw$centers

clusplot(newdata, winedata.clustraw$cluster, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)

#using the same clutering method and compare the centriods or profiles of each dataset
wss <- (nrow(dep_data)-1)*sum(apply(dep_data,2,var))
for (i in 2:6)
  wss[i] <- sum(kmeans(dep_data,centers=i)$withness)

plot(1:6, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")

#using silhouette to identify the number of clusters
fviz_nbclust(newdata, kmeans, method = "silhouette")

#using kmeans clustering method, the number of centers is 2
winedata.clust <- kmeans(dep_data,2,nstart=25)

winedata.clust
winedata.clust$size
winedata.clust$centers
clusplot(dep_data, winedata.clust$cluster, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)



#using the same clutering method and compare the centriods or profiles of each dataset
wss <- (nrow(val_data)-1)*sum(apply(val_data,2,var))
for (i in 2:6)
  wss[i] <- sum(kmeans(val_data,centers=i)$withness)
plot(1:6, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")

#show the centers of each cluster of validation result
winedata.clust2 <- kmeans(val_data,2,nstart=25)
winedata.clust2
winedata.clust2$size
winedata.clust2$centers

clusplot(val_data, winedata.clust2$cluster, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)


#**************   Multi-category Logistic Regression   *********************

#classify the quality into three groups
winedata$quality[winedata$quality==3|winedata$quality==4]="Low"
winedata$quality[winedata$quality==5|winedata$quality==6]="Medium"
winedata$quality[winedata$quality==7|winedata$quality==8]="High"

#using multinomal regression
mult.wine <- multinom(winedata$quality~winedata$Acidity+winedata$Fermentation.Products+winedata$Sulfur.Dioxide+winedata$Flavour,data=winedata)
summary(mult.wine)

#step and step train the model, so we find all four variables are important
step.wine <- step(mult.wine)

#predict the model
wine.pred <- predict(step.wine) 
summary(wine.pred)
#show the confusion matrix
confusionMatrix(wine.pred,winedata$quality)
