install.packages("neuralnet",dependencies = TRUE)
install.packages("MASS",dependencies = TRUE)
install.packages("ROSE",dependencies = TRUE)

library(neuralnet)
library(dplyr)
data<-Boston
str(data)
head(data)
help(data)
nrow(data)

index<-sample(1:nrow(data),0.7*nrow(data))

index

trainData<-data[index,]
testData<-data[-index,]

# use linear model to predict then we will compare the performance of the model

################Linear Model###################################
lm.fit<-glm(medv~.,data=trainData)
pr.lm<-predict(lm.fit,testData)

#MSE
MSE.lm<-sum((pr.lm-testData$medv) ^2)/nrow(testData)



#######################Neural Network Model ####################

maxs<-apply(data,2,max)
mins<-apply(data,2,min)

scaled_data<-as.data.frame(scale(data,center = mins,scale=maxs-mins))

scaled_data

trainScaledData<-scaled_data[index,]
testScaleData<-scaled_data[-index,]
testScaleData
str(testScaleData)

nmModel<-neuralnet(medv~.,data=trainScaledData, hidden = c(3,3),) # 2 hidden layers each with 3 neurons

plot(nmModel)

nmModel$net.result

pr.nn<-predict(nmModel,testScaleData[,1:13])
pr.nn

# compare Neural Model with Linear model

#descale
pr.nn.descaled<- pr.nn*(max(data$medv)-min(data$medv)) + min(data$medv)

test.descaled.Data<-testScaleData$medv * (max(data$medv)-min(data$medv)) + min(data$medv)

# calculate the MSE Mean Sqaured Error

MSE.nn<- sum((test.descaled.Data - pr.nn.descaled)^2)/nrow(testData)
MSE.lm
MSE.nn


