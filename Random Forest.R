install.packages("randomForest",dependencies = TRUE)


########################HR Problem#######################
library(randomForest)
library(neuralnet)
library(MASS)
library(ROSE)
library(rpart)
library(rpart.plot)
setwd("/Users/akankshamishra/Documents/EPBA/Data Mining/R Pract")
employeeData<-read.csv("HR_comma_sep.csv")
str(employeeData)

employeeData$left<-as.factor(employeeData$left)

table(employeeData$left)

nrow(employeeData)
colnames(employeeData)
employeeData
set.seed(12345)


index<-sample(1:nrow(employeeData),0.7*nrow(employeeData))

index

trainData<-employeeData[index,]
testData<-employeeData[-index,]

dtModel<-rpart(left~.,data=trainData,method="class")

pdf("HRPlot.pdf")
rpart.plot(dtModel, type = 4, extra = 2)
dev.off()


HRPredict<-predict(dtModel,testData,type="class")

HRPredict

library(caret)

confusionMatrix(factor(testData$left),factor(HRPredict))


################Random Forest######################
library(randomForest)
rfModel<-randomForest(left~.,data=trainData)
summary(rfModel)
confusionMatrix(rfModel$predicted,trainData$left)

#predict

predictRF<-predict(rfModel,testData)
predictRF
confusionMatrix(predictRF,testData$left)

plot(rfModel)

#important part---- changing number of trees to 200 instead of default 500   #default step factor is 3
tuneRandomForest<-tuneRF(trainData[,-7],trainData[,7],ntreeTry = 200,stepFactor = 1,
                        plot=TRUE,trace=TRUE,improve=0.01) #default step factor is 3


# reduce stepFactor to 0.5
tuneRandomForest<-tuneRF(trainData[,-7],trainData[,7],ntreeTry = 200,stepFactor = 0.5,
                        plot=TRUE,trace=TRUE,improve=0.01) #default step factor is 3


##### Revised RF after tuning

revised.rfModel<-randomForest(left~.,data=trainData,ntree=200,mtry=3)
plot(revised.rfModel)
predict.revised.RF<-predict(revised.rfModel,testData)
confusionMatrix(factor(predict.revised.RF),factor(testData$left))


#### Variable Importance

varImpPlot(revised.rfModel,
           sort=T,
           n.var = 5,
           main="Top 5 important variable")
  
importance(revised.rfModel)










