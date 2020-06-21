install.packages("e1071",dependencies=TRUE)
library(e1071)
getwd()
data<-read.csv("sample1.csv")
traindata<-data[1:14,]
testdata<-data[15,]
traindata
table(traindata$Enrolls)
table(traindata$Enrolls,traindata$Age)

# Naive Bayes classifier

model<-naiveBayes(Enrolls~.,data=traindata)
model


#predict

results<-predict(model,testdata,type=)
results
table(results)


install.packages("rpart",dependencies=TRUE)
install.packages("rpart.plot",dependencies = TRUE)
library(rpart)
library(rpart.plot)                 

bankData<-read.csv("bank-sample.csv")
dtModel<-rpart(subscribed~ job + marital + education + default + housing + loan + contact + poutcome, 
               method="class", data=bankData,control=rpart.control(minsplit=2))
dtModel

pdf("bankPlot.pdf")
rpart.plot(dtModel)
dev.off

#predict

newData<-data.frame(job="management",marital="married",education="tertiary",
                    default="no", housing="yes", loan="no",contact="cellular",poutcome="success")
newData
table(bankData$subscribed)
table(bankData$subscribed,bankData$contact)






###### HR Problem Decision Tree ###############

employeeData<-read.csv("HR_comma_sep.csv")
employeeData
table(employeeData$left)
nrow(employeeData)
set.seed(12345)

# selecting 70 % of data randomly
index<-sample(1:nrow(employeeData),0.7*nrow(employeeData))

#get traindata
trainData<-employeeData[index,]
testData<-employeeData[-index,]

dtModel<-rpart(left~.,data=trainData,method = "class")

pdf("HRPlot.pdf")
rpart.plot(dtModel,type=4)
dev.off

HRPredict<-predict(dtModel,testData,type="class")
HRPredict


#create the confusion Matrix

confusionMatrix(factor(testData$left),factor(HRPredict))
(3382+965)/(3382+965+98+55)










