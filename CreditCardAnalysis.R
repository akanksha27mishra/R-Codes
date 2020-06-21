setwd("/Users/akankshamishra/Documents/EPBA/Data Mining/R Pract")
creditCard.data<-read.csv("creditcard.csv")
ncol(creditCard.data)
nrow(creditCard.data)
summary(creditCard.data)
sum(is.na(creditCard.data))

apply(creditCard.data,2,function(x) sum(is.na(x)))

str(creditCard.data)
creditCard.data$Class<-as.factor(creditCard.data$Class)
str(creditCard.data)

library(dplyr)
glimpse(creditCard.data) # function to see the data 

detach(package:dplyr)

table(creditCard.data$Class)

plot(creditCard.data$Class)

# class Imbalance

correlation.matrix<-cor(creditCard.data[-31],method="pearson") # correlation Matrix

correlation.matrix
#install.packages("corrplot",dependencies = TRUE)
library(corrplot)
corrplot(correlation.matrix)

fraud<-creditCard.data[creditCard.data$Class==1,]

normal<-creditCard.data[creditCard.data$class==0,]


summary(fraud$Amount)
summary(normal$Amount)

plot(fraud$Amount~fraud$Time)
plot(normal$Amount~normal$Time)

index<-sample(1:nrow(creditCard.data),round(0.7*nrow(creditCard.data),0))
train.data<-creditCard.data[index,]
test.data<-creditCard.data[-index,]

n<-names(train.data)
n

############################

eqn.form<- as.formula(paste("Class~", paste(n[!n %in% "Class"],collapse = "+")))

eqn.form

############# Naive Bayes ######################

library(e1071)

nbModel<- naiveBayes(eqn.form,train.data)

nbModel

nbPredict<- predict(nbModel, test.data, type="class")

confusionMatrix(factor(nbPredict), factor(test.data$Class))

107/(22+107)
################ Random Forest ###########

library(randomForest)
rfModel<-randomForest(eqn.form,data = train.data,ntree=50)
rfModel
rfModel$confusion
plot(rfModel)
rfPredict<-predict(rfModel,test.data,type="Class")
confusionMatrix(factor(rfPredict),factor(test.data$Class))


install.packages("DMwR",dependencies = TRUE)
library(DMwR)

table(crecreditCard.data$class)
creditCard.data.SMOTE<-SMOTE(eqn.form,creditCard.data,perc.over = 20000,perc.under = 100,k=5)
str(creditCard.data.SMOTE)

index<-sample(1:nrow(creditCard.data.SMOTE),round(0.7*nrow(creditCard.data),0))
train.data.SMOTE<-creditCard.data.SMOTE[index,]
test.data.SMOTE<-creditCard.data.SMOTE[-index,]

nbModel.SMOTE<-naiveBayes(eqn.form,train.data.SMOTE)


