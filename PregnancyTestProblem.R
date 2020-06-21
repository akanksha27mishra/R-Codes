install.packages("ROCR",dependencies=TRUE)
install.packages("caret",dependencies=TRUE)
library(ROCR)
library(caret)
PregnancyData<-read.csv("Pregnancy.csv")
PregnancyData.Test<-read.csv("Pregnancy_test.csv")
summary(PregnancyData)
str(PregnancyData)

#run the logistics model

Pregnancy.logit<-glm(PregnancyData$PREGNANT~.,data=PregnancyData,family=binomial("logit"))
summary(Pregnancy.logit)

#Predict based on model we have trained parameters - trained model and the test data

Pregnancy.predict<-predict(Pregnancy.logit,PregnancyData.Test)
summary(Pregnancy.predict)
Pregnancy.predict

Pregnancy.predict<-predict(Pregnancy.logit,PregnancyData.Test,type="response")
Pregnancy.predict

Pregnancy.predict[50]
PregnancyData.Test[50,]
setwd("/Users/akankshamishra/Documents/EPBA/Data Mining/R Pract")

write.csv(Pregnancy.predict,"prediction.csv")

#all values above 0.5 is 1 else 0

Pregnancy.predict.category<-ifelse(Pregnancy.predict>0.5,1,0)
Pregnancy.predict.category

## confusion Matrix ###

Preg.conf.matrix<-confusionMatrix(factor(Pregnancy.predict.category),factor(PregnancyData.Test$PREGNANT))

Preg.conf.matrix
Preg.conf.matrix$table


####ROC curve

Pregnancy.predict.glm<-prediction(Pregnancy.predict,PregnancyData.Test$PREGNANT)

#create a performance object
Pregnancy.perf.glm<-performance(Pregnancy.predict.glm,"tpr","fpr")
plot(Pregnancy.perf.glm)


