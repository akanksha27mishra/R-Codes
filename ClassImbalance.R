install.packages("ROSE")
library(ROSE)

??hacide

data(hacide)
table(hacide.test$cls)

table(hacide.train$cls)

library(rpart) # helps to create decision tree
dtree<-rpart(cls~.,data=hacide.train)
predictTree<-predict(dtree,newdata = hacide.test,type = "class")
predictTree
library(caret)
confusionMatrix(predictTree,hacide.test$cls)


roc.curve(hacide.test$cls,predictTree)

#over Sampling

# N is calculated by doing 2*980 (over) and 2*20 (under)
data_balanced_over<-ovun.sample(cls~.,data=hacide.train,method = "over", N=1960)#over under sample function

table(data_balanced_over$data$cls)

#undersampling

data_balanced_under<-ovun.sample(cls~.,data=hacide.train,method = "under", N=40)#over under sample function

table(data_balanced_under$data$cls)

#both
data_balanced_both<-ovun.sample(cls~.,data=hacide.train,method = "both",p=0.5, N=1000,seed=1234)

table(data_balanced_both$data$cls)

#synthetic generation of Data

data.rose<-ROSE(cls~.,data=hacide.train,seed=123)
table(data.rose$data$cls)

tree.rose<-rpart(cls~.,data=data.rose$data)
tree.over<-rpart(cls~.,data=data_balanced_over$data)
tree.under<-rpart(cls~.,data=data_balanced_under$data)
tree.both<-rpart(cls~.,data=data_balanced_both$data)

#predict

predictTree.rose<-predict(tree.rose,newdata = hacide.test,type = "class")
predictTree.over<-predict(tree.over,newdata = hacide.test,type = "class")
predictTree.under<-predict(tree.under,newdata = hacide.test,type = "class")
predictTree.both<-predict(tree.both,newdata = hacide.test,type = "class")


#ROSE
confusionMatrix(predictTree.rose,hacide.test$cls)
roc.curve(hacide.test$cls,predictTree.rose)

#over

confusionMatrix(predictTree.over,hacide.test$cls)
roc.curve(hacide.test$cls,predictTree.over)

#under
confusionMatrix(predictTree.under,hacide.test$cls)
roc.curve(hacide.test$cls,predictTree.under)

#Both

confusionMatrix(predictTree.both,hacide.test$cls)
roc.curve(hacide.test$cls,predictTree.both)








