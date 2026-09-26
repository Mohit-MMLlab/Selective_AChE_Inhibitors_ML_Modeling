##READ DATA
read.csv("Train.csv",header = TRUE)->Train

Train$Class->Class_Train
as.factor(Class_Train)->Class_Train
is.factor(Class_Train)
Train[,-1]->Train
Train[,-2135]->Train1

Train1[,-which(sapply(Train1, var)==0)]->Train2
cbind(Train2,Class_Train)->Train2

Train2$Class_Train->TrainActivity

read.csv("Test.csv",header = TRUE)->Test
Test$Class->Class_Test
as.factor(Class_Test)->Class_Test
is.factor(Class_Test)
Test[,-1]->Test
Test[,-2135]->Test1
cbind(Test1,Class_Test)->Test2
Test2$Class_Test->TestActivity

library(caret)
preProcess_range_model <- preProcess(Test2[1:2124], method='range')
Test3 <- predict(preProcess_range_model, newdata = Test2[1:2124])

##NORMALIZATION
library(caret)
preProcess_range_model <- preProcess(Train2[1:2111], method='range')
Train3 <- predict(preProcess_range_model, newdata = Train2[1:2111])

##correlation
cor(Train3)->COR

##REMOVING HIGH CORRELATED VARIABLES
findCorrelation(COR,cutoff = 0.6,verbose = TRUE)->hcor
Train3[,-hcor]->coretrain

Train_Final<-coretrain[,c("ExtFP673","ExtFP773","ExtFP853","ExtFP246","ExtFP486","ExtFP766","ExtFP461","ExtFP511","ExtFP619","ExtFP765","ExtFP492","ExtFP597","ExtFP658","PubchemFP638","ExtFP630","ExtFP315","ExtFP812","ExtFP510","ExtFP922","ExtFP163","ExtFP241","PubchemFP13","ExtFP65","ExtFP248","ExtFP602","ExtFP701","ExtFP279","ExtFP949","KRFP2113","ExtFP531")]

cbind(Train_Final, TrainActivity)->Train_Final


##KNN MODEL 
library(caret)
grid_search<-expand.grid(k = seq(1, 15, by = 2))
trctrl <- trainControl(method = "cv", number = 10)
set.seed(100)
kmodel <-train(TrainActivity ~., data= Train_Final,method="knn",metric="Accuracy",trControl=trctrl,tuneGrid=grid_search)
conf_10_fold<-confusionMatrix(kmodel, norm="none")

abcd<-as.array.default(conf_10_fold)

conf<-as.data.frame.table(abcd)

TP<-conf[1,4]
FN<-conf[2,4]
FP<-conf[3,4]
TN<-conf[4,4]

precision<-TP/(TP+FP)
sensitivity<-TP/(TP+FN)
specificity<-TN/(TN+FP)
accuracy<-(TP+TN)/(TP+TN+FP+FN)
f1_score<-(2*((precision*sensitivity)/(precision+sensitivity)))
MCC<-(TP*TN-FP*FN)/sqrt((TP+FP)*(TP+FN)*(TN+FP)*(TN+FN))

predict(kmodel,Train_Final[1:30])->f2
confusionMatrix(f2,TrainActivity,mode='everything', positive='S')->x2

TP<-297
FP<-43
FN<-33
TN<-227
MCCtrain<-(TP*TN-FP*FN)/sqrt((TP+FP)*(TP+FN)*(TN+FP)*(TN+FN))

##EXTERNAL VALIDATION
predict(kmodel,Test1[1:2134])->f2
confusionMatrix(f2,TestActivity, mode='everything', positive='S')->x2

TP<-108
FP<-34
FN<-11
TN<-79
MCCtest<-(TP*TN-FP*FN)/sqrt((TP+FP)*(TP+FN)*(TN+FP)*(TN+FN))

library(caret)
library(kernlab)
library(pROC)
predictionwithinProbs <- predict(kmodel, newdata = Train_Final, type = "prob")

par(pty="s")
ROC_PLOT1<-roc(TrainActivity,predictionwithinProbs[,2],plot = TRUE, legacy.axes=TRUE,xlab="False positive percentage", ylab="True positive percentage",
              col="#477eb8",lwd=4,print.auc=TRUE)

predictionwithinProbs <- predict(kmodel, newdata = Test1[1:2134], type = "prob")

par(pty="s")
ROC_PLOT1<-roc(TestActivity,predictionwithinProbs[,2],plot = TRUE, legacy.axes=TRUE,xlab="False positive percentage", ylab="True positive percentage",
              col="#477eb8",lwd=4,print.auc=TRUE)


