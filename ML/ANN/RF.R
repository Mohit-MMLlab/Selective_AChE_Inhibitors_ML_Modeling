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

##randomforest model
control <- trainControl(method="cv", number=10, search="random")
set.seed(111)
mtry <- sqrt(ncol(Train_Final))
rf_random <- train(TrainActivity ~., data= Train_Final,method="rf", tuneLength=15, trControl= control)


conf_10_fold<-confusionMatrix(rf_random, norm="none")

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
f1_s2core<-(2*((precision*sensitivity)/(precision+sensitivity)))
MCC<-(TP*TN-FP*FN)/sqrt((TP+FP)*(TP+FN)*(TN+FP)*(TN+FN))

predict(rf_random,Train_Final[1:30])->a1
confusionMatrix(a1,TrainActivity, mode='everything', positive='S')->h1
TP<-302
FP<-21
FN<-28
TN<-249
MCCtrain<-(TP*TN-FP*FN)/sqrt((TP+FP)*(TP+FN)*(TN+FP)*(TN+FN))

##testset prediction 
Testpred2 <- predict(preProcess_range_model, newdata = Test1[1:2134])

predict(rf_random,Testpred2)->a2

confusionMatrix(a2,TestActivity, mode='everything', positive='S')->h2
TP<-102
FP<-26
FN<-17
TN<-87
MCCtest<-(TP*TN-FP*FN)/sqrt((TP+FP)*(TP+FN)*(TN+FP)*(TN+FN))


library(caret)
library(kernlab)
library(pROC)
predictionwithinProbs <- predict(rf_random, newdata = Train_Final, type = "prob")

par(pty="s")
ROC_PLOT1<-roc(TrainActivity,predictionwithinProbs[,2],plot = TRUE, legacy.axes=TRUE,xlab="False positive percentage", ylab="True positive percentage",
              col="#477eb8",lwd=4,print.auc=TRUE)

predictionwithinProbs <- predict(rf_random, newdata = Test1[1:2134], type = "prob")

par(pty="s")
ROC_PLOT1<-roc(TestActivity,predictionwithinProbs[,2],plot = TRUE, legacy.axes=TRUE,xlab="False positive percentage", ylab="True positive percentage",
              col="#477eb8",lwd=4,print.auc=TRUE)

importance <- varImp(rf_random, scale=FALSE)
plot(importance, las = 4, cex.axis = 1)

