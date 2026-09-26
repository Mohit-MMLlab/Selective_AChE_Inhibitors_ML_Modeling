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

library(caret)
library(kernlab)
set.seed(100)
trctrl <- trainControl(method = "cv", number = 10, classProbs = TRUE)
GRID<-expand.grid(sigma = seq(0.1,1,by=0.1),C=seq(1,2,by= 0.1))
svm_Radial1 <- train( TrainActivity ~., data= Train_Final, method = "svmRadial",trControl=trctrl,tuneGrid=GRID)

conf_10_fold<-confusionMatrix(svm_Radial1)

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

test_pred <- predict(svm_Radial1, newdata = Train_Final[1:30])
confusionMatrix(test_pred,TrainActivity, mode='everything', positive='S')->r1

TP<-306
FP<-21
FN<-24
TN<-249
MCCtrain<-(TP*TN-FP*FN)/sqrt((TP+FP)*(TP+FN)*(TN+FP)*(TN+FN))

##prediction on test set
TestFinal <- predict(preProcess_range_model, newdata = Test1[1:2134])
test_pred1 <- predict(svm_Radial1, newdata = TestFinal)
confusionMatrix(test_pred1,TestActivity, mode='everything', positive='S')->r2

TP<-108
FP<-28
FN<-11
TN<-85
MCCtest<-(TP*TN-FP*FN)/sqrt((TP+FP)*(TP+FN)*(TN+FP)*(TN+FN))

library(caret)
library(kernlab)
library(pROC)

#train set roc
predictionwithinProbs <- predict(svm_Radial1, newdata = Train_Final[1:30], type = "prob")

par(pty="s")
ROC_PLOT<-roc(TrainActivity,predictionwithinProbs[,2],plot = TRUE, legacy.axes=TRUE,xlab="False positive percentage", ylab="True positive percentage",
              col="#477eb8",lwd=4,print.auc=TRUE)


##test set roc
predictionwithinProbs <- predict(svm_Radial1, newdata = TestFinal, type = "prob")

par(pty="s")
ROC_PLOT<-roc(TestActivity,predictionwithinProbs[,2],plot = TRUE, legacy.axes=TRUE,xlab="False positive percentage", ylab="True positive percentage",
              col="#477eb8",lwd=4,print.auc=T)


## ---------- FDA Prediction ----------

# Read FDA data
fda <- read.csv("fda_ml.csv", header = TRUE)

# Get SVM features from training
svm_features <- colnames(Train_Final)[!colnames(Train_Final) %in% "TrainActivity"]

# Keep only the intersection of FDA and training features
common_features <- intersect(svm_features, colnames(fda))
mohitfda <- fda[, common_features, drop = FALSE]

# Check if any training features are missing in FDA
missing_features <- setdiff(svm_features, colnames(fda))
if(length(missing_features) > 0){
  message("⚠️ Missing features in FDA data: ", paste(missing_features, collapse = ", "))
  # Add them as all-zeros
  for(f in missing_features){
    mohitfda[[f]] <- 0
  }
}

# Reorder columns to match training
mohitfda <- mohitfda[, svm_features, drop = FALSE]

# Ensure no missing values
if(any(!complete.cases(mohitfda))){
  message("⚠️ Found rows with NA values, dropping them...")
  bad_rows <- which(!complete.cases(mohitfda))
  print(paste("Dropped rows:", paste(bad_rows, collapse = ", ")))
  fda <- fda[complete.cases(mohitfda), ]
  mohitfda <- mohitfda[complete.cases(mohitfda), ]
}

# Predict classes and probabilities
predFDA_class <- predict(svm_Radial1, newdata = mohitfda)
predFDA_prob  <- predict(svm_Radial1, newdata = mohitfda, type = "prob")

# Combine results
FDA_results <- cbind(fda, Predicted_Class = predFDA_class, predFDA_prob)

# Save results
write.csv(FDA_results, "FDA_predictions_with_probabilities.csv", row.names = FALSE)

message("✅ FDA predictions saved with ", nrow(FDA_results), " molecules.")



  


