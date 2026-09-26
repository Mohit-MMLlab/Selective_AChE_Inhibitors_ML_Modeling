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
preProcess_range_model <- preProcess(Test2[1:2134], method='range')
Test3 <- predict(preProcess_range_model, newdata = Test2[1:2134])

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
cbind (Train_Final, TrainActivity)->Train_Final


library(caret)
library(kernlab)
library(dplyr)

set.seed(100)

# train control
trctrl <- trainControl(method = "cv", number = 10, classProbs = TRUE)
GRID <- expand.grid(sigma = seq(0.1,1,by=0.1), C=seq(1,2,by=0.1))

# ---- Train original model ----
svm_Radial1 <- train(TrainActivity ~ ., data = Train_Final,
                     method = "svmRadial",
                     trControl = trctrl,
                     tuneGrid = GRID)

# ---- Evaluate on test set ----
TestFinal <- predict(preProcess_range_model, newdata = Test1[1:2134])
test_pred1 <- predict(svm_Radial1, newdata = TestFinal)

conf <- confusionMatrix(test_pred1, TestActivity, mode="everything", positive="S")

TP <- conf$table[2,2]
TN <- conf$table[1,1]
FP <- conf$table[2,1]
FN <- conf$table[1,2]

acc_orig <- (TP + TN) / (TP+TN+FP+FN)
MCC_orig <- (TP*TN - FP*FN) / sqrt((TP+FP)*(TP+FN)*(TN+FP)*(TN+FN))

# ---- Y-scrambling ----
n_iter <- 100  # number of randomizations
results <- data.frame(iter = 0:n_iter,
                      Accuracy = NA,
                      MCC = NA,
                      Type = c("Original", rep("Scrambled", n_iter)))

# store original
results$Accuracy[1] <- acc_orig
results$MCC[1] <- MCC_orig

for(i in 1:n_iter){
  set.seed(100 + i)
  
  # scramble labels
  scrambled <- Train_Final
  scrambled$TrainActivity <- sample(scrambled$TrainActivity)
  
  # retrain model on scrambled labels
  svm_scrambled <- train(TrainActivity ~ ., data = scrambled,
                         method = "svmRadial",
                         trControl = trctrl,
                         tuneGrid = GRID)
  
  # evaluate on SAME external test set
  test_pred_scr <- predict(svm_scrambled, newdata = TestFinal)
  conf_scr <- confusionMatrix(test_pred_scr, TestActivity, mode="everything", positive="S")
  
  TP <- conf_scr$table[2,2]
  TN <- conf_scr$table[1,1]
  FP <- conf_scr$table[2,1]
  FN <- conf_scr$table[1,2]
  
  acc <- (TP + TN) / (TP+TN+FP+FN)
  mcc <- (TP*TN - FP*FN) / sqrt((TP+FP)*(TP+FN)*(TN+FP)*(TN+FN))
  
  results$Accuracy[i+1] <- acc
  results$MCC[i+1] <- mcc
}

# ---- Plot Accuracy vs MCC ----
library(ggplot2)

ggplot(results, aes(x=Accuracy, y=MCC, color=Type)) +
  geom_point(size=3, alpha=0.7) +
  geom_hline(yintercept = 0, linetype="dashed", color="grey50") +
  geom_vline(xintercept = 0.5, linetype="dashed", color="grey50") +
  theme_minimal(base_size = 14) +
  labs(title="Y-Scrambling Validation (SVM Radial)",
       x="Accuracy (Test Set)",
       y="MCC (Test Set)")
