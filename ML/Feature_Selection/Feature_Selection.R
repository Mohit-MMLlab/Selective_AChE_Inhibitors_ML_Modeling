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


##NORMALIZATION
library(caret)
preProcess_range_model <- preProcess(Train2[1:2111], method='range')
Train3 <- predict(preProcess_range_model, newdata = Train2[1:2111])

##correlation
cor(Train3)->COR

##REMOVING HIGH CORRELATED VARIABLES
findCorrelation(COR,cutoff = 0.6,verbose = TRUE)->hcor
Train3[,-hcor]->coretrain

##FEATURE SELECTION BY RFE
set.seed(224)
subsets <- c(1,5,10,15,20,25,30,35)
ctrl <- rfeControl(functions = rfFuncs,
                   method = "cv",
                   repeats = 10,
                   verbose = FALSE)

RFE <- rfe(x=coretrain, y=TrainActivity,
           sizes = subsets,       
           rfeControl = ctrl)
predictors(RFE)

library(ggplot2)
ggplot(data = RFE, metric = "Accuracy") + theme_bw()
ggplot(data = RFE, metric = "Kappa") + theme_bw()

varimp_data <- data.frame(feature = row.names(varImp(RFE))[1:30],
                          importance = varImp(RFE)[1:30, 1])
varimp_data

library(ggplot2)

ggplot(data = varimp_data, 
       aes(x = reorder(feature, importance), y = importance, fill = importance)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(importance, 4)), 
            hjust = -0.1, color = "black", size = 4) +
  coord_flip() +
  scale_fill_viridis_c(option = "viridis") +
  labs(x = "Features", y = "Variable Importance", fill = "Importance") +
  theme_minimal(base_size = 16) +
  theme(
    axis.text.y = element_text(color = "black"),
    axis.text.x = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    legend.position = "right"
  )


