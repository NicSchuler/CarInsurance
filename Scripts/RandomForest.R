library(data.table)
# Import and clean data-----------
dtrain <- read.csv("../Data/Train_pg17.csv", stringsAsFactors=TRUE)
dtest <- read.csv("../Data/Test_pg17.csv", stringsAsFactors=TRUE)

cor(dtrain$vh_cyl,dtrain$vh_din) # almost 73%
model_set <- dtrain[,c(6,11,12,16,18,22,28,30,31,34)]

# keeping only significant parts of insee code 
model_set$pol_insee_code <- as.factor(substr(model_set$pol_insee_code, 0, 2))
pie((sort(table(model_set$pol_insee_code)))/length(model_set$pol_insee_code))


# dummy as factor
model_set$dummy_claim <- as.factor(model_set$dummy_claim)

# Random Forest ----
library(randomForest)
trainsample <- model_set[sample(nrow(model_set), 16000), ]

start.time <- Sys.time()
rf <- randomForest(dummy_claim ~.-pol_insee_code, data = model_set,
                   family = binomial,na.action=na.omit)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken # 2 minutes


# preparing test set for prediction -- necessary?
model_set_test <- dtest[,c(6,11,12,16,18,22,28,30,31,34)]
dtest$dummy_claim <- as.factor(dtest$dummy_claim)
model_set_test$pol_insee_code <- as.factor(substr(model_set_test$pol_insee_code, 0, 2))
model_set_test$dummy_claim <- as.factor(model_set_test$dummy_claim)
# Predicting 
pred <- predict(rf, dtest,type = "response") #type = "response" necessary?

library(caret)
confusionMatrix(pred,model_set_test$dummy_claim)

# Confusion Matrix and Statistics
# 
# Reference
# Prediction     0     1
# 0 15574  1599
# 1 10723  1769
# 
# Accuracy : 0.5846         
# 95% CI : (0.579, 0.5902)
# No Information Rate : 0.8865         
# P-Value [Acc > NIR] : 1              
# 
# Kappa : 0.0539         
# 
# Mcnemar's Test P-Value : <2e-16         
#                                          
#             Sensitivity : 0.5922         
#             Specificity : 0.5252         
#          Pos Pred Value : 0.9069         
#          Neg Pred Value : 0.1416         
#              Prevalence : 0.8865         
#          Detection Rate : 0.5250         
#    Detection Prevalence : 0.5789         
#       Balanced Accuracy : 0.5587         
#                                          
#        'Positive' Class : 0

# ROC - Curve

# find important insee codes --> 84 und 32
lm <- lm(dummy_claim ~ pol_insee_code, data = trainsample)

