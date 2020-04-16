library(data.table)
# Import and clean data-----------
dtrain <- read.csv("Train_pg17.csv", stringsAsFactors=TRUE)
dtest <- read.csv("Test_pg17.csv", stringsAsFactors=TRUE)
table(dtrain$dummy_claim)

# it's clean

# Delete useless columns
dtrain$id_client=NULL
dtrain$id_vehicle=NULL
dtrain$id_policy=NULL
dtrain$id_year=NULL
dtrain$Sum_claim_amount=NULL

dtest$id_client=NULL
dtest$id_vehicle=NULL
dtest$id_policy=NULL
dtest$id_year=NULL
dtest$Sum_claim_amount=NULL

# check variable classes
str(dtrain)
dtrain$dummy_claim <- as.factor(dtrain$dummy_claim)
dtest$dummy_claim <- as.factor(dtest$dummy_claim)

# Random Forest ----
library(randomForest)
trainsample <- dtrain[sample(nrow(dtrain), 16000), ]

start.time <- Sys.time()
rf <- randomForest(dummy_claim ~ pol_coverage+pol_duration+pol_usage+drv_age1+
                     drv_sex1+vh_age+vh_value+vh_speed, data = dtrain,
                   family = binomial,na.action=na.omit)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

# rf:
# Type of random forest: classification
# Number of trees: 500
# No. of variables tried at each split: 2
# 
# OOB estimate of  error rate: 9.35%
# Confusion matrix:
#       0     1 class.error
# 0 49989 11495   0.1869592
# 1     0 61485   0.0000000

# Predicting
pred <- predict(rf, dtest,type = "response") #type = "response" necessary?

library(caret)
confusionMatrix(pred,dtest$dummy_claim)
# Confusion Matrix and Statistics
# 
# Reference
# Prediction            0     1
#                 0 21305  2422
#                 1  4992   946
# 
# Accuracy : 0.7501         
# 95% CI : (0.7451, 0.755)
# No Information Rate : 0.8865         
# P-Value [Acc > NIR] : 1              
# 
# Kappa : 0.0683         
# 
# Mcnemar's Test P-Value : <2e-16         
#                                          
#             Sensitivity : 0.8102         
#             Specificity : 0.2809         
#          Pos Pred Value : 0.8979         
#          Neg Pred Value : 0.1593         
#              Prevalence : 0.8865         
#          Detection Rate : 0.7182         
#    Detection Prevalence : 0.7998         
#       Balanced Accuracy : 0.5455         
#                                          
#        'Positive' Class : 0  


roc(dtest$dummy_claim,pred,plot = TRUE,legacy.axes=TRUE,percent = TRUE,xlab="False Positive Percentage(1-Sensitivity)",
    ylab="True Positive Percentage(Sensitivity)",col="gold",lwd=3)
