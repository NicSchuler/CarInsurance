rm(list = ls())
library(data.table)
library(randomForest)
library(caret)
# Import and adapt data-----------
dtrain <- read.csv("../Data/Train_pg17.csv", stringsAsFactors=TRUE)
dtest <- read.csv("../Data/Test_pg17.csv", stringsAsFactors=TRUE)

cor(dtrain$vh_cyl,dtrain$vh_din) # almost 73%
model_set <- dtrain[,c(6,11,16,18,22,28,30,31,34)]

# dummy as factor
model_set$dummy_claim <- as.factor(model_set$dummy_claim)

# Tuned random forest-----
fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated three times
  repeats = 2)

# sample set for calculating doubling factor
trainsample <- model_set[sample(nrow(model_set), 150), ]

start.time <- Sys.time()
rf_tuned <- train(dummy_claim ~.,
                  data = trainsample,
                  method = "rf",
                  na.action=na.omit,
                  trControl = fitControl)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken 

saveRDS(rf_tuned, file = "rf_tuned.rds")

# tuned model with 10 fold cv repeated 10 time takes 
# 17 sec for n = 100 
# 35 sec for n = 200 --> linear --> 2^17 = 131072
log2(100) # 6.64
2^10 # = 17408
(17*1024)/360
122970 / 2^6
ggplot(rf_tuned)

# Looking at its predictive power 
pred2 <- predict(rf_tuned, newdata =  dtest)
confusionMatrix(pred2,model_set_test$dummy_claim)
dtest2 <- as.data.table(dtest)
dtest2$pred <- pred2

# Tuned Regression ----
# on the data predicted positive in classification 
dtrain2 <- as.data.table(dtrain)
dtrain2$pred <- predict(rf_tuned, dtrain2)

# table with only positively predicted training data
setkey(dtrain2,pred)
dtrain3 <- dtrain2[dtrain2$pred == 1]

# preparing the data for regression
dtrain4 <- dtrain3[,c(6,11,16,18,22,28,30,31,33)]

# tuning regression
fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated three times
  repeats = 10)

lm_tuned <- train(Sum_claim_amount ~.,
                  data = dtrain4,
                  method = "lm",
                  na.action=na.omit,
                  trControl = fitControl)

save(lm_tuned, file = "lm_tuned.RData")

# table with only positively predicted testing data
setkey(dtest2,pred)
dtest3 <- dtest2[dtest2$pred == 1]

pred3 <- predict(lm_tuned, dtest3)
rmse <- sqrt(mean((dtest3$Sum_claim_amount-pred3)^2))
rmse

