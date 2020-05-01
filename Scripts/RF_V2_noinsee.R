rm(list = ls())
library(data.table)
library(randomForest)
library(caret)
# Import and adapt data-----------
dtrain <- read.csv("Data/Train_pg17.csv", stringsAsFactors=TRUE)
dtest <- read.csv("Data/Test_pg17.csv", stringsAsFactors=TRUE)

cor(dtrain$vh_cyl, dtrain$vh_din) # almost 73%
model_set <- dtrain[,c(6,11,14,16,18,22,28,30,31,34)]

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

saveRDS(rf_tuned, file = "Data/rf_tuned_shiny.rds")

# Looking at its predictive power 
pred <- predict(rf_tuned, newdata =  dtest)
# confusionMatrix(pred,as.factor(dtest$dummy_claim))

# prep for regression
dtest2 <- as.data.table(dtest)
dtest2$pred <- pred

# Tuned Regression ----
# on the data predicted positive in classification 
dtrain2 <- as.data.table(dtrain)
dtrain2$pred <- predict(rf_tuned, dtrain2)

# table with only positively predicted training data
setkey(dtrain2,pred)
dtrain3 <- dtrain2[dtrain2$pred == 1]

# preparing the data for regression
dtrain4 <- dtrain3[,c(6,11,16,18,22,28,30,31,33,35)]

# subsample
trainsample2 <- dtrain4[sample(nrow(dtrain4), 1000), ]

# tuning regression
fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 5,
  ## repeated three times
  repeats = 2)

lm_tuned_only_pos <- train(Sum_claim_amount ~.,
                           data = trainsample2,
                           method = "lm",
                           na.action=na.omit,
                           trControl = fitControl)
# lm2 <- lm(Sum_claim_amount ~.,data = dtrain4,na.action=na.omit)

saveRDS(lm_tuned_only_pos, file = "Data/lm_tuned_only_pos.rds")

# table with only positively predicted testing data
setkey(dtest2,pred)
dtest3 <- dtest2[dtest2$pred == 1]

pred3 <- predict(lm_tuned, dtest2)
rmse <- sqrt(mean((dtest2$Sum_claim_amount-pred3)^2))
rmse #1118
mean(dtrain$Sum_claim_amount) #551
