library(data.table)
library(randomForest)
library(caret)
library(dplyr)
# Import and prepare upsampled data-----
dtrain <- read.csv("Data/Train_pg17.csv", stringsAsFactors=TRUE)
dtest <- read.csv("Data/Test_pg17.csv", stringsAsFactors=TRUE)

# choosing and adapting variables together with information from shiny interface
cor(dtrain$vh_cyl,dtrain$vh_din) # almost 73%
model_set <- dtrain[,c(6,11,13,16,18,22,28,30,31,34)]
model_set$dummy_claim <- as.factor(model_set$dummy_claim)

# Tuned random forest-----
# Training a model and saving it
fitControl <- trainControl(method = "repeatedcv", number = 5, repeats = 2)
rf_tuned <- train(dummy_claim ~., data = model_set, method = "rf", na.action=na.omit, trControl = fitControl)
save(rf_tuned, file = "Modelle/rf_tuned2.RData")

# Looking at its predictive power 
pred2 <- predict(rf_tuned, newdata =  dtest)
confusionMatrix(pred2,as.factor(dtest$dummy_claim))
dtest2 <- as.data.table(dtest)
dtest2$pred <- pred2

# Tuned Regression ----
# On the data predicted positive in classification 
dtrain2 <- as.data.table(model_set)
dtrain2$pred <- predict(rf_tuned, dtrain2)
dtrain2$Sum_claim_amount = dtrain$Sum_claim_amount
dtrain2 = dtrain2 %>% select(-dummy_claim)

lm_tuned4 <- lm(log(Sum_claim_amount+1) ~., data = dtrain2)

save(lm_tuned4, file = "Modelle/lm_tuned4.RData")

# table with only positively predicted testing data
setkey(dtest2,pred)
dtest3 <- dtest2[dtest2$pred == 1]

pred3 <- predict(lm_tuned4, dtest3)
rmse <- sqrt(mean((dtest3$Sum_claim_amount-exp(pred3))^2))
rmse



