# Achtung produziert rmse von fast 1500 unter anderem, weil random forest nur mit 150
# Beobachtungen läuft. Anpassungen in Zeile 28 und 32 nötig um Modell mit allen trainings-
# daten zu trainieren.


rm(list = ls())
library(data.table)
library(randomForest)
library(caret)
# Import and adapt data-----------
dtrain <- read.csv("../../Data/Train_pg17.csv", stringsAsFactors=TRUE)
dtest <- read.csv("../Data/Test_pg17.csv", stringsAsFactors=TRUE)

cor(dtrain$vh_cyl,dtrain$vh_din) # almost 73%
model_set <- dtrain[,c(6,11,13,16,18,22,28,30,31,34)]

# dummy as factor
model_set$dummy_claim <- as.factor(model_set$dummy_claim)

# Tuned random forest-----
fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 5,
  ## repeated three times
  repeats = 2)

# sample set for calculating doubling factor
trainsample <- model_set[sample(nrow(model_set), 500), ]

start.time <- Sys.time()
rf_tuned <- train(dummy_claim ~.,
                  data = model_set,
                  method = "rf",
                  na.action=na.omit,
                  trControl = fitControl)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken 

save(rf_tuned, file = "rf_tuned2.RData")

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
confusionMatrix(pred2,as.factor(dtest$dummy_claim))
dtest2 <- as.data.table(dtest)
dtest2$pred <- pred2

# Tuned Regression ----
# on the data predicted positive in classification 
dtrain2 <- as.data.table(model_set)
dtrain2$pred <- predict(rf_tuned, dtrain2)
dtrain2$Sum_claim_amount = dtrain$Sum_claim_amount
dtrain2 = dtrain2 %>% select(-dummy_claim)

# table with only positively predicted training data
# setkey(dtrain2,pred)
# dtrain3 <- dtrain2[dtrain2$pred == 1]

# preparing the data for regression
# dtrain4 <- dtrain3[,c(6,11,16,18,22,28,30,31,33)]

# tuning regression
# fitControl <- trainControl(## 10-fold CV
#   method = "repeatedcv",
#   number = 10,
#   ## repeated three times
#   repeats = 10)

# lm_tuned <- train(Sum_claim_amount ~.,
#                   data = dtrain2,
#                   method = "lm",
#                   na.action=na.omit,
#                   trControl = fitControl)

lm_tuned4 <- lm(log(Sum_claim_amount+1) ~., data = dtrain2,)

save(lm_tuned4, file = "lm_tuned4.RData")

# table with only positively predicted testing data
setkey(dtest2,pred)
dtest3 <- dtest2[dtest2$pred == 1]

pred3 <- predict(lm_tuned4, dtest2)
rmse <- sqrt(mean((dtest2$Sum_claim_amount-exp(pred3))^2))
rmse

mae <- mean(abs(dtest2$Sum_claim_amount-exp(pred3)))
mae

sum(exp(pred3)) - sum(dtest2$Sum_claim_amount)

GeneralRevenue = (sum(exp(pred3) + 200)*1.25)
GeneralExpenditures = sum(dtest2$Sum_claim_amount)+nrow(dtest2)*200
GeneralProfit = GeneralRevenue - GeneralExpenditures

GeneralKPI = c("Revenue","Expenditures","Profit")
GeneralValue = c(GeneralRevenue, GeneralExpenditures, GeneralProfit)
GeneralFin = data.frame(GeneralKPI, GeneralValue=round(GeneralValue)/1000,0)

options(scipen=999)

GeneralFinPlot = ggplot(GeneralFin, aes(x=GeneralKPI, y=GeneralValue)) +
    geom_col(aes(fill=GeneralKPI), show.legend = FALSE) +
    geom_text(aes(label=GeneralValue), vjust=1.6, color="white", size=5.5)+
    scale_x_discrete(limits=c("Revenue", "Expenditures", "Profit")) +
    scale_fill_manual(values = c("Revenue"="black", "Expenditures"= "red", "Profit"="red")) +
    labs(x=element_blank(), y="in kEuros", title = "Revenue, Expenditures and Profit on training data") +
    theme_classic()

ggsave("Failplot.png")
