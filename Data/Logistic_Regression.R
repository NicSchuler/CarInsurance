
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
# logistic regression model


logmod <- glm(dummy_claim ~ pol_coverage+pol_duration+pol_usage+drv_age1+
                drv_sex1+vh_age+vh_value+vh_speed,data = dtrain,
              family=binomial(link="logit"),maxit=100)

# Confusion Matrix
prediction <- predict.glm(logmod,newdata=dtest,type = "response")
table(prediction>0.5,dtest$dummy_claim)

# ROC Curve
roc(dtest$dummy_claim,prediction,plot = TRUE,legacy.axes=TRUE,percent = TRUE,xlab="False Positive Percentage(1-Sensitivity)",
    ylab="True Positive Percentage(Sensitivity)",col="gold",lwd=3)

