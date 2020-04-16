
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


logmod <- glm(dummy_claim ~ .,data = dtrain,
              family=binomial(link="logit"),maxit=100)

prediction <- predict.glm(logmod,newdata=dtest,type = "response")

memory.size(max = FALSE)


