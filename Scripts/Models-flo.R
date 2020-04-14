# Models -----
library(data.table)
ins <- fread("Data/pg17traindata.csv")
library(randomForest)

# correlation coefficients:
cor(ins$vh_cyl,ins$vh_din) # almost 72%

# keeping only significant parts of insee code 
model_set <- ins
model_set$pol_insee_code <- substr(model_set$pol_insee_code, 0, 2)


# preparing for a classification part
ins$Y <- ifelse(ins$Sum_claim_amount == 0,0,1)

# creating a model data frame with important variables
model_set <- model_set[,c(1:3,6,11,12,16,18,22,24,28,30,31:33)]

#converting strings to factor
cols <- c("pol_coverage", "pol_usage", "pol_insee_code", "drv_sex1", "vh_make")
model_set <- as.data.frame(model_set)
model_set[cols] <- lapply(model_set[cols], factor)

# splitting the model set into train and test data
# for reproducibility
set.seed(123)

smp_size <- floor(0.7 * nrow(model_set))
sample <- sample(seq_len(nrow(model_set)), size = smp_size)
model_set.train <- model_set[sample, ]
model_set.test  <- model_set[-sample, ]


# Überflüssig? -----
# looking at distributions of sex, car brand, coveraege, usage and location:
pie((sort(table(model_set$vh_make)))/length(model_set$vh_make))
pie((sort(table(model_set$pol_coverage)))/length(model_set$pol_coverage))
pie((sort(table(model_set$pol_usage)))/length(model_set$pol_usage))
pie((sort(table(model_set$drv_sex1)))/length(model_set$drv_sex1))
pie((sort(table(model_set$pol_insee_code)))/length(model_set$pol_insee_code))
# likely too many levels for brand and insee code ()

# Random Forest -----

subframe <- subset(model_set.train, select=c(-id_client, -id_vehicle, -id_policy,-CountDistinct_id_claim, -pol_insee_code, -vh_make))
subframe2 <- subframe[1:16000,]

start.time <- Sys.time()

rf1 <- randomForest(Sum_claim_amount ~., data=subframe)

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

pred <- predict(rf1, model_set.test)
# root mean squared error
sqrt(mean((pred-model_set.test$Sum_claim_amount)^2))
mean(model_set.test$Sum_claim_amount)


linear <- lm(Sum_claim_amount ~ ., data=m2)
pred <- predict(linear, model_set.test) 
sqrt(mean((pred-model_set.test$Sum_claim_amount)^2)) #983
      
summary(linear)

# only with damage
m1 <- model_set

m1 <- as.data.table(model_set)
setkey(m1,CountDistinct_id_claim)
m2 <- m1[CountDistinct_id_claim>0]
hist(log(m2$Sum_claim_amount))






