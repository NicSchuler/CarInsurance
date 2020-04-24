
rm(list=ls())
hist(pg17upsampledData$Sum_claim_amount)
max(pg17upsampledData$Sum_claim_amount)

data <- pg17upsampledData

data$CountDistinct_id_claim=NULL
data[,1:5]=NULL

str(data)
min(data$Sum_claim_amount)
max(data$Sum_claim_amount)

data$rfPred=ifelse(data$Sum_claim_amount>0,1,0)
data$rfPred <- as.factor(data$rfPred)

linear <- lm(Sum_claim_amount~ pol_coverage+pol_duration+pol_usage+
               drv_age1+drv_sex1+drv_age_lic1+vh_age+vh_din+vh_speed+vh_value+drv_drv2+rfPred,data = data)
summary(linear)