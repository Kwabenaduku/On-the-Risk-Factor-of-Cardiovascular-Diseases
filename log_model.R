
# Importing libraries
library(caret)
library(MASS)
library(tidyverse)
library(readr)

##importing the dataset
heart_data_s = read_csv("heart_data.csv")

##misssing data##
sum(is.na(heart_data_s))

##converting age from days to years##
heart_data_s$age = trunc(heart_data_s$age/365)

##remaing the variable name ap_lo and ap_hi##
heart_data_s <- rename(heart_data_s, systolic_bp = ap_hi)
heart_data_s <- rename(heart_data_s, Diastolic_bp = ap_lo)
heart_data_s = filter(heart_data_s, (Diastolic_bp >=60 & Diastolic_bp <= 120)  , (systolic_bp >=90 &systolic_bp <= 200) )


####data preparation##
##splitting the Data in Train and Test##
newdata_h = heart_data_s[which(heart_data_s$cardio == 1), ]
newdata_h1 = heart_data_s[which(heart_data_s$cardio == 0), ]
sum(is.na(heart_data_s))


##train Data ##
set.seed(321)
train_h_rows = sample(1:nrow(newdata_h), 0.5*nrow(newdata_h))
train_s_rows = sample(1:nrow(newdata_h1), 0.5*nrow(newdata_h1))
train_h = newdata_h[train_h_rows, ]
train_s = newdata_h1[train_s_rows, ]
train_H_set = rbind(train_h,train_s)


##test Data ##
test_h = newdata_h[-train_h_rows , ]
test_s = newdata_h1[-train_s_rows, ]
test_H_set = rbind(test_h,test_s)

write.csv(train_H_set, "training.csv")
write.csv(test_H_set, "testing.csv")

###Logistic regression model##
logit_model = glm(cardio ~ age + gender + height +weight + systolic_bp * Diastolic_bp + cholesterol + gluc + alco + smoke + active ,data = train_H_set,family = binomial("logit"))


# Save model to RDS file
saveRDS(logit_model, "log_model.rds")
