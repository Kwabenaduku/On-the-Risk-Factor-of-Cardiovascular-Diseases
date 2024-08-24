library(MASS)
library(tidyverse)
library(readr)
library(ggplot2)
library(rgl)
install.packages("rgl")
heart_data_s <- read_csv("Downloads/heart_data.csv")


##misssing data##
sum(is.na(heart_data_s))

##converting age from days to years##
heart_data_s$age = trunc(heart_data_s$age/365)

##remaing the variable name ap_lo and ap_hi##
heart_data_s <- rename(heart_data_s, systolic_bp = ap_hi)
heart_data_s <- rename(heart_data_s, Diastolic_bp = ap_lo)
heart_data_s = filter(heart_data_s, (Diastolic_bp >=60 & Diastolic_bp <= 120)  , (systolic_bp >=90 &systolic_bp <= 200) )


###UNivariate Analysis#
##gender##
ggplot(heart_data_s, aes(x = "", fill = factor(gender))) +
  geom_bar(width = 1) +
  coord_polar(theta = "y") +
  labs(fill = "Gender") +
  theme_void()



###bivariate Analyis height and weight##
ggplot(heart_data_s, aes(x = smoke, y = cardio)) +
  geom_boxplot() +
  xlab("weight (kilograms)") +
  ylab("Height (Inches)") +
  ggtitle("Relationship between Height and Weight") 



###weight and blood pressure##
plot3d(heart_data_s$systolic_bp,heart_data_s$Diastolic_bp,heart_data_s$weight, type = "s")
play3d(spin3d(axis = c(0,0,1), rpm = 10), duration = 10)


library(ggcorrplot)


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

table(train_H_set$cardio)
str(train_H_set)
##test Data ##
test_h = newdata_h[-train_h_rows , ]
test_s = newdata_h1[-train_s_rows, ]
test_H_set = rbind(test_h,test_s)

table(test_H_set$cardio)


###Logistic regression model##
logit_model = glm(cardio ~ age + gender + height +weight + systolic_bp * Diastolic_bp + cholesterol + gluc + alco + smoke + active ,data = train_H_set,family = binomial("logit"))
summary(logit_model)

###predition##
predic__H_model = predict(logit_model, test_H_set, type = "response")

# Convert the predictions into binary class predictions
predict_H <- ifelse(predic__H_model > 0.5, 1, 0)
# Create the confusion matrix
table(predict_H, test_H_set$cardio)
###checking the accuracy ##
Accuracy = mean(predict_H == test_H_set$cardio)
Accuracy
#ROC curve
library(ROSE)
par(mfrow = c(1, 1))
roc.curve(predict_H == test_H_set$cardio, predict_H)

###backward selection##
step(new_model,direction = "both") 
plot(logit_model,scale = "bic")
plot(logit_model,scale = "Cp")  
plot(logit_model,scale = "adjr2")  




new_model = glm(cardio ~ age+gender +weight + systolic_bp + Diastolic_bp +cholesterol+ gluc + alco + active + smoke ,data = train_H_set,family = binomial("logit"))
summary(new_model)

predict_new = predict(new_model, test_H_set, type = "response")
predict_newS <- ifelse(predict_new > 0.5, 1, 0)
# Create the confusion matrix
table(predict_newS, test_H_set$cardio)
###checking the accuracy ##
Accuracy = mean(predict_newS == test_H_set$cardio)
Accuracy

#####
test_H_set_one = filter(test_H_set, index == 47107 | index == 37376)




predict_new_g = predict(new_model, test_H_set_one, type = "response")
predict_new_gs <- ifelse(predict_new_g > 0.5, 1, 0)
# Create the confusion matrix
table(predict_new_gs, test_H_set_one$cardio)
Accuracy = mean(predict_new_gs == test_H_set_one$cardio)
Accuracy



