library(MASS)
library(tidyverse)
library(ggplot2)
library(readr)
heart_data <- read_csv("heart_data.csv")

str(heart_data)

  ##misssing data##
sum(is.na(heart_data))

##remaing the variable name ap_lo and ap_hi##
heart_data <- rename(heart_data, systolic_bp = ap_hi)
heart_data <- rename(heart_data, Diastolic_bp = ap_lo)

##converting age from days to years##
heart_data$age = trunc(heart_data$age/365)



####filtering the range of ap-HI AND AP_LO ##
heart_data = subset(heart_data, systolic_bp <= 200 & systolic_bp >= 90)
heart_data = subset(heart_data, Diastolic_bp <= 120 & Diastolic_bp >= 60)
view(heart_data)


###univariate Analysis for indepent variables##
par(mfrow = c(2, 4)) # to create multiple plots in one window
boxplot(heart_data$age, main = "Boxplot of Age" , xlab = "Age", col = "yellow")
hist(heart_data$age, main = "Histogram of Age", xlab = "Age", col = "lightblue")

boxplot(heart_data$weight, main = "Boxplot of weight" , xlab = "weight", col = "yellow")
hist(heart_data$weight, main = "Histogram of weight", xlab = "weight", col = "lightblue")


par(mfrow = c(2, 2))
boxplot(heart_data$systolic_bp, main = "systolic_bp" ,col = "yellow")
hist(heart_data$systolic_bp, main = "Histogram of systolic_bp", xlab = "systolic_bp", col = "lightblue")
boxplot(heart_data$Diastolic_bp, main = "Diastolic_bp", col = "yellow")
hist(heart_data$Diastolic_bp, main = "Histogram of Diastolic_bp", xlab = "Diastolic_bp", col = "lightblue")


###bivariate analysis of weigth and cholesterol level##
ggplot(heart_data, aes( x = cholesterol ,y = weight)) + geom_point()
ggplot(heart_data, aes(x = factor(cholesterol), y = weight, fill = factor(cholesterol))) +
  geom_bar(stat = "identity")
ggplot(heart_data, aes(x = factor(cholesterol), y = weight, fill = factor(cholesterol))) +
  geom_boxplot()



###bivariate analysis of weigth and active ##
ggplot(heart_data, aes( x = active ,y = weight)) + geom_point()
ggplot(heart_data, aes(x = factor(active), y = weight, fill = factor(active))) +
  geom_bar(stat = "identity")
ggplot(heart_data, aes(x = factor(active), y = weight, fill = factor(active))) +
  geom_boxplot()

###bivariate analysis of age  and cholesterol  ##
ggplot(heart_data, aes( x = cholesterol ,y = age)) + geom_point()
ggplot(heart_data, aes(x = factor(cholesterol), y = age, fill = factor(cholesterol))) +
  geom_boxplot()


##bivariate analysis of age  and cardio ###
ggplot(heart_data, aes( x = cardio ,y = age)) + geom_point()
ggplot(heart_data, aes(x = factor(cardio ), y = age, fill = factor(cardio ))) +
  geom_boxplot()


##bivariate analysis of weight and cardio ###
ggplot(heart_data, aes( x = cardio ,y = weight)) + geom_point()
ggplot(heart_data, aes(x = factor(cardio), y = weight, fill = factor(cardio))) +
  geom_boxplot()






###multivariate analysis of age =, gender and cardio##
ggplot(heart_data, aes(x = factor(cardio), y = age, fill = factor(gender))) +
  geom_boxplot()

###multivariate analysis of weight, gender and cardio##
ggplot(heart_data, aes(x = factor(cardio), y = weight, fill = factor(gender))) +
  geom_boxplot()

###multivariate analysis of age , smoke and cardio##
ggplot(heart_data, aes(x = factor(cardio), y = age, fill = factor(smoke))) +
  geom_boxplot()

###multivariate analysis of weigth, smoke and cardio##
ggplot(heart_data, aes(x = factor(cardio), y = weight, fill = factor(smoke))) +
  geom_boxplot()



###multivariate analysis of age, alcohol and cardio##
ggplot(heart_data, aes(x = factor(cardio), y = age, fill = factor(alco))) +
  geom_boxplot()

###multivariate analysis of age, alcohol and cardio##
ggplot(heart_data, aes(x = factor(cardio), y =weight, fill = factor(alco))) +
  geom_boxplot()






###multivariate analysis of weight, gender and cardio##
ggplot(heart_data, aes(x = factor(cardio), y = weight, fill = factor(gender))) +
  geom_boxplot()


###multivariate variate analysis of age, weight, cardio and blood pressure##
pairs(heart_data[, c("cardio", "systolic_bp", "Diastolic_bp","weight","age")])
ggplot(heart_data, aes(x = factor(cardio), y = Diastolic_bp, fill = systolic_bp)) +
  geom_boxplot()
ggplot(heart_data, aes(x = systolic_bp, y = Diastolic_bp, fill = cardio)) + 
  geom_boxplot() +
  xlab("Smoker") +
  ylab("Age") +
  ggtitle("Relationship between Smoker, Age, and BMI") +
  scale_fill_gradient(low = "blue", high = "red")




ggplot(df, aes(x = x, y = y, color = z)) +
  geom_point(size = 3) +
  ggtitle("Scatter Plot with Colors") +
  xlab("Variable 1") +
  ylab("Variable 2") +
  scale_color_manual(values = c("A" = "red", "B" = "blue", "C" = "green"))

###multivariate variate analysis of age and blood pressure##
pairs(heart_data[, c("age", "systolic_bp", "Diastolic_bp")])
model_4 = lm(age ~ systolic_bp + Diastolic_bp  , data = heart_data)
summary(model_4)




###Multivariate analysis of age weight and blood pressure##
pairs(heart_data[, c("weight", "systolic_bp", "Diastolic_bp", "age")])
model_5 = lm(age ~ systolic_bp + Diastolic_bp + weight , data = heart_data)
summary(model_5)






#####Multivariate analysis of response with the other independent variable ##
ggplot(heart_data, aes( x = cardio,y = weight)) + geom_boxplot()
ggplot(heart_data, aes( x = cardio,y = weight)) + geom_point()


ggplot(heart_data, aes( x = cardio,y = age)) + geom_boxplot()
ggplot(heart_data, aes( x = cardio,y = age)) + geom_point()

pairs(heart_data[, c("cardio", "systolic_bp", "Diastolic_bp")])
model_10 = lm(cardio ~ systolic_bp + Diastolic_bp, data = heart_data)
summary(model_10)


pairs(heart_data[, c("cardio", "systolic_bp", "Diastolic_bp", "age", "weight")])
model_11 = lm(cardio ~ systolic_bp + Diastolic_bp + age + weight, data = heart_data)
summary(model_11)

pairs(heart_data[, c("cardio", "systolic_bp", "Diastolic_bp", "age", "weight", "smoke", "alco")])
model_12 = lm(cardio ~ systolic_bp + Diastolic_bp + age + weight + smoke + alco , data = heart_data)
summary(model_12)


##bivariate analysis of weigth and cholesterol level and blood pressure###
ggplot(heart_data , aes()) +
  geom_histogram()

heart_data$blood_pressure = paste(heart_data$ap_hi,  heart_data$ap_lo)













