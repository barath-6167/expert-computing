library(readxl)

calories<- read.csv(file.choose())
attach(calories)
summary(calories)

plot(Calories.Consumed, Weight.gained..grams.) # scatter plot
cor(Calories.Consumed,Weight.gained..grams.) # correlation coefficient 0.94

model <- lm(Weight.gained..grams.~ Calories.Consumed) # linear regression
model
summary(model) # output and evaluating

predict(model)
model$residuals

confint(model,level=0.95)
predict(model, interval = "confidence")

rmse <- sqrt(mean(model$residuals^2))
rmse
##R^2 is 0.89, rmse is 103.30 and we go further transformation

# Log model
plot(log(Calories.Consumed), Weight.gained..grams.)
cor(log(Calories.Consumed), Weight.gained..grams.)
model1 <- lm(Weight.gained..grams. ~ log(Calories.Consumed)) # log transformation
summary(model1)
predict(model1)
model1$residuals
predict(model1, interval = 'confidence')

rmse1 <- sqrt(mean(model1$residuals^2))
rmse1
#r^squared is 80.9 and rmse is 141.00
#exp model
plot(Calories.Consumed, log(Weight.gained..grams.))
cor(Calories.Consumed,log(Weight.gained..grams.))
model2 <- lm(log(Weight.gained..grams.)~ Calories.Consumed) # exponential tranformation
summary(model2)
model2$residuals

log_cal <- predict(model2,interval="confidence")
log_cal
cal <- exp(log_cal)
cal

err <- Weight.gained..grams.-cal
err

rmse2 <- sqrt(mean(err^2))
rmse2
#r^squared is 87.8 and rmse is 118.04

# Polynomial transformation
model3 <- lm(log(Weight.gained..grams.) ~ Calories.Consumed + I(Calories.Consumed * Calories.Consumed))
summary(model3)

confint(model3,level=0.95)

log_res <- predict(model3,interval="confidence")
calpoly <- exp(log_res)
calpoly
err_poly <- Weight.gained..grams. - calpoly
err_poly

rmse3 <- sqrt(mean(err_poly^2))
rmse3
#r^squared is 87.8 and rmse is 117.4 

plot(Calories.Consumed*Calories.Consumed,log(Weight.gained..grams.))
model4<-lm(log(Weight.gained..grams.) ~ Calories.Consumed + I(Calories.Consumed * Calories.Consumed) + I(Calories.Consumed^3))
summary(model4)
plot(model4)
log_res1<- predict(model4,interval = 'confidence')
cal_poly1 <- exp(log_res1)
cal_poly1
err_poly1<- Weight.gained..grams.- cal_poly1
err_poly1
rmse4<- sqrt(mean(err_poly1^2))
rmse4
# finally we got a good model with r^squared 94.2 and rmse 73.79

