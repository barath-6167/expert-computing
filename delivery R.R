library(readxl)

delivery<- read.csv(file.choose())
attach(delivery)
summary(delivery)
View(delivery)

plot(Sorting.Time,Delivery.Time) # scatter plot
cor(Sorting.Time,Delivery.Time) # correlation coefficient 82.5

model <- lm(Delivery.Time ~ Sorting.Time) # linear regression
model
summary(model1) # output and evaluating

predict(model1)
model1$residuals
plot(model)

confint(model,level=0.95)
predict(model1, interval = "confidence")

rmse <- sqrt(mean(model1$residuals^2))
rmse

#r^squared is 68.2 and rmse is 2.7
# Log model
plot(log(Sorting.Time),Delivery.Time)
cor(log(Sorting.Time),Delivery.Time)
model1 <- lm(Delivery.Time ~ log(Sorting.Time)) # log transformation
summary(model1)
predict(model1)
model1$residuals
predict(model1, interval = 'confidence')

rmse1 <- sqrt(mean(model1$residuals^2))
rmse1
#r^squared is 69.5 and rmse is 2.73
#exp model
plot(Sorting.Time, log(Delivery.Time))
cor(Sorting.Time, log(Delivery.Time))
model2 <- lm(log(Delivery.Time) ~ Sorting.Time) # exponential tranformation
summary(model2)
model2$residuals

log_del <- predict(model2,interval="confidence")
log_del
del <- exp(log_del)
del

err <- Delivery.Time - del
err
plot(del)
rmse2 <- sqrt(mean(err^2))
rmse2
#r^squared is 71.1 and rmse is 2.9
#polynomial transformation

model3 <- lm(log(Delivery.Time) ~ Sorting.Time + I (Sorting.Time*Sorting.Time))
summary(model3)
model3$residuals
log_del1 <- predict(model3,interval="confidence")
log_del1
del1 <- exp(log_del1)
del1

err1 <- Delivery.Time - del
err1
plot(del1)
rmse3 <- sqrt(mean(err^2))
rmse3
#r^squared is 76.5 and rmse is 2.79
model4 <- lm(log(Delivery.Time) ~ Sorting.Time + I (Sorting.Time*Sorting.Time)+ I (Sorting.Time^3))
summary(model4)
model4$residuals
log_del2 <- predict(model4,interval="confidence")
log_del2
del2 <- exp(log_del2)
del2

err2 <- Delivery.Time - del
err2
plot(del2)
rmse4 <- sqrt(mean(err^2))
rmse4
#applying all the transformations model4 looks accurate with less rmse 
# R_square 76.5
#rmse 2.7


