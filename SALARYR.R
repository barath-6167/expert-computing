salary<- read.csv(file.choose())
attach(salary)
summary(salary)

plot(YearsExperience,Salary) # scatter plot
cor(YearsExperience, Salary) # correlation coefficient

model <- lm(Salary ~ YearsExperience) # linear regression
model
summary(model) # output and evaluating

predict(model)
model$residuals

confint(model,level=0.95)
predict(model, interval = "confidence")

rmse <- sqrt(mean(model$residuals^2))
rmse


# Log model
plot(log(YearsExperience), Salary)
cor(log(YearsExperience), Salary)
model2 <- lm(Salary~ log(YearsExperience)) # log transformation
summary(model2)
predict(model2)
model2$residuals
predict(model2, interval = 'confidence')

rmse2 <- sqrt(mean(model2$residuals^2))
rmse2


# Exp model
plot(YearsExperience, log(Salary))
cor(YearsExperience, log(Salary))
model3 <- lm(log(Salary) ~ YearsExperience) # exponential tranformation
summary(model3)
model3$residuals

log_sal <- predict(model3,interval="confidence")
log_sal
sal <- exp(log_sal)
sal

err <- Salary - log_sal
err

rmse3 <- sqrt(mean(err^2))
rmse3


# Polynomial transformation
model4 <- lm(log(Salary) ~ YearsExperience + I(YearsExperience * YearsExperience))
summary(model4)

confint(model4,level=0.95)

log_sal1 <- predict(model4,interval="confidence")
salpoly <- exp(log_sal1)
salpoly
err_poly <- Salary - salpoly
err_poly

rmse4 <- sqrt(mean(err_poly^2))
rmse4

model5<- lm(log(Salary) ~ YearsExperience + I(YearsExperience*YearsExperience)+I(YearsExperience^3))
model5
summary(model5)
log_sal2<- predict(model5, interval = 'confidence')
log_sal2
salpoly2<-exp(log_sal2)
salpoly2
err_poly2<- Salary - salpoly2
err_poly2
rmse5<- sqrt(mean(err_poly2^2))
rmse5

