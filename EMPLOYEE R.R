
attach(employee)
summary(employee)

plot(Salary_hike,Churn_out_rate) # scatter plot
cor(Salary_hike,Churn_out_rate) # correlation coefficient -0.91

model <- lm(Churn_out_rate ~ Salary_hike) # linear regression
model
summary(model) # output and evaluating

predict(model)
model$residuals

confint(model,level=0.95)
predict(model, interval = "confidence")

rmse <- sqrt(mean(model$residuals^2))
rmse
#r^squared is 83.1 and rmse for the model is 3.9

# Log model
plot(log(Salary_hike), Churn_out_rate)
cor(log(Salary_hike), Churn_out_rate)
model1 <- lm(Churn_out_rate ~ log(Salary_hike)) # log transformation
summary(model1)
predict(model1)

rmse1 <- sqrt(mean(model1$residuals^2))
rmse1
# r^squared is 84.9 and rmse is 3.7

# Exp model
plot(Salary_hike, log(Churn_out_rate))
cor(Salary_hike, log(Churn_out_rate))
model3 <- lm(log(Churn_out_rate) ~ Salary_hike) # exponential tranformation
summary(model3)
predict(model3)
model3$residuals

log_emp <- predict(model3,interval="confidence")
log_emp
emp <- exp(log_emp)
emp

err <- Churn_out_rate - emp
err

rmse3 <- sqrt(mean(err^2))
rmse3
#r^sqaured is 87.4 and rmse is 3.5

# Polynomial transformation
model4 <- lm(log(Churn_out_rate) ~ Salary_hike + I(Salary_hike * Salary_hike))
summary(model4)
predict(model4)
confint(model4,level=0.95)
?ggplot

log_emp1 <- predict(model4,interval="confidence")
emp_poly <- exp(log_emp1)
emp_poly

err_poly <- Churn_out_rate - emp_poly
err_poly

rmse4 <- sqrt(mean(err_poly^2))
rmse4
#hence the adjusted r squared is 0.98 and rmse is 2.01
#which says this model with 2Degree ploynomial equation fits better 
