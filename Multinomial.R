# Multinomial Logit Model
# packages required

install.packages('mlogit')
install.packages('nnet')
library('mlogit')
library('nnet')

s <- read.csv(file.choose())# mdata set


#s$prog  <- relevel(s$prog, ref= "academic")  # to change the baseline level to vocation and general
s <- s[-c(1,2)]
attach(s)
table(schtyp)
library(plyr)
s <- rename(s,c('female'='gender'))
s$gender<- revalue(s$gender,c('male'=1))
s$gender<- revalue(s$gender,c('female'=0))
s$schtyp<- revalue(s$schtyp,c('public'=1))
s$schtyp<- revalue(s$schtyp,c('private'=0))

s$honors<- revalue(s$honors,c('enrolled'=1))
s$honors<- revalue(s$honors,c('not enrolled'=0))
library(dummies)

s <- cbind(s,dummy(s$ses))
s <-s[-2]

summary(s)


# Regression Model

y = s$prog
Model1 <-multinom(prog~., data = s)
summary(Model1)  # considered academic as the basline and aic value is 356.00

s$prog <- as.factor(s$prog)
s$prog <- relevel(s$prog, ref= "vocation")
?relevel

Model2 <-multinom(prog~., data = s) # vocation is considered as baseline
summary(Model2)#356.00 aic value
##### Significance of Regression Coefficients###
z <- summary(Model1)$coefficients / summary(Model1)$standard.errors
p_value <- (1-pnorm(abs(z),0,1))*2

summary(Model1)$coefficients
p_value

# odds ratio 
exp(coef(Model1))

# predict probabilities
?fitted
prob <- fitted(Model1)
prob

# Find the accuracy of the model

class(prob)
?class
prob <- data.frame(prob)
View(prob)
prob["pred"] <- NULL

# Custom function that returns the predicted value based on probability
get_names <- function(i){
  return (names(which.max(i)))
}

pred_name <- apply(prob,1,get_names)
?apply
prob$pred <- pred_name
View(prob)

# Confusion matrix
table(pred_name,s$prog)

# confusion matrix visualization
barplot(table(pred_name,s$prog),beside = T,col=c("red","blue","orange"),legend=c("academic","vocation","general"),main = "Predicted(X-axis) - Legends(Actual)",ylab ="count")
table(s$prog)

# Accuracy 
mean(pred_name==s$prog) # 62.5



# For Model2
##### Significance of Regression Coefficients###
z1 <- summary(Model2)$coefficients / summary(Model2)$standard.errors
p_value1 <- (1-pnorm(abs(z1),0,1))*2

summary(Model2)$coefficients
p_value1

# odds ratio 
exp(coef(Model2))

# predict probabilities
?fitted
prob1 <- fitted(Model2)
prob1

# Find the accuracy of the model

#class(prob)
?class
prob1 <- data.frame(prob1)
View(prob1)
prob1["pred1"] <- NULL

# Custom function that returns the predicted value based on probability
get_names <- function(i){
  return (names(which.max(i)))
}

pred_name1 <- apply(prob1,1,get_names)
?apply
prob1$pred1 <- pred_name1
View(prob1)

# Confusion matrix
table(pred_name1,s$prog)

# confusion matrix visualization
barplot(table(pred_name1,s$prog),beside = T,col=c("red","blue","orange"),legend=c("vocation","academic","general"),main = "Predicted(X-axis) - Legends(Actual)",ylab ="count")
table(S$prog)

# Accuracy 
mean(pred_name1==s$prog)#62.5 and nochange in prediction or confusion matrix

# Split the data into train and test
library(caTools)
sample.split(S,SplitRatio = 0.7)->mysplit
subset(S,mysplit==T)->train
subset(S,mysplit==F)->test
#y = train$prog
train$prog <- relevel(train$prog, ref= "academic")
Model_train <-multinom(prog~., data = train)

summary(Model_train)  # considered academic as the base

?relevel

##### Significance of Regression Coefficients###
z <- summary(Model_train)$coefficients / summary(Model_train)$standard.errors
p_value <- (1-pnorm(abs(z),0,1))*2

summary(Model_train)$coefficients
p_value

# odds ratio 
exp(coef(Model_train))

# predict probabilities
?fitted
prob <- fitted(Model_train) # Test data prediction
predict(Model_train)
prob
dim(prob)
prob1 <- predict(Model_train, newdata = test)
prob1
# Find the accuracy of the model

#class(prob)
?class
prob <- data.frame(prob)
View(prob)
prob["pred"] <- NULL

# Custom function that returns the predicted value based on probability
get_names <- function(i){
  return (names(which.max(i)))
}

pred_name <- apply(prob,1,get_names)
?apply
prob$pred <- pred_name
View(prob)

# Confusion matrix
table(pred_name,train$prog) # Train data
table(prob1,test$prog) # Test data

# confusion matrix visualization
barplot(table(pred_name,train$prog),beside = T,col=c("red","blue","orange"),legend=c("academic","general","vocation"),main = "Predicted(X-axis) - Legends(Actual)",ylab ="count")
table(train$prog)

barplot(table(prob1,test$prog),beside = T,col=c("red","blue","orange"),legend=c("academic","general","vocation"),main = "Predicted(X-axis) - Legends(Actual)",ylab ="count")
table(test$prog)
table(prob1)

# Accuracy 
mean(pred_name==train$prog) # 60.6% --Train accuracy

mean(prob1==test$prog)# 65.7%  Test accuracy


# For Model3
train$prog <- relevel(train$prog, ref= "vocation")
Model3 <-multinom(prog~., data = train) # vocation is considered as baseline
summary(Model3)
##### Significance of Regression Coefficients###
z1 <- summary(Model3)$coefficients / summary(Model3)$standard.errors
p_value1 <- (1-pnorm(abs(z1),0,1))*2

summary(Model3)$coefficients
p_value1

# odds ratio 
exp(coef(Model3))
AIC(Model3) # 241.80
# predict probabilities
?fitted
fit_M <- fitted(Model3)
prob_train <- predict(Model3)
prob_test <- predict(Model3,newdata =test)
# Find the accuracy of the model
# Confusion matrix
table(prob_train,train$prog) # train data confusion matrix

table(prob_test, test$prog)# test data confusion matrix


# confusion matrix visualization
barplot(table(prob_train,train$prog),beside = T,col=c("red","blue","orange"),legend=c("vocation","academic","general"),main = "Predicted(X-axis) - Legends(Actual)",ylab ="count")

barplot(table(prob_test,test$prog),beside = T,col=c("red","blue","orange"),legend=c("vocation","academic","general"),main = "Predicted(X-axis) - Legends(Actual)",ylab ="count")

# Accuracy 
mean(prob_train==train$prog) # 60.6% train accuracy
mean(prob_test==test$prog) # 65.73% test accuracy

#Let us remove gender as the p-values are very high
p_value1
b_v <-subset(s, select=-c(read))
library(caTools)
sample.split(b_v,SplitRatio = 0.8)->mysplit
subset(b_v,mysplit==T)->train1
subset(b_v,mysplit==F)->test1


Model4 <-multinom(prog~., data = train1)
summary(Model4)
# Read has high p-value so let us remove it and see
z3 = summary(Model4)$coefficients / summary(Model4)$standard.errors
p_value4 = (1-pnorm(abs(z3),0,1))*2
prob_test4 = predict(Model3, newdata = test1)
barplot(table(prob_test4,test1$prog),beside = T,col=c("red","blue","orange"),legend=c("vocation","academic","general"),main = "Predicted(X-axis) - Legends(Actual)",ylab ="count")
table(prob_test3,test1$prog)
mean(prob_test3==test1$prog) # 62.5% test accuracy

# Categorical values significance, correlation using chisquare, proportional tests
#takin the data set again as it is already with categorical data which helps to 
#perform chi_sqaure easily

s_new = read.csv(file.choose())

s_5 = subset(s,select= c(gender,prog))
table(s_5)
chisq.test(table(s_5))# p-high so gender is correlated

s_6 = subset(s_new,select=c(schtyp,prog))
table(s_6)
chisq.test(table(s_6)) # p-value is 0.0097 <0.05 so not required

s_7 = subset(s_new,select=c(honors,prog))
table(s_7)
chisq.test(table(s_7)) # p-value is 0.00045 < 0.05, not required

s_8 = subset(s_new,select=c(ses,prog))
table(s_8)
chisq.test(table(s_8)) # p-value is 0.0023, low so can remove this as well

#Final Model considering the significant values
colnames(s)
s_new1 = subset(s_new,select = -c(schtyp,ses,honors,X,id))

s_new1 = rename(m_new1,c('female' = 'gender'))
s_new1$ gender = revalue(s_new1$gender,c('female'= 0))
s_new1$ gender = revalue(s_new1$gender,c('male'= 1))

s_new1$prog = as.factor(s_new1$prog)

Model_f = multinom(prog~.,data = m_new1)
summary(Model_f)

Z_f = summary(Model_f)$coefficients / summary(Model_f)$standard.errors
p_valuef = (1-pnorm(abs(Z_f),0,1))*2

prob_f = predict(Model_f)
table(prob_f,s_new1$prog)
barplot(table(prob_f,s_new1$prog),beside = T,col=c("red","blue","orange"),legend=c("vocation","academic","general"),main = "Predicted(X-axis) - Legends(Actual)",ylab ="count")

mean(prob_f==s_new1$prog) #60.5% accuracy


# Observation shows General Program predictions are poor, so let us consider General as base and try
s_new1$prog = relevel(s_new1$prog,ref = "general")
Model_f2 = multinom(prog~.,data = s_new1)
summary(Model_f2)

Z_f2 = summary(Model_f2)$coefficients / summary(Model_f2)$standard.errors
p_valuef2 = (1-pnorm(abs(Z_f2),0,1))*2  # p-values are high for vocation wrt general

prob_f2 = predict(Model_f2)#No use,still general predictions are wrong
table(prob_f2,s_new1$prog)
barplot(table(prob_f2,s_new1$prog),beside = T,col=c("red","blue","orange"),legend=c("general","academic","vocation"),main = "Predicted(X-axis) - Legends(Actual)",ylab ="count")

mean(prob_f2==s_new1$prog) # 59.5% accuracy

#above all predictions show that withrespect to academic other two have less enrollments or less probability for enrolling
#considering the model with higher probability for academic whereas vocation
#general have less prbability of enrollment 

