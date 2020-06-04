library(readxl)
computer<- read.csv(file.choose())
attach(computer)
View(computer)
is.na(computer)
sum(is.na(computer))
summary(computer)
plot(computer)
cor(computer)
str(computer)
table(computer$cd)
computer<-computer[-1]
library(plyr)
computer$cd <- revalue(computer$cd,c('yes'=1))
computer$cd<- revalue(computer$cd,c('2' = 0))
computer$multi <- revalue(computer$multi,c('yes'=1))
computer$multi <- revalue(computer$multi,c('no'=1))
computer$premium <- revalue(computer$premium,c('yes'=1))
computer$premium<- revalue(computer$premium,c('no'=1))
plot(computer)
cor(computer)
computer$cd = as.numeric(computer$cd)
computer$multi = as.numeric(computer$multi)
computer$premium = as.numeric(computer$premium)
cor(computer)
install.packages('car')
library(car)
#finding variation inflation factor for x variables

vifads<-lm(ads ~ speed+hd+ram+screen)
summary(vifprice)
vifspeed<- lm(speed ~ ads+hd+ram+screen)
summary(vifspeed)
vifhd<- lm(hd ~ speed+ram+ads+screen)
summary(vifhd)
vifram<- lm(ram ~ speed+hd+ads+screen)
summary(vifram)
vifscreen<- lm(screen ~ speed+ram+ads+hd)
summary(vifscreen)
#building the model by removing high weighted variable
outcome<- colnames(computer[10])
#rest are input data
variables <- colnames(computer[-10])

f <- as.formula(
  paste(outcome, 
        paste(variables, collapse = " + "), 
        sep = " ~ "))
print(f)
model1<- lm(f, data = computer)
model1
summary(model1)
#removing another variable with insignificant p value 

rmse1<- sqrt(mean(model1$residuals^2))
rmse1
vif(model1)
outcome<- colnames(computer[10])
#rest are input data
tempVar<-c(colnames(computer[-10]))
print(tempVar)
print(tempVar[-9])
variables <-(tempVar[-9])


f <- as.formula(
  paste(outcome, 
        paste(variables, collapse = " + "), 
        sep = " ~ "))
print(f)
model2<- lm(f, data = computer)
summary(model2)

rmse2<-sqrt(mean(model2$residuals^2))
rmse2

