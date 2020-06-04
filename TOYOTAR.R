getwd()

a <- ToyotaCorolla
View(a)
attach(a)
summary(a) # Explore the data

#deleting the unwanted columns using subset function
Toy = subset(a, select = -c(Model,Mfg_Month,Mfg_Year,Fuel_Type,Met_Color,Color,Automatic,Cylinders,Mfr_Guarantee,BOVAG_Guarantee,Guarantee_Period,ABS,Airbag_1,Airbag_2,Airco,Automatic_airco,Boardcomputer,CD_Player,Central_Lock,Powered_Windows,Power_Steering,Radio,Mistlamps,Sport_Model,Backseat_Divider,Metallic_Rim,Radio_cassette,Tow_Bar) )
Toy<- Toy[,-1]
attach(Toy)

plot(Age_08_04,Price) # Plot relation ships between each X with Y
plot()
plot(KM,Price)
plot(HP,Price)
plot(cc,Price)
plot(Doors,Price)
plot(Gears,Price)
plot(Quarterly_Tax,Price)
plot(Weight,Price)

## Or make a combined plot
pairs(Toy)   # Scatter plot for all pairs of variables

cor(Toy) # correlation matrix
#here there is no correlation problem between  indeoendent variables

# The Linear Model of interest
model.Toy <- lm(Price~Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight) # lm(Y ~ X)
summary(model.Toy)

model.cc<- lm(Price~cc)
summary(model.cc)
#individually the p value is significant for cc
model.Doors<- lm(Price~Doors)
summary(model.Doors)
#individually the p value is significant for Doors
#now trying it with both the varibales together, we get
model.cd<- lm(Price~cc+Doors)
summary(model.cd)
#this is giving us the siginificant p value for both the variables


#######                    Scatter plot matrix with Correlations inserted in graph
panel.cor <- function(x, y, digits=2, prefix="", cex.cor)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r = (cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex <- 0.4/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex)
}
pairs(Toy, upper.panel=panel.cor,main="Scatter Plot Matrix with Correlation Coefficients")

###                                   Partial Correlation matrix
install.packages("corpcor")
library(corpcor)
cor(Toy)

cor2pcor(cor(Toy))


# Diagnostic Plots
install.packages(car)
library(car)
plot(model.Toy)# Residual Plots, QQ-Plos, Std. Residuals vs Fitted, Cook's distance
qqPlot(model.Toy, id.n=5) # QQ plots of studentized residuals, helps identify outliers

# Deletion Diagnostics for identifying influential variable
influence.measures(model.Toy)
influenceIndexPlot(model.Toy, id.n=3) # Index Plots of the influence measures
influencePlot(model.Toy, id.n=3) # A user friendly representation of the above

## Regression after deleting the 81st observation
model.Toy1<-lm(Price~Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight, data=Toy[-81,])
summary(model.Toy1)
#here cc became significant after deleting the influencial value

### Variance Inflation Factors
vif(model.Toy)  # VIF is > 10 => collinearity
VIF1<-lm(Age_08_04~KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight)
VIF2<-lm(KM~Age_08_04+HP+cc+Doors+Gears+Quarterly_Tax+Weight)
VIF3<-lm(HP~Age_08_04+KM+cc+Doors+Gears+Quarterly_Tax+Weight)
VIF4<-lm(cc~Age_08_04+KM+HP+Doors+Gears+Quarterly_Tax+Weight)
VIF5<-lm(Doors~Age_08_04+KM+HP+cc+Gears+Quarterly_Tax+Weight)
VIF6<-lm(Gears~Age_08_04+KM+HP+cc+Doors+Quarterly_Tax+Weight)
VIF7<-lm(Quarterly_Tax~Age_08_04+KM+HP+cc+Doors+Gears+Weight)
VIF8<-lm(Weight~Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax)


summary(VIF1)
summary(VIF2)
summary(VIF3)
summary(VIF4)
summary(VIF5)
summary(VIF6)
summary(VIF7)
summary(VIF8)

1/1-(0.13)

#### Added Variable Plots ######
avPlots(model.Toy, id.n=2, id.cex=0.8, col="red")

library("MASS")
stepAIC(model.Toy) # backward

plot(model.Toy)
# Lower the AIC (Akaike Information Criterion) value better is the model. AIC is used only if you build
# multiple models.
model.final <- lm(Price~Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight, data=Toy)
summary(model.final)
#here doors have no effect on price so we remove doors 

model.final1 <- lm(Price~Age_08_04+KM+HP+cc+Gears+Quarterly_Tax+Weight, data=Toy[-81,])
summary(model.final1)

avPlots(model.final1, id.n=2, id.cex=0.8, col="red")

vif(model.final1)

rmse<-sqrt(mean(model.final1$residuals^2))
rmse
#applying log, sqrt and otherpolynomial transformations doesn't change the r2 value in anyway 
model.p<- lm(log(Price)~Age_08_04*Age_08_04+KM*KM+HP*HP+cc*cc+Gears*Gears+Quarterly_Tax*Quarterly_Tax+Weight*Weight, data= Toy[-81,] )
summary(model.p)
#considering the model.final1 is the good model with accuracy of 0.86

#data partitioning
n = nrows(Toy)
n1 = n*0.7
n2 = n-n1
train = sample(1:n, n1)
test = Toy[-train, ]
model.train <- lm(Price~Age_08_04+KM+HP+cc+Gears+Quarterly_Tax+Weight, train = Toy[-81,])
summary(model.train)

predict<- predict(model.train, newdata = test)
actual = test$Price
error = actual-predict

test.rmse = sqrt(mean(error**2))
test.rmse

train.rmse = sqrt(mean(model.train$residulas**2))
train.rmse





