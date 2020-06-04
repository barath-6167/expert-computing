getwd()

start <- read.csv (file.choose())
View(start)

attach(start)
#converting categorical data to factor
startup<- as.factor(start$State)
#converting the state column into dummy
install.packages("dummies")
library("dummies")
start1<- cbind(start, dummy(start$State))
start1<- start1[,-4]
names(start1)[1]<- "RDSpend"
names(start1)[3]<- "marketing"
names(start1)[7]<- "state_ny"

#normal qq plots for all the variables 
qqnorm(Administration)
qqline(Administration)
qqnorm(R.D.Spend)
qqline(R.D.Spend)
qqnorm(Marketing.Spend)
qqline(Marketing.Spend)
qqnorm(Profit)
qqline(Profit)
attach(start1)
summary(start1) # Explore the data


plot(Administration,Profit) # Plot relation ships between each X with Y
plot(RDSpend,Profit)
plot(marketing,Profit)

## Or make a combined plot
pairs(start1)   # Scatter plot for all pairs of variables

cor(start1) # correlation matrix

# The Linear Model of interest
colnames(start1)
model.start1 <- lm(Profit~RDSpend+Administration+marketing+startCalifornia+startFlorida+state_ny, data = start1) # lm(Y ~ X)
summary(model.start1)#0.95
#linear model for each independent variable
model.s1<-lm(Profit~Administration)
summary(model.s1)
#independently it is insignificant too 

model.s2 <- lm(Profit ~ marketing, data = start1 )
summary(model.s2)
#independently marketing is significant

model.s3 <- lm(Profit~Administration+marketing, data = start1)
summary(model.s3)
#they are significant to each other

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
pairs(start1, upper.panel=panel.cor,main="Scatter Plot Matrix with Correlation Coefficients")

###                                   Partial Correlation matrix
install.packages("corpcor")
library(corpcor)
cor(start1)

cor2pcor(cor(start1))


# Diagnostic Plots
install.packages(car)
library(car)
plot(model.start1)# Residual Plots, QQ-Plos, Std. Residuals vs Fitted, Cook's distance
qqPlot(model.start1, id.n=5) # QQ plots of studentized residuals, helps identify outliers

# Deletion Diagnostics for identifying influential variable
influence.measures(model.start1)
influenceIndexPlot(model.start1, id.n=3) # Index Plots of the influence measures
influencePlot(model.start1, id.n=3) # A user friendly representation of the above

## Regression after deleting the 50th observation
model.s4<-lm(Profit~RDSpend+Administration+marketing+startCalifornia+startFlorida+state_ny,data = start1[-50,])
summary(model.s4)# 0.96
##Regression after deleting the 49th observation
model.s5<-lm(Profit~RDSpend+Administration+marketing+startCalifornia+startFlorida+state_ny,data = start1[-49,])
summary(model.s5) #0.95

model.s6<- lm(Profit~RDSpend+Administration+marketing, data = start1)
summary(model.s6)#0.95

#transformations on insignificant varibales
cor(Profit, Administration*Administration)
cor(Profit, log(Administration))
cor(Profit, Administration*Administration*Administration)
cor(Profit, sqrt(Administration))
cor(log(Profit), Administration)
cor(Profit, marketing*marketing)
cor(Profit, sqrt(marketing))
cor(Profit, marketing*marketing*marketing)
cor(log(Profit), marketing*marketing)
cor(sqrt(Profit), marketing)
#deleting administration from the data set
model.s7<- lm(Profit~RDSpend+marketing)
summary(model.s7)#0.95
pred<- predict(model.s7, interval = 'confidence')
pred
err<- Profit-pred
err
rmse<- sqrt(mean(err)^2)
rmse #2.449513e-11

model.s8<- lm(Profit~RDSpend+marketing+ I(marketing*marketing))
summary(model.s8)#0.95
pred1<- predict(model.s8, interval = 'confidence')
pred1
err1<- Profit-pred
err1
rmse1<- sqrt(mean(err1)^2)
rmse1 #3.769053e-11

### Variance Inflation Factors
vif(model.s7)  # VIF is > 10 => collinearity
VIF1<-lm(RDSpend~Administration+marketing)
VIF2<-lm(Administration~RDSpend+marketing)
VIF3<-lm(marketing~Administration+RDSpend)
summary(VIF1)
summary(VIF2)
summary(VIF3)


#### Added Variable Plots ######
avPlots(model.start1, id.n=2, id.cex=0.8, col="red")
avPlots(model.s7, id.n=2, id.cex=0.8, col="red")
library("MASS")
stepAIC(model.start1) # backward
stepAIC(model.s7) #915.18
plot(model.s7)

#considering model.s7 is the best model with good accuracy
