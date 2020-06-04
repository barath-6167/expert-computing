# -*- coding: utf-8 -*-
"""
Created on Thu May  7 06:48:57 2020

@author: barath goud
"""


import pandas as pd
import numpy as np

corolla = pd.read_csv("C:/Users/User/Desktop/DATA SETS $ CODESR/ToyotaCorolla.csv")
corolla = ToyotaCorollacsv.copy()
corolla.head()
corolla.describe()

Toyota = ToyotaCorollacsv[["Price","Age_08_04","KM","HP","cc","Doors","Gears","Quarterly_Tax","Weight"]]
#LINE Assumptions( Linear,Independent, Normal,Equal Variance)
#Lets do scatter plot
import seaborn as sns
sns.pairplot(Toyota.iloc[:,:])
import statsmodels.api as sm
Toyota.corr()
import statsmodels.formula.api as smf # for regression model

# Scatter plot between the variables along with histograms
import seaborn as sns
sns.pairplot(Toyota.iloc[:,:])
                             
# Correlation matrix 
Toyota.corr()

# preparing model considering all the variables 
import statsmodels.formula.api as smf # for regression model
         
ml1 = smf.ols('Price~Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight',data=Toyota).fit() # regression model

# Summary
ml1.summary() #0.86

# calculating VIF's values of independent variables
rsq_hp = smf.ols('Age_08_04~KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight',data=Toyota).fit().rsquared  
vif_hp = 1/(1-rsq_hp) #1.88 

rsq_wt = smf.ols('KM~Age_08_04+HP+cc+Doors+Gears+Quarterly_Tax+Weight',data=Toyota).fit().rsquared  
vif_wt = 1/(1-rsq_wt)#1.74

rsq_vol = smf.ols('HP~Age_08_04+KM+cc+Doors+Gears+Quarterly_Tax+Weight',data=Toyota).fit().rsquared  
vif_vol = 1/(1-rsq_vol) #1.41

rsq_sp = smf.ols('cc~KM+Age_08_04+HP+Doors+Gears+Quarterly_Tax+Weight',data=Toyota).fit().rsquared  
vif_sp = 1/(1-rsq_sp) #1.16

rsq_cd = smf.ols('Doors~cc+KM+Age_08_04+HP+Gears+Quarterly_Tax+Weight',data=Toyota).fit().rsquared  
vif_cd = 1/(1-rsq_cd) #1.15

rsq_cg = smf.ols('Gears~Doors+cc+KM+Age_08_04+HP+Quarterly_Tax+Weight',data=Toyota).fit().rsquared
vif_cg = 1/(1-rsq_cg) #1.09

rsq_cq = smf.ols('Quarterly_Tax~Gears+Doors+cc+KM+Age_08_04+HP+Weight',data=Toyota).fit().rsquared
vif_cq = 1/(1-rsq_cq) #2.31
print(vif_cq)

rsq_cw = smf.ols('Weight~Quarterly_Tax+Gears+Doors+cc+KM+Age_08_04+HP',data=Toyota).fit().rsquared
vif_cw = 1/(1-rsq_cw) #2.51
print(vif_cw)
#all the vif 's are less than 10 so we don't have any varibale to remove we try transformation

ml2 = smf.ols('Price~Age_08_04+KM+HP+np.log(cc)+Doors+Gears+Quarterly_Tax+Weight',data=Toyota).fit()
ml2.summary() #0.86
pred2 = ml2.predict(Toyota)
pred2
actual2=Toyota.Price
error2=pred2-actual2
error2

sqrs2 = error2*error2
mse2 = np.mean(sqrs2)
rmse2 = np.sqrt(mse2)
print(rmse2) 
#r^squared 86.7% and rmse is 1322.12
#we try doing another transformations because we have insignificant p value for one varible "doors"

ml3 = smf.ols('Price~Age_08_04+KM+HP+np.log(cc)+np.log(Doors)+Gears+Quarterly_Tax+Weight',data=Toyota).fit()
ml3.summary()

pred3 = ml3.predict(Toyota)
pred3
actual3=Toyota.Price
error3=pred3-actual3
error3

sqres = error3*error3
mse = np.mean(sqres)
rmse = np.sqrt(mse)
print(rmse)
#r^squared and rmse are same there is no chnage in accuarcy and error rate 
#applying sqaure trasnformation to the variable doors
ml4 = smf.ols('Price~Age_08_04+KM+HP+np.log(cc)+np.sqrt(Doors)+Gears+Quarterly_Tax+Weight',data=Toyota).fit()
ml4.summary()

pred4 = ml4.predict(Toyota)
pred4
actual4=Toyota.Price
error4=pred4-actual4
error4
rmse4 = np.sqrt(np.mean(error4*error4))
print(rmse4)
#r^sqquared 0.86 and rmse is 1322 and still
#p value is insignificant for doors
#applying exp transformation
ml5 = smf.ols('np.log(Price)~Age_08_04+KM+HP+np.log(cc)+Doors+Gears+Quarterly_Tax+Weight',data=Toyota).fit()
ml5.summary()
pred5 = ml5.predict(Toyota)
pred5
pred5_log = np.exp(pred5)
pred5_log
actual5=Toyota.Price
error5=pred5-actual5
error5
rmse5 = np.sqrt(np.mean(error5*error5))
print(rmse5)
#r^squared 0.85 and rmse is 11317 which is huge variation and cannot accept this model
## Train,test data and accuracy

from sklearn.model_selection import train_test_split
Toyota_train,Toyota_test  = train_test_split(Toyota,test_size = 0.3) # 30% test data

# preparing the model on train data 
model_train = smf.ols('Price~Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight',data=Toyota_train).fit()

# train_data prediction
train_pred = model_train.predict(Toyota_train)

# train residual values 
train_resid  = train_pred - Toyota_train.Price

# RMSE value for train data 
train_rmse = np.sqrt(np.mean(train_resid*train_resid)) 
print(train_rmse) # 1304.8

# prediction on test data set 
test_pred = model_train.predict(Toyota_test)

# test residual values 
test_resid  = test_pred - Toyota_test.Price

# RMSE value for test data 
test_rmse = np.sqrt(np.mean(test_resid*test_resid))
print(test_rmse)#1396.6

# both the rmse is almost similiar which say it properly fits
# Lets plot the influential plot to see if there are any influential records using AVPLOT
#AVPLOT in Python
import matplotlib.pyplot as plt
fig, ax = plt.subplots(figsize=(15,8))
fig = sm.graphics.influence_plot(ml1, ax=ax, criterion="cooks")
import statsmodels.api as sm
Toyota_t = Toyota.drop(81,axis =0)
model_t1 = smf.ols('Price~Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight',data=Toyota_t).fit()
model_t1.summary() #0.86
#cc became significant after removing 81st influntial variable aqnd doors is still insignificant and have no correlation
model_t2 = smf.ols('Price~Age_08_04+KM+HP+cc+Gears+Quarterly_Tax+Weight',data=Toyota_t).fit()
model_t2.summary() #0.86

pred_final = model_t2.predict(Toyota)
pred_final

actual_f=Toyota.Price
error_f=pred_final-actual_f
error_f
rmse_f = np.sqrt(np.mean(error_f*error_f))
print(rmse_f)
#1338 is the rmse and 0.86 is the r^sqaured which is good model