# -*- coding: utf-8 -*-
"""
Created on Mon May  4 16:03:04 2020

@author: Barath goud
"""

import pandas as pd
import numpy as np

St=Startupscsv.copy()
St.isna().sum()
St.head()
St.State.value_counts()

a=pd.get_dummies(St,columns=["State"])
a = a.drop(['State_California'],axis=1) # To avoid multicollinearity,drop one dummy column
t = St.describe()

St.dtypes
a.dtypes

St =a.copy()

cols = list(St.columns.values)
cols_new = ['R&D Spend','Administration','Marketing Spend','State_Florida','State_New York','Profit']
St = St.reindex(columns=cols_new)
St.columns = ['RndD','Administration','Marketing','State_F','State_N','Profit']


import matplotlib.pyplot as plt
#Boxplot check
plt.boxplot(St['RndD'])
plt.boxplot(St['Marketing'])
#whereas Administration have no correlation

#LINE Assumptions( Linear,Independent, Normal,Equal Variance)
#Lets do scatter plot
import seaborn as sns
sns.pairplot(St.iloc[:,:])
import statsmodels.api as sm
St.corr()
import statsmodels.formula.api as smf # for regression model

y = St['Profit']
x=St.iloc[0:,0:5]
ml1 = smf.ols('y~x', data = St).fit() # regression model
ml1.summary()
# # Summary #0.95
#removing states column and building the model becuase they are actegorical data and converted into dummies which cannot be having any correlation
ml2 = smf.ols('Profit~RndD+Administration+Marketing',data=St).fit() 
# # Summary

ml2.summary() # R^2 is 0.951
#p-values are high for all the columns except for RndD


plt.scatter(St.Marketing, St.Administration)

#Lets see if we can remove one insignificant independent variable
#VIF-Variable Inflation Factor

rsq_rnd = smf.ols('RndD~Administration+Marketing+State_F+State_N',data=St).fit().rsquared   
vif_rnd = 1/(1-rsq_rnd) #2.4955

rsq_admin = smf.ols('Administration~RndD+Marketing+State_F+State_N',data=St).fit().rsquared  
vif_admin = 1/(1-rsq_admin) #1.777

rsq_marketing = smf.ols('Marketing~RndD+Administration+State_F+State_N',data=St).fit().rsquared  
vif_marketing = 1/(1-rsq_marketing) #2.4167

#states column can be removed or used it deosnt affect the model in anyway we prefer to remove them to make the models easy to predict
#Vif as dataframe
d1 = {'Variables':['RndD','Administration','Marketing'],'VIF':[vif_rnd,vif_admin,vif_marketing]}
Vif_frame = pd.DataFrame(d1)  
Vif_frame
#No Vif value is >10 to ignore

d, re#Let us just go with backward approach methomove the predictor with highest P-value-Administration
ml1_t = smf.ols('Profit~RndD+Marketing+State_F+State_N',data=Start).fit() 
ml1_t.summary() # No change in R^2 value, even now p-value of Marketing is high

#Remove Marketing
ml1_t1 = smf.ols('Profit~RndD+Administration+State_F+State_N',data=Start).fit() 
ml1_t1.summary()# R^2 value is 0.948 ,p value insignificant for admin

ml1_t2 = smf.ols('Profit~RndD+State_F+State_N',data=Start).fit() 
ml1_t2.summary() # 0.947 is R^2 value

# Residual Plots
fig = plt.figure(figsize=(10, 7))
sns.residplot(Start.Administration, Start.Profit, color='blue') 
sns.residplot(Start.Marketing, Start.Profit, color='magenta')
sns.residplot(Start.RndD, Start.Profit, color='orange')
#Performing different transformations to get good R^2 value
ml2 = smf.ols('Profit~RndD+(Administration*Administration)+np.sqrt(Marketing)+State_F+State_N',data=Start).fit() 

ml2.summary() # No change in R^2 value same 0.951, p values are high for Admin nd Marketing


ml3 = smf.ols('Profit~RndD+np.log(Administration)+Marketing+State_F+State_N',data=Start).fit() 

ml3.summary() #R^2 is 0.95, p-values are high

#Considering 0 values in the data as missed to capture ones, lets process the data and substitute 0 with mean of the column in RndD, Marketing

St3 = St.copy()
St3['RndD']=St3['RndD'].replace(0,(St3['RndD'].mean()))
St3['Marketing']=St3['Marketing'].replace(0,(St3['Marketing'].mean()))

ml4 = smf.ols('Profit~RndD+Administration+Marketing+State_F+State_N',data=St3).fit() 
ml4.summary() # R^2 value decreased 0.812 , so not a correct method may be
#Now lets remove administrationa nd try
ml4_t = smf.ols('Profit~RndD+Marketing+State_F+State_N',data=St3).fit() 
ml4_t.summary()# No change in R^2-0.812 and still p value high for Marketing
plt.boxplot(St3['RndD'])
plt.boxplot(St3['Marketing']) # Data stays the same normalised, just the IQR reduced

ml5 = smf.ols('Profit~RndD+np.log(Administration)+np.log(Marketing)+State_F+State_N',data=St3).fit() 
ml5.summary() # R^2 is 0.801, p values are insignificant



## Train,test data and accuracy

from sklearn.model_selection import train_test_split
start_train,start_test  = train_test_split(St,test_size = 0.3) # 30% test data

# preparing the model on train data 
model_train = smf.ols("Profit~RndD+Administration+Marketing+State_F+State_N",data=start_train).fit()
model_train.summary() #0.93
# train_data prediction
train_pred = model_train.predict(start_train)

# train residual values 
train_resid  = train_pred - start_train.Profit

# RMSE value for train data 
train_rmse = np.sqrt(np.mean(train_resid*train_resid)) # 9147.36

# prediction on test data set 
test_pred = model_train.predict(start_test)

# test residual values 
test_resid  = test_pred - start_test.Profit

# RMSE value for test data 
test_rmse = np.sqrt(np.mean(test_resid*test_resid)) # 9027.3

# Lets plot the influential plot to see if there are any influential records using AVPLOT
#AVPLOT in Python
fig, ax = plt.subplots(figsize=(12,8))
fig = sm.graphics.influence_plot(ml1, ax=ax, criterion="cooks")

Start_f = St.drop(49,axis =0)
model_t1 = smf.ols("Profit~RndD+Administration+Marketing",data=Start_f).fit()
model_t1.summary() # R^2 is 0.962 , p-values are high for Admin and Marketing

model_t2 = smf.ols("Profit~RndD+Marketing",data=Start_f).fit()
model_t2.summary() # R^2 is 0.962 and p-values are fine now except for categorical data

# model_t2 prediction 
Pred = model_t1.predict(Start_f)
Pred
# train residual values 
Res  = Pred - Start_f.Profit

# RMSE value for train data 
rmse = np.sqrt(np.mean(Res*Res))
print(rmse)# 7430
#considering model_t2 is best fit model with good accuracy and less rmse comparing to before models 













# Using Sklearn performing the Linear Regression
from sklearn.linear_model import LinearRegression
         
# Preparing model                  
LR1 = LinearRegression()
mod1=LR1.fit(Start.iloc[:,:5],Start.Profit)
# Getting coefficients of variables               
LR1.coef_
LR1.intercept_

LR1.score(Start.iloc[:,:5],Start.Profit) #Adjusted R^2 =0.95
Start2 = Start.copy()
Start2['Administration'] = np.log(Start2['Administration'])
Start2['Marketing Spend'] = (Start2['Marketing Spend'] * Start2['Marketing Spend'] )

LR2 =LinearRegression()
mod1=LR1.fit(Start.iloc[:,:5],Start2.Profit)

