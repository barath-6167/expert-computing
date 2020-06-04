# -*- coding: utf-8 -*-
"""
Created on Tue May 19 22:39:43 2020

@author: Dell
"""

import pandas as pd
import numpy as np

#Importing Data
election = pd.read_csv("C:/Datasets_BA/Logistic regression/election_data.csv",sep=",")

#removoing row
e = election_datacsv.drop(election_datacsv.index[0])
election_datacsv.describe()
election_datacsv.head()
e.head()
cols = list(Start.columns.values)
cols_new = ['Election-id','Result','Year','Amount Spent','Popularity Rank']
e1 = e.reindex(columns=cols_new)
e1.columns = ['Election_id','Result','Year','AmountSpent','PopularityRank']
e1 = e1[['Election_id','Year','AmountSpent','PopularityRank','Result']]
e1 = e1.drop(['Election_id'],axis = 1)
e1 = e1.drop(['Year'],axis = 1)
logit_model = sm.logit('Result ~ AmountSpent+PopularityRank',data = e1).fit()
e1.shape
logit_model.summary()

### Splitting the data into train and test data 
from sklearn.model_selection import train_test_split
train_data,test_data = train_test_split(e1,test_size = 0.3) # 30% test data

# Model building 
import statsmodels.formula.api as sm
logit_model = sm.logit('Result ~ Year+AmountSpent+PopularityRank',data = train_data).fit(method = 'bfgs')
logit_model2 = sm.logit('Result ~ AmountSpent',data = e1).fit()
logit_model2.summary()
predict1 = logit_model2.predict(pd.DataFrame(e1['AmounSpent']))

logit_model3 = sm.logit('Result ~PopularityRank ',data = e1).fit()
logit_model3.summary()
predict3 = logit_model3.predict(pd.DataFrame(e1['PopularityRank']))
import matplotlib.pyplot as plt
plt.plot(e1.AmountSpent,predict1)
plt.plot(e1.PopularityRank,predict3)

#summary
logit_model.summary()
e2 = e1.iloc[:,0:2]

predict = logit_model.predict(pd.DataFrame(e2))
predict
from sklearn.metrics import confusion_matrix

cnf_matrix = confusion_matrix(e1['Result'], predict > 0.5 )
cnf_matrix

accuracy = ()/()
accuracy





