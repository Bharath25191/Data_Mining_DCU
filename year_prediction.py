
# coding: utf-8

# In[139]:

import pandas as pd
import numpy as np


# In[140]:

data=pd.read_csv("D:\\terror_clean.csv",encoding="utf8")


# In[ ]:




# In[141]:

data=data.dropna(axis=0)


# In[ ]:




# In[142]:

from sklearn.cross_validation import train_test_split


# In[143]:

data=data.drop(['Unnamed: 0','eventid'], axis=1)
pca_data=data.drop(['imonth','iday'], axis=1)


# In[144]:

target=data['iyear']


# In[145]:

inp=data.drop(['iyear','imonth','iday'],axis=1)


# In[146]:

inp=inp.as_matrix()


# In[147]:

inp


# In[148]:

X_train, X_test, y_train, y_test = train_test_split(inp, target, test_size=0.2, random_state=0)


# In[149]:

from sklearn.neighbors import KNeighborsClassifier


# In[150]:

neigh = KNeighborsClassifier(n_neighbors=3)


# In[151]:

neigh.fit(X_train, y_train) 


# In[152]:

print(neigh.score(X_test,y_test))


# In[153]:

from sklearn.decomposition import PCA


# In[154]:

pca = PCA()


# In[155]:

pca_data=pca_data.as_matrix()


# In[156]:

trans_data=pca.fit_transform(inp)


# In[157]:

trans_data


# In[158]:

X_train, X_test, y_train, y_test = train_test_split(inp, target, test_size=0.2, random_state=0)


# In[159]:

neigh.fit(X_train, y_train) 


# In[160]:

print(neigh.score(X_test,y_test))


# In[163]:

df = pd.DataFrame(inp)


# In[164]:

df.head()


# In[ ]:



