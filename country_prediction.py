
# coding: utf-8

# In[84]:

import pandas as pd
import numpy as np


# In[85]:

data=pd.read_csv("D:\\terror_clean.csv",encoding="utf8")


# In[ ]:




# In[86]:

data=data.dropna(axis=0)


# In[ ]:




# In[87]:

from sklearn.cross_validation import train_test_split


# In[88]:

data=data.drop('Unnamed: 0', axis=1)


# In[89]:

target=data['country']


# In[90]:

inp=data.drop(['region','country','latitude','longitude'],axis=1)


# In[91]:

inp=inp.as_matrix()


# In[92]:

inp


# In[93]:

X_train, X_test, y_train, y_test = train_test_split(inp, target, test_size=0.2, random_state=0)


# In[94]:

from sklearn.neighbors import KNeighborsClassifier


# In[95]:

neigh = KNeighborsClassifier(n_neighbors=1)


# In[96]:

neigh.fit(X_train, y_train) 


# In[97]:

print(neigh.score(X_test,y_test))


# In[98]:

from sklearn.decomposition import PCA


# In[99]:

pca = PCA()


# In[ ]:




# In[100]:

trans_data=pca.fit_transform(inp)


# In[101]:

trans_data


# In[102]:

X_train, X_test, y_train, y_test = train_test_split(inp, target, test_size=0.2, random_state=0)


# In[103]:

neigh.fit(X_train, y_train) 


# In[104]:

print(neigh.score(X_test,y_test))


# In[105]:

df = pd.DataFrame(inp)


# In[106]:

df.head()


# In[ ]:



