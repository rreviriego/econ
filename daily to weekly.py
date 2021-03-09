#!/usr/bin/env python
# coding: utf-8

# In[3]:


import pandas as pd
from datetime import datetime


# In[4]:


btc = pd.read_csv('btc.csv')


# In[5]:


btc.head()


# In[6]:


btc['Date'] = btc['Date'].astype('datetime64[ns]')


# In[9]:


btc_weekly = btc.resample('W-Wed', label = 'right', closed = 'right', on='Date').sum().reset_index().sort_values(by='Date')


# In[11]:


btc_weekly.head()


# In[12]:


btc_weekly.to_csv(r'C:\Users\revir\Documents\coding\tfg\btcweekly.csv', index = False, header = True)

