import pandas as pd, datetime
from pandas_datareader import data
start=datetime.datetime(1970, 1, 1)
end=datetime.datetime(2017, 1, 1)
# Personal Consumption Expenditures - PCE
# Federal government total expenditures - W019RCQ027SBEA
# Real Gross Private Domestic Investment - GPDIC1
# Net Exports of Goods and Services - NETEXP
# Real Gross Domestic Product - GDPC1
# Total Credit to Private Non-Financial Sector - CRDQUSAPABIS
df = data.DataReader(['PCE','W019RCQ027SBEA','GPDIC1','NETEXP','GDPC1','CRDQUSAPABIS'], 'fred', start, end)
df.columns = ['consump','govexp','invest','netexp','gdp','crt']
df.to_csv('data.csv')
