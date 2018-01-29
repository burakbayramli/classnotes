import pandas as pd, datetime
from pandas_datareader import data
start=datetime.datetime(1970, 1, 1)
end=datetime.datetime(2017, 1, 1)
df = data.DataReader(['GDPC1','GDPPOT','PCEPI','FEDFUNDS'], 'fred', start, end)
df.to_csv('taylorfred.csv')
