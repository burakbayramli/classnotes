import pandas as pd, datetime
from pandas_datareader import data, wb
dat = wb.download(indicator=['SL.UEM.TOTL.ZS','NY.GDP.DEFL.KD.ZG','NE.RSB.GNFS.ZS'], country=['USA', 'TUR','GBR'], start=1970, end=2016)
dat.to_csv('data.csv')


