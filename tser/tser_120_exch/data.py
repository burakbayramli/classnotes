# Alttaki adresten REER_database_ver6Apr2020.zip dosyasini aliyoruz.
# https://www.bruegel.org/publications/datasets/real-effective-exchange-rates-for-178-countries-a-new-database
# 
import numpy as np
import pandas as pd, zipfile


import pandas as pd, datetime
from pandas_datareader import data
start=datetime.datetime(1977, 1, 1)
end=datetime.datetime(1991, 1, 1)
# german current account
df2 = data.DataReader(['BPBLTT01DEQ188S'], 'fred', start, end)

with zipfile.ZipFile('/tmp/REER_database_ver6Apr2020.zip', 'r') as z:
    df1 =  pd.read_excel(z.open('REER_database_ver6Apr2020.xls'),sheet_name='NEER_MONTHLY_38')
df1 = df1[[df1.columns[0], 'NEER_38_DE']]
df1.columns = ['date','DE']
df1['date'] = df1['date'].str.replace('M','')
df1['date'] = pd.to_datetime(df1['date'],format="%Y%m")
df1 = df1.set_index('date')
df1 = np.log(df1)
df1 = df1[(df1.index > '1977-01-01') & (df1.index < '1991-01-01')]

df = df2.join(df1)

df.to_csv('de.csv')
