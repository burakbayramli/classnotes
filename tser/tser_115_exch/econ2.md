

NEER def

https://www.investopedia.com/terms/n/neer.asp

https://www.bruegel.org/publications/datasets/real-effective-exchange-rates-for-178-countries-a-new-database


```python
import pandas as pd, zipfile
with zipfile.ZipFile('REER_database_ver6Apr2020.zip', 'r') as z:
    df =  pd.read_excel(z.open('REER_database_ver6Apr2020.xls'),sheet_name='NEER_MONTHLY_38')
```


```python
df1 = df[[df.columns[0], 'NEER_38_JP']]
df1.columns = ['date','yen']
df1['date'] = df1['date'].str.replace('M','')
df1['date'] = pd.to_datetime(df1['date'],format="%Y%m")
df1 = df1.set_index('date')
df1 = df1[(df1.index > '1969-01-01') & (df1.index < '1980-01-01')]
df1.plot()
plt.savefig('out.png')
```



