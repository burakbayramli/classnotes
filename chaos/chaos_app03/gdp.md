
```python
import pandas as pd, datetime
from pandas_datareader import data
start=datetime.datetime(1970, 1, 1)
end=datetime.datetime(2018, 1, 1)
df = data.DataReader(['A4102C1A027NBEA','GDP'], 'fred', start, end)
df.to_csv('gdp.csv')
```



```python
import pandas as pd
df = pd.read_csv('gdp.csv',parse_dates=True)
df.columns = ['date','W','GDP']
df = df.dropna()
df = df.set_index('date')
df['wage_share'] = df.W / df.GDP
print df.head()
```

```text
                  W       GDP  wage_share
date                                     
1970-01-01  551.537  1053.528    0.523514
1971-01-01  584.528  1137.812    0.513730
1972-01-01  638.786  1233.807    0.517736
1973-01-01  708.775  1380.680    0.513352
1974-01-01  772.304  1494.654    0.516711
```

```python
df['wage_share'].plot()
plt.savefig('chaos_app03_01.png')
```






