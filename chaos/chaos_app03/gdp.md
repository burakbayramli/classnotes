fred debt non-financial corporations NCBDBIQ027S
fred debt households and non-profit institutions CMDEBT
All Employees PAYEMS
employment level LNS11300060

```python
import pandas as pd, datetime
from pandas_datareader import data
start=datetime.datetime(1970, 1, 1)
end=datetime.datetime(2018, 1, 1)
df = data.DataReader(['A4102C1A027NBEA','GDP','CMDEBT','NCBDBIQ027S','POPTOTUSA647NWDB','LNS11300060'], 'fred', start, end)
df.to_csv('gdp.csv')
```



```python
import pandas as pd
df = pd.read_csv('gdp.csv',parse_dates=True)
df.columns = ['date','W','GDP','house_debt','corp_debt','N','emp']
df = df.dropna()
df = df.set_index('date')
df.loc[:,'N'] = df['N'] / 1000000.
df.loc[:,'corp_debt'] = df['corp_debt'] / 1000000.
df.loc[:,'house_debt'] = df['house_debt'] / 1000.
df['wage_share'] = df.W / df.GDP
print df.tail()
```

```text
                   W        GDP  house_debt  corp_debt           N   emp  \
date                                                                       
2012-01-01  6938.912  15973.881    13.41991   4.255168  313.998379  81.5   
2013-01-01  7126.099  16475.440    13.46482   4.598664  316.204908  81.1   
2014-01-01  7487.356  17031.324    13.65538   4.910201  318.563456  81.0   
2015-01-01  7870.624  17874.715    14.01678   5.237851  320.896618  81.0   
2016-01-01  8098.840  18325.187    14.29826   5.663045  323.127513  81.2   

            wage_share  
date                    
2012-01-01    0.434391  
2013-01-01    0.432529  
2014-01-01    0.439623  
2015-01-01    0.440322  
2016-01-01    0.441951  
```

```python
df['wage_share'].plot()
plt.savefig('chaos_app03_01.png')
```






