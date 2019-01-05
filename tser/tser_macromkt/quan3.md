
```python
import pandas as pd

df1 = pd.read_csv('quandl-gdp.csv',index_col=0,parse_dates=True)
df2 = pd.read_csv('quandl-inf.csv',index_col=0,parse_dates=True)
```

```python
df1['gdpyoy'] = (df1.Value - df1.Value.shift(4)) / df1.Value.shift(4) * 100.0
df1.tail(120).gdpyoy.plot()
plt.savefig('/data/data/com.termux/files/home/Downloads/quan3_1.png')
```

```python
df2.tail(200).Value.plot()
plt.savefig('/data/data/com.termux/files/home/Downloads/quan3_2.png')
```
