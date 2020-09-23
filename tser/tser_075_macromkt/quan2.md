

```python
import pandas as pd

df1 = pd.read_csv('quandl-gdp.csv',index_col=0,parse_dates=True)
df2 = pd.read_csv('quandl-inf.csv',index_col=0,parse_dates=True)

df1['gdpyoy'] = (df1.Value - df1.Value.shift(4)) / df1.Value.shift(4) * 100.0
def f(x):
    if x.name.month == 4: return "%d%s" % (x.name.year,"Q1")
    elif x.name.month == 7: return "%d%s" % (x.name.year, "Q2")
    elif x.name.month == 10: return "%d%s" % (x.name.year, "Q3")
    elif x.name.month == 1: return "%d%s" % (x.name.year-1, "Q4")

df1['Q'] = df1.apply(f, axis=1)
print (df1[['gdpyoy','Q']].tail(10))
```

```text
              gdpyoy       Q
Date                        
2016-04-01  1.295951  2016Q1
2016-07-01  1.536247  2016Q2
2016-10-01  1.878757  2016Q3
2017-01-01  1.938263  2016Q4
2017-04-01  2.114707  2017Q1
2017-07-01  2.338877  2017Q2
2017-10-01  2.471707  2017Q3
2018-01-01  2.580414  2017Q4
2018-04-01  2.869807  2018Q1
2018-07-01  3.039632  2018Q2
```

```python
df2['cpi'] = df2.resample('Q')[['Value']].mean()
df2c = df2.dropna()

def f(x):
    if x.name.month == 3: return "%d%s" % (x.name.year,"Q1")
    elif x.name.month == 6: return "%d%s" % (x.name.year, "Q2")
    elif x.name.month == 9: return "%d%s" % (x.name.year, "Q3")
    elif x.name.month == 12: return "%d%s" % (x.name.year, "Q4")

df2c['Q'] = df2c.apply(f, axis=1)
print (df2c[['cpi','Q']].tail(10))
print (df2c.tail(20))
```

```text
                 cpi       Q
Date                        
2016-06-30  1.051000  2016Q2
2016-09-30  1.120667  2016Q3
2016-12-31  1.801333  2016Q4
2017-03-31  2.539667  2017Q1
2017-06-30  1.899000  2017Q2
2017-09-30  1.964000  2017Q3
2017-12-31  2.117667  2017Q4
2018-03-31  2.214333  2018Q1
2018-06-30  2.712000  2018Q2
2018-09-30  2.642000  2018Q3
            Value   ...         Q
Date                ...          
2013-12-31  1.502   ...    2013Q4
2014-03-31  1.512   ...    2014Q1
2014-06-30  2.072   ...    2014Q2
2014-09-30  1.658   ...    2014Q3
2014-12-31  0.756   ...    2014Q4
2015-03-31 -0.074   ...    2015Q1
2015-06-30  0.124   ...    2015Q2
2015-09-30 -0.036   ...    2015Q3
2015-12-31  0.730   ...    2015Q4
2016-03-31  0.853   ...    2016Q1
2016-06-30  1.006   ...    2016Q2
2016-09-30  1.464   ...    2016Q3
2016-12-31  2.075   ...    2016Q4
2017-03-31  2.381   ...    2017Q1
2017-06-30  1.625   ...    2017Q2
2017-09-30  2.233   ...    2017Q3
2017-12-31  2.109   ...    2017Q4
2018-03-31  2.360   ...    2018Q1
2018-06-30  2.872   ...    2018Q2
2018-09-30  2.277   ...    2018Q3

[20 rows x 3 columns]
```


```python
df = df1[['gdpyoy','Q']].merge(df2c[['cpi','Q']], on='Q')
df['gdpyoy'] = df.gdpyoy.shift(1)
df['gdpdiff'] = df.gdpyoy.diff()
df['cpidiff'] = df.cpi.diff()
df = df.dropna()
```


x-axis 2Y average of yoy real gdp growth rate
y-axis marginal rate of change of the yoy real gdp


```python
df3 = df.copy()
roll=4;delay=-1
df3['gdpyoydiff'] = df3['gdpyoy'].diff()
df3['x']= df3['gdpyoy'].diff().rolling(roll).mean() * 100.0
df3['y'] = (df3.gdpyoy.diff() *100.0).shift(delay)
df4 = df3.head(250)
#plt.xlim(-300,300)
#plt.ylim(-300,300)
#plt.plot(df4.x,df4.y,'.')
#plt.savefig('out.png')

import statsmodels.formula.api as smf
results = smf.ols('y ~ x', data=df4).fit()

df5 = df3.tail(30)
df5['ypred'] = df5.x*results.params['x'] + results.params['Intercept']
#df5['ypred'] = df5.x*(-0.9095) + 3.3631
#df5['ypred'] = df5.x*(0.73596) - 0.11808703
df5['sign_acc'] = ((df5.ypred * df5.y) > 0)
print (df5.sign_acc.sum() / len(df5))
print (results.aic)
print (results.params)
print (results.rsquared)
tmp = np.array(df5[['gdpyoy','y','ypred','sign_acc']])
#print (tmp)
```

```text
0.6666666666666666
3155.5173735898506
Intercept    1.420050
x           -0.067526
dtype: float64
0.001947943182620926
```


```python
df6 = df5.copy()
df6.loc[:,'gdpyoypred'] = (df6['gdpyoy'] + df6['ypred']/5.0)
df6.loc[:,'gdpyoy2'] = df6['gdpyoy'].shift(1)
df6[['gdpyoy2','gdpyoypred']].plot()
plt.savefig('out2.png')
print (df6[['gdpyoy','ypred']].tail(20))
```

```text
       gdpyoy     ypred
267  1.917418  2.451008
268  2.614123 -0.513809
269  1.457197  1.613765
270  2.602733 -0.843878
271  3.035910 -0.468120
272  2.702265  1.271253
273  3.808564 -2.549388
274  3.368475  0.127371
275  2.379008  2.528992
276  2.000006  2.605561
277  1.556847  5.221263
278  1.295951  4.918759
279  1.536247  2.842749
280  1.878757  1.624736
281  1.938263  0.776167
282  2.114707  0.037875
283  2.338877  0.065098
284  2.471707  0.419066
285  2.580414  0.336008
286  2.869807  0.145337
```










```python
arr = pd.DataFrame([1,2,3,4,5])
print (arr.shift(-1))
```

```text
     0
0  2.0
1  3.0
2  4.0
3  5.0
4  NaN
```







