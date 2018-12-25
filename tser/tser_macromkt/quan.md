
mccollough

Vid 1) https://www.youtube.com/watch?v=hS-JOXZrcdU

Vid 2) https://www.youtube.com/watch?v=2NvNGAIvcb0

Vid 3) https://youtu.be/V-uoyDj0tKs

Vid 4) https://youtu.be/27SQ5mYf38k

Quad1 good, and Quad2 really good. In both growth is going up.

Quad2 growth and inf going up at the same time. Everyone wins except golds, bonds, and dollar.

Quad4 g and inf go down at the same time. 

In Quad4 you want to be
   long dollar
   long low beta defensives, utilities, consumer staples
   long bonds
   short momentum
   short growth
   short technology

if inflation is falling, the 10yr follows that almost like a glove, u
have to get inf right to get 10yr bond yield right.

From Quad 2 to Quad 4, 
     DO NOT long momentum, high beta, tech, russell growth.
     DO low beta, divident yielding stocks, healthcare, consumer staples, etc.


Quad 3 is stagflation, that's what central bankers do, when they
realize they are in Quad 4.  They devalue curr, create asset price
inf.  That's what Chinese did on 2018 when they realized they were in
Quad 4.  They devalued yuan by 4%, they tried to create illusion of
growth through asset price inflation, but what they get is economic
stagflation.

Quad 1 and 2 - all good
Quad 3 stagflation
Quad 4 deflation

Bond proxies are utilities, consumer staples.

Growth and inflation slowing means margins compressing, causes deleverage P&L.

Vid 4, 39:50, On Dalio saying "US econ is 2 years away from downturn,
$ will plunge, as gov prints money to fund deficit, next crisis will
be more on dollar than debt". Get timing right. If the US is going
into recession, and the world is in recession, then the dollar is
going up. The rest of the world has to use world's reserve currency to
fund a deleveraging of their invested position. That's why in Quad4
the $ has the highest expected value.



```python
import quandl, os

fname = '%s/.quandl' % os.environ['HOME']
if not os.path.isfile(fname):
    print ('Please create a %s file ' % fname)
    exit()
auth = open(fname).read()

df1 = quandl.get("FRED/GDPC1-Real-Gross-Domestic-Product", returns="pandas",authtoken=auth)
df2 = quandl.get("RATEINF/INFLATION_USA-Inflation-YOY-USA", returns="pandas",authtoken=auth)
df1.to_csv('quandl-gdp.csv')
df2.to_csv('quandl-inf.csv')
```

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

```text
       gdpyoy    ...          cpi
277  1.295951    ...     1.081333
278  1.536247    ...     1.051000
279  1.878757    ...     1.120667
280  1.938263    ...     1.801333
281  2.114707    ...     2.539667
282  2.338877    ...     1.899000
283  2.471707    ...     1.964000
284  2.580414    ...     2.117667
285  2.869807    ...     2.214333
286  3.039632    ...     2.712000

[10 rows x 3 columns]
```

```python
import random
plt.xlim(-1.0,1.0)
plt.ylim(-1.0,1.0)
plt.grid()
res = df[['Q','cpidiff','gdpdiff']].tail(14)
for (q,x,y) in np.array(res):
    plt.plot(x,y,'rd')
    xa = random.choice(np.linspace(0,0.05,3))
    plt.text(x+xa,y+xa,q)
plt.savefig('quads.png')    
```

When irates go up and u r in Quad2, bonds go down. When irates go down
and u r in Quad4 bonds go up.





