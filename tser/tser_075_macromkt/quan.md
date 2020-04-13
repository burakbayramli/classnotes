
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

Volatility

youtube.com/watch?v=FwpckOfUyRk

youtube.com/watch?v=-WJtepxbQbE

youtube.com/watch?v=USrojq9tgzs

Volume, volatility, price relation
https://m.youtube.com/watch?v=n3u8fMYKmwA


When irates go up and u r in Quad2, bonds go down. When irates go down
and u r in Quad4 bonds go up.

if an asset’s price and volume is rising while its volatility is
falling that’s bullish. It means investors are buying with
conviction. Conversely, if an asset’s price is falling while volume
and volatility are rising that’s bearish. It means investors are
selling with conviction

https://accounts.hedgeye.com/products/daily-trading-ranges/61

How to trade on bear trends, vol
https://m.youtube.com/watch?v=n3u8fMYKmwA

Get that soros comment on vol

More vol
https://m.youtube.com/watch?v=CBCenp7_j_M

Narrow risk range
https://m.youtube.com/watch?v=NBVin93Y4M4


















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
#print (df2c.tail(20))
```



```python
df = df1[['gdpyoy','Q']].merge(df2c[['cpi','Q']], on='Q')
df['gdpyoy'] = df.gdpyoy.shift(1)
df['gdpdiff'] = df.gdpyoy.diff()
df['cpidiff'] = df.cpi.diff()
df = df.dropna()
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

Scraping options

https://www.nasdaq.com/symbol/aapl/option-chain



