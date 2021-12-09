



shapiro wilk
   https://machinelearningmastery.com/time-series-seasonality-with-python/
   https://ocw.mit.edu/courses/mathematics/18-01sc-single-variable-calculus-fall-2010/unit-5-exploring-the-infinite/part-b-taylor-series/session-99-taylors-series-continued/MIT18_01SCF10_Ses99c.pdf
   https://github.com/BMClab/BMC/blob/master/functions/detect_cusum.py



```python
import pandas as pd

y = np.random.randn(300)/5
y[100:200] += np.arange(0, 4, 4/100)
x = range(len(y))
df = pd.DataFrame(y,columns=['y'])
df['x'] = x
df = df.set_index('x')

#y = np.random.randn(300)
#y[100:200] += 6
#df = pd.DataFrame(y,columns=['y'])
#df['x'] = x
#df = df.set_index('x')

#x = 2*np.pi*np.arange(0, 3, .01)
#y = 2*np.sin(x)
#df = pd.DataFrame(y,columns=['y'])
#df['x'] = x
#df = df.set_index('x')

df.plot()
plt.savefig('out.png')
```


```python
from sklearn.linear_model import LinearRegression
from scipy import stats

def test_change(df, x_cp):
    print (x_cp)
    model = LinearRegression()
    block1 = df[df.index < x_cp]
    Y = np.array(block1.y).reshape(-1,1)
    x = np.array(block1.index).reshape(-1,1)
    print (Y.shape)
    print (x.shape)
    model.fit(Y,x)
    trend = model.predict(x)
    detrended = Y - trend

    x = x.reshape(-1); detrended = detrended.reshape(-1)
    deg = 4
    coef = np.polyfit(x, detrended, deg)
    print (coef)
    polypred = [np.sum([xx**(deg-c)*c for c in coef]) for xx in x]
    deseasoned = detrended - polypred

    print (stats.shapiro(deseasoned))

    #detrended.plot()
    #plt.savefig('out.png')

test_change(df, 100)

```

```text
100
(100, 1)
(100, 1)
[-1.20115262e-09  5.80129443e-07 -1.95022345e-05 -1.86647321e+00
 -4.93929664e+01]
ShapiroResult(statistic=nan, pvalue=1.0)
```

<0.05 normality rejected