
```python
x = np.random.randn(300)*np.sqrt(2)
e = np.random.randn(300)*np.sqrt(0.5)
y = np.sin(x)+e
df = pd.DataFrame([x,y]).T
df.columns = ['x','y']
df = df.sort_index(by='x')
print df.head()
knots=np.array([-5.5938, -3.7732, -1.9526, -0.1320, 1.6886, 3.5092, 5.3298]);
```

```text
            x         y
3   -3.501874  0.966620
249 -3.381457  0.194481
230 -3.065531 -0.526412
8   -2.891948  1.191124
78  -2.855147 -0.210229
```


f(x)=b0 + b1*x + b2*(x-t1)^3*(x>t1) + b3*(x-t2)^3*(x>t2) + ...

```python
import pandas as pd
df = pd.read_csv('sine.csv',comment='#',header=None,sep='\s')
df.columns = ['x','y']
df = df.sort_index(by='x')
knots=np.array([-5.5938, -3.7732, -1.9526, -0.1320, 1.6886, 3.5092, 5.3298]);
```


```python
import pandas as pd
df = pd.read_csv('cube.csv')
df = df.sort_index(by='x')
knots=np.array([3,3.5,8,14,14.5]);
```


```python
import scipy.linalg as lin

def rcs(x,y,knots):
    n = len(y)
    k = knots
    X1 = x
    q = len(k)-1
    myX=np.zeros((n,len(knots)-2))

    for j in range(q-1):
    	tmp1 = (x-k[j])**3 * (x>k[j])
	tmp2 = (x-k[q-1])**3 * (x>k[q-1])*(k[q]-k[j])
	XX= tmp1-tmp2/(k[q]-k[q-1])
        tmp1 = (x-k[q])**3 * (x>k[q])
        tmp2 = (k[q-1]-k[j])
	XX = XX+tmp1*tmp2/(k[q]-k[q-1])
	myX[:,j]=XX

    X = np.hstack( (np.ones((n,1)),np.reshape(X1,(n,1)),myX) )
    bhat = np.linalg.lstsq(X,y)[0]
    bhatt = np.zeros(len(knots)+1)
    bhatt[len(bhat)] = (bhat[2:]*(k[0:-2]-k[-1])).sum()
    bhatt[len(bhat)] = bhatt[len(bhat)] / (k[-1]-k[-2])
    bhatt = np.hstack([bhatt, 0])    
    bhatt[-1] = (bhat[2:]*(k[0:-2]-k[-2])).sum()
    bhatt[-1] = bhatt[-1] / (k[-2]-k[-1])
    bhat = np.hstack((bhat, bhatt[-2:]))
    return bhat

def speval(x,coefs,knots):
    tmp = coefs[0] + coefs[1]*x
    for k in range(len(knots)): tmp = tmp + coefs[k+2]*((x-knots[k])**3)*(x>knots[k])
    return tmp

bhat = rcs(df.x,df.y,knots)
print bhat
```

```text
[  2.92667505e+00   2.87723079e-01  -6.20021835e-04   2.02377439e-02
  -6.75860947e-02   4.47649368e-01  -3.99680995e-01]
```


```python
df['spline'] = speval(df.x, bhat, knots)
df2 = df.set_index('x')
df2[['y','spline']].plot()
plt.hold(True)
for k in knots: plt.plot(k,speval(k,bhat,knots),'rd')
plt.savefig('test_02.png')
```









