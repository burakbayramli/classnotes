
```python
import pandas as pd
df = pd.read_csv('out.dat',comment='#',header=None,sep='\s')
df.columns = ['x','y']
df = df.sort_index(by='x')
coefs = pd.read_csv('coef.dat',comment='#',header=None,sep='\s')
coefs = coefs[0]
knots=np.array([-5.5938, -3.7732, -1.9526, -0.1320, 1.6886, 3.5092, 5.3298]);
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
bhat = rcs(df.x,df.y,knots)
print bhat
```

```text
[ 2.81385027  0.39846919 -0.10054507  0.31261315 -0.28652342 -0.11477276
  0.44401577 -0.35741489  0.10262723]
```










f(x)=b0 + b1*x + b2*(x-t1)^3*(x>t1) + b3*(x-t2)^3*(x>t2) + ...

```python
def fitted(x,coefs,knots):
    return coefs[0] + coefs[1]*x + \
           coefs[2]*((x-knots[0])**3)*(x>knots[0]) + \
           coefs[3]*((x-knots[1])**3)*(x>knots[1]) + \
           coefs[4]*((x-knots[2])**3)*(x>knots[2]) + \
           coefs[5]*((x-knots[3])**3)*(x>knots[3]) + \
           coefs[6]*((x-knots[4])**3)*(x>knots[4]) + \
           coefs[7]*((x-knots[5])**3)*(x>knots[5]) + \
           coefs[8]*((x-knots[6])**3)*(x>knots[6]) 
df['yy'] = fitted(df.x, coefs, knots)
print df.head()
df2 = df.set_index('x')
print df2.head()
df2[['y','yy']].plot()
plt.hold(True)
for k in knots: plt.plot(k,fitted(k,coefs,knots),'d')
plt.savefig('test_02.png')
```

```text
            x         y        yy
105 -6.256893  0.555405  0.320671
190 -6.165326 -0.420585  0.357158
53  -5.028769  1.599764  0.791903
66  -4.892917  1.074633  0.829556
25  -4.420171  0.923360  0.890011
                  y        yy
x                            
-6.256893  0.555405  0.320671
-6.165326 -0.420585  0.357158
-5.028769  1.599764  0.791903
-4.892917  1.074633  0.829556
-4.420171  0.923360  0.890011
```

