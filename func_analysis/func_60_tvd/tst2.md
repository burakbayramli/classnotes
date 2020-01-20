

```python
import autograd as ad
from autograd import numpy as anp
import scipy.sparse as sps

MU = 50.0
EPSILON = 0.001
n = 300
np.random.seed(0)
xorig = np.random.randn(n,n)
data = np.array([-1*np.ones(n), np.ones(n)])
diags = np.array([0, 1])
D = sps.spdiags(data, diags, n, n)
#print (D)
#print (xorig)

def obj(xvec):
    x = xvec.reshape(n,n)
    Ux = D.dot(x)
    Uy = D.T.dot(x.T).T
    Ux = f(Ux).sum()
    Uxsum = f(Ux).sum()
    Uysum = f(Uy).sum()
    phi_atv = Uxsum + Uysum
    #print (phi_atv)

    E = xorig-x
    diff = anp.power(anp.dot(E,(E.T)),2).sum()

    psi = diff + MU*phi_atv
    
    #print (psi)
    return psi

    
#x0 = np.array([[1.,2.,3.],[2.,3.,4.],[4.,5.,6.]])
x0 = np.random.randn(n,n)
#print (x0)
obj(x0)

grad = ad.grad(obj)
x00 = ad.numpy.reshape(x0,n*n)
res = grad(x00)
#print (grad(x00))
```


```python
import chumpy as ch
x, y, A = ch.array([10,20,30]), ch.array([5]), ch.eye(3)
z = x.T.dot(A).dot(x)
f = z + y**2

df_x = f.dr_wrt(x)
print (df_x)

x[0] = 44
print (x)
df_x = f.dr_wrt(x)
print (df_x)
```



```python
import scipy.sparse as sps
import chumpy as ch

MU = ch.Ch(50.0)
EPSILON = ch.Ch(0.001)
n = 300
np.random.seed(0)
xorig = ch.array(np.random.randn(n,n))
data = np.array([-1*np.ones(n), np.ones(n)])
diags = np.array([0, 1])
D = sps.spdiags(data, diags, n, n)

#print (D.todense())


def f(u):
    return ch.sqrt(EPSILON**2 + ch.power(u,2)) - EPSILON

xvec = ch.array(ch.zeros(n*n))
x = xvec.reshape(n,n)
Ux = D.dot(x)
Uy = D.T.dot(x.T).T
Uxsum = f(Ux).sum()
Uysum = f(Uy).sum()
phi_atv = Uxsum + Uysum
print (type(phi_atv))
E = xorig-x
diff = ch.power(E.dot(E.T),2).sum()
print (diff)
print (type(diff))
psi = diff + MU*phi_atv
print (psi)
res = psi.dr_wrt(x)
```

```text
<class 'chumpy.ch_ops.add'>
[188.18395721]
<class 'chumpy.ch_ops.sum'>
[188.18395721]
```




