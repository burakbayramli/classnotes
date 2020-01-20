

```python
import autograd as ad
from autograd import numpy as anp
import scipy.sparse as sps

MU = 50.0
EPSILON = 0.001
n = 3
np.random.seed(0)
xorig = np.random.randn(n,n)
data = np.array([-1*np.ones(n), np.ones(n)])
diags = np.array([0, 1])
D = sps.spdiags(data, diags, n, n).todense()
#print (D)
#print (xorig)

def f(u):
    return anp.power(EPSILON**2 + anp.power(u,2),0.5) - EPSILON

def obj(xvec):
    x = xvec.reshape(n,n)
    Ux = D.dot(x)
    Uy = D.T.dot(x.T).T
    Ux = f(Ux).sum()
    Uxsum = f(Ux).sum()
    Uysum = f(Uy).sum()
    phi_atv = Uxsum + Uysum
    print (phi_atv)

    E = xorig-x
    diff = anp.power(anp.dot(E,(E.T)),2).sum()

    psi = diff + MU*phi_atv
    
    #print (psi)
    return psi

    
x0 = np.array([[1.,2.,3.],[2.,3.,4.],[4.,5.,6.]])
print (x0)
obj(x0)

grad = ad.grad(obj)
x00 = ad.numpy.reshape(x0,n*n)
grad(x00)
```

```text
[[1. 2. 3.]
 [2. 3. 4.]
 [4. 5. 6.]]
36.98100645417316
Autograd ArrayBox with value 36.98100645417316
Out[1]: 
array([ 138.90067617,  426.86417324,  743.94097718,  370.78996771,
        926.44601801, 1533.45533661,  901.69286291, 1883.47445845,
       2788.11365041])
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

```text
  (0, 0)	20.0
  (0, 1)	40.0
  (0, 2)	60.0
[44. 20. 30.]
  (0, 0)	88.0
  (0, 1)	40.0
  (0, 2)	60.0
```




