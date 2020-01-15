

```python
from scipy.sparse import spdiags
data = np.array([[1, 2, 3, 4], [1, 2, 3, 4], [1, 2, 3, 4]])
diags = np.array([0, -1, 2])
spdiags(data, diags, 4, 4).toarray()
```

```text
Out[1]: 
array([[1, 0, 3, 0],
       [1, 2, 0, 4],
       [0, 2, 3, 0],
       [0, 0, 3, 4]])
```

```python
data = np.array([[-1, -1, -1, -1], [1, 1, 1, 1]])
diags = np.array([0, 1])
spdiags(data, diags, 4, 4).toarray()
```

```text
Out[1]: 
array([[-1,  1,  0,  0],
       [ 0, -1,  1,  0],
       [ 0,  0, -1,  1],
       [ 0,  0,  0, -1]])
```






```python
f = 'xcor.mat'
import scipy.io as sio
xcor = sio.loadmat(f)
xcor = test['xcor']
xcor = np.reshape(xcor,(len(xcor)))
print (xcor.shape)
plt.plot(range(len(xcor)), xcor)
plt.savefig('/tmp/out.png')
```

```text
(5000,)
```

```python
eps = 1e-6
mu = 50.0

def phi_tv(x):
   return np.sum(np.abs(np.diff(x)))
   
def phi_atv(x):
   return np.sum(np.sqrt(eps + np.power(np.diff(x),2)) - eps)
   
print (phi_tv(xcor))
print (phi_atv(xcor))
```

```text
155.63025715999999
155.8936302273509
```

```python
def f(u):
   return np.sum(np.power(u-xcor, 2)) + phi_atv(u)

u0 = np.ones(len(xcor))
print (f(u0))
```

```text
5585.878953812943
```



n=5000;
EPSILON = 0.001;
MU = 50;

```python
f = 'xcor.mat'
import scipy.io as sio
test = sio.loadmat(f)
xcor = test['xcor']
print (xcor.shape)
```

```text
(5000, 1)
```

```python
f = '/tmp/xcor.mat'
import scipy.io as sio
test = sio.loadmat(f)
xcor = test['xcor']
print (xcor.shape)

f = '/tmp/x.mat'
import scipy.io as sio
test = sio.loadmat(f)
x = test['x']
print (x.shape)
```

```text
(5000, 1)
(5000, 1)
```

```python
plt.plot(range(len(x)), xcor)
plt.plot(range(len(x)), x)
plt.savefig('/tmp/out.png')
```









