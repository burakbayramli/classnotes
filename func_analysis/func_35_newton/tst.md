

```python
from autograd import grad, hessian, numpy as anp

def f1(x):
   x1,x2 = x
   return f2(x1,x2)

@np.vectorize
def f2(x1,x2):
   return anp.exp(x1 + 3*x2 - 0.1) + \
          anp.exp( x1 - 3*x2 - 0.1 ) + \
	  anp.exp(-x1-0.1)

h2 = hessian(f1, 0)
res = h2(anp.array([1.0,1.0]))
print (res)
```

```text
[[0. 0.]
 [0. 0.]]
```




```python
from mpl_toolkits.mplot3d import Axes3D

D = 50
x = np.linspace(-2.0,1.0,D)
y = np.linspace(-1.0,1.0,D)

xx,yy = np.meshgrid(x,y)
zz = f2(xx,yy)

contours = [1,2,3,4,5,6]
cs=plt.contour(xx,yy,zz,contours)
plt.savefig('boyd-1092.png')
```

```python
fig = plt.figure()
ax = fig.gca(projection='3d')
surf = ax.plot_surface(xx, yy, zz)

plt.savefig('boyd-1091.png')
```

















