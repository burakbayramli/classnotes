

```python
from autograd import grad, hessian

def objective(X): # hedef
    x, y, z = X
    return x**2 + y**2 + z**2

h = hessian(objective, 0)
x,y,z = 1.0,1.0,1.0
res = h(np.array([x, y, z]))
print (res)
g = grad(objective, 0)
res = g(np.array([x, y, z]))
print (res)
```

```text
[[2. 0. 0.]
 [0. 2. 0.]
 [0. 0. 2.]]
[2. 2. 2.]
```




```python
from mpl_toolkits.mplot3d import Axes3D
@np.vectorize
def f(x1,x2):
   return np.exp(x1 + 3*x2 - 0.1) + np.exp( x1 - 3*x2 - 0.1 ) + np.exp(-x1-0.1)

D = 50
x = np.linspace(-2.0,1.0,D)
y = np.linspace(-1.0,1.0,D)

xx,yy = np.meshgrid(x,y)
zz = f(xx,yy)

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

















