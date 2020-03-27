
```python
import numpy.linalg as lin

N = 10
x = np.linspace(-3,3,N); y = np.linspace(-3,3,N)
xx,yy = np.meshgrid(x,y)

@np.vectorize
def rbf1(x,y,m,gamma,w):
    return w*np.exp(-gamma*lin.norm(np.array([x,y])-m)  )

#res = rbf1(1, 2, np.array([[3,3]]), gamma=0.1, w=0.5)
zz = rbf1(xx, yy, m=2.0, gamma=0.1, w=0.5)
print (zz.shape)
#print
```

```text
(10, 10)
```









```python
from scipy.interpolate import Rbf
 
def func(x, y):
    s1 = 0.2; x1 = 36.5; y1 = 32.5
    g1 = np.exp( -4 *np.log(2) * ((x-x1)**2+(y-y1)**2) / s1**2)
    return g1
N = 100
x = np.linspace(-3,3,N); y = np.linspace(-3,3,N)
xx,yy = np.meshgrid(x,y)
zz = func(xx,yy)
rbfi = Rbf(xx, yy, zz)
```

```python
from mpl_toolkits.mplot3d import Axes3D
from matplotlib import cm

fig = plt.figure()
zi = rbfi(x, y)
xx,yy = np.meshgrid(x,y)
zz = rbfi(xx, yy)
ax = fig.gca(projection='3d')
ax.view_init(elev=29, azim=29)
surf = ax.plot_surface(xx, yy, zz, cmap=cm.coolwarm,linewidth=0, antialiased=False)
plt.savefig('out1.png')
```




















