

```python
c = -3.4
gamme = 0.5
w=-0.5
np.log(c/w) / -gamma
```

```text
Out[1]: -19.16922612182061
```




```python
import numpy.linalg as lin

N = 30
x = np.linspace(-3,3,N); y = np.linspace(-3,3,N)
xx,yy = np.meshgrid(x,y)

@np.vectorize
def rbf1(xarg,yarg):
    m1=np.array([1,1]); gamma1=0.5; w1=-0.5
    return w1*np.exp(-gamma1*lin.norm(np.array([xarg,yarg])-m1)  )

res = rbf1(xarg=1, yarg=2)
#print (res)
zz = rbf1(xx, yy)
print (zz.shape)

from mpl_toolkits.mplot3d import Axes3D
from matplotlib import cm
fig = plt.figure()
ax = fig.gca(projection='3d')
ax.view_init(elev=29, azim=-29)
surf = ax.plot_surface(xx, yy, zz, cmap=cm.coolwarm,linewidth=0, antialiased=False)
plt.savefig('/tmp/out1.png')
```

```text
(30, 30)
```

```python
contours = [-0.3]
cs=plt.contour(xx,yy,zz,contours)
plt.savefig('/tmp/out2.png')
```


















