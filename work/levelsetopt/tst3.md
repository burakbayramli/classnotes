
```python
gamma = 0.5
w=-0.5
c = np.linspace(-1,1,100)
l = np.log(c / w) / -gamma
print (l)
plt.plot(c,l)
plt.savefig('/tmp/out3.png')
```

```text
[-1.38629436 -1.34547662 -1.30380844 -1.26125365 -1.21777367 -1.1733274
 -1.1278709  -1.08135717 -1.03373588 -0.98495297 -0.93495037 -0.8836655
 -0.83103089 -0.77697354 -0.72141441 -0.66426767 -0.6054399  -0.5448292
 -0.48232411 -0.41780239 -0.35112955 -0.2821572  -0.21072103 -0.13663849
 -0.05970593  0.02030474  0.10365014  0.19062036  0.28154511  0.37680121
  0.47682205  0.58210951  0.69324922  0.81093022  0.93597093  1.06935368
  1.21227161  1.36619369  1.53295691  1.71490046  1.91506738  2.13751865
  2.38784494  2.67404662  3.00815479  3.40949618  3.91212504  4.58506951
  5.60672076  7.80394534         nan         nan         nan         nan
         nan         nan         nan         nan         nan         nan
         nan         nan         nan         nan         nan         nan
         nan         nan         nan         nan         nan         nan
         nan         nan         nan         nan         nan         nan
         nan         nan         nan         nan         nan         nan
         nan         nan         nan         nan         nan         nan
         nan         nan         nan         nan         nan         nan
         nan         nan         nan         nan]
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

ax.plot_surface(xx, yy, zz, cmap=cm.coolwarm,linewidth=0, antialiased=False)

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


















