Rosen
1.0,1.0

Peaks
-1.34739625,  0.20451886]

Himmelblau
[3,2][-2.805,3.131],[-3.779,-3.28][3.584,-1.848]


```python
def rosen(x,y):
    return 100*(y-x**2)**2+(1-x)**2


N = 30
x = np.linspace(-3,3,N); y = np.linspace(-3,3,N)
xx,yy = np.meshgrid(x,y)
zz = rosen(xx,yy)


from mpl_toolkits.mplot3d import Axes3D
from matplotlib import cm
fig = plt.figure()
ax = fig.gca(projection='3d')
ax.view_init(elev=9, azim=129)

ax.plot_surface(xx, yy, zz, cmap=cm.coolwarm,linewidth=0, antialiased=False)

plt.savefig('/tmp/out2.png')
```

```python
from scipy.interpolate import Rbf
import hammer

n = 20
x = -3 + 6*hammer.hammersley([2,3],n)
z = rosen(x[:,0],x[:,1])

rbfi = Rbf(x[:,0],x[:,1],z,function='gaussian',epsilon=0.15)
print (rbfi.nodes)

minidx = np.argmin(rbfi.nodes)
print (minidx)
print (rbfi.xi.T[minidx])

x1,y1 = rbfi.xi.T[minidx]
z1 = rbfi(x1,y1)
print (x1,y1,z1)

fig = plt.figure()
ax = fig.gca(projection='3d')
ax.view_init(elev=49, azim=-109)
ax.plot_wireframe(xx, yy, zz,rstride=2, cstride=2)
ax.plot([x1],[y1],[z1],'rd')
plt.savefig('/tmp/out3.png')
```

```text
[1.42265883e+04 1.03415309e+04 3.32932000e+03 3.50242000e+03
 3.10600000e+02 2.03125000e+03 5.24500000e+01 2.46970000e+02
 3.59770000e+02 7.38812500e+02 1.50625000e+01 1.48112500e+02
 2.29682500e+02 7.20932500e+02 9.96250000e+00 6.89312500e+02
 3.84625000e+01 5.21766062e+03 3.10723563e+03 7.40319062e+03]
14
[1.2   1.125]
1.1999999999999993 1.125 9.962499999999897
```





















