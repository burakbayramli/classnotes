Rosen
1.0,1.0

Peaks
-1.34739625,  0.20451886]

Himmelblau
[3,2][-2.805,3.131],[-3.779,-3.28][3.584,-1.848]


```python
def peaks(x,y):
    z =  (3*(1-x)**2 * np.exp(-(x**2) - (y+1)**2) 
          - 10*(x/5 - x**3 - y**5) * np.exp(-x**2 - y**2)
          - 1/3 * np.exp(-(x+1)**2 - y**2)) 
    return(z)

N = 30
x = np.linspace(-3,3,N); y = np.linspace(-3,3,N)
xx,yy = np.meshgrid(x,y)
zz = peaks(xx,yy)


from mpl_toolkits.mplot3d import Axes3D
from matplotlib import cm
fig = plt.figure()
ax = fig.gca(projection='3d')
ax.view_init(elev=29, azim=-29)

ax.plot_surface(xx, yy, zz, cmap=cm.coolwarm,linewidth=0, antialiased=False)

plt.savefig('/tmp/out2.png')
```

```python
from scipy.interpolate import Rbf
import hammer

n = 20
x = -3 + 6*hammer.hammersley([2,3],n)
z = peaks(x[:,0],x[:,1])

rbfi = Rbf(x[:,0],x[:,1],z,function='gaussian',epsilon=0.15)
print (rbfi.nodes)

minidx = np.argmin(rbfi.nodes)
print (minidx)
print (rbfi.xi.T[minidx])
```

```text
[ 6.14175343e-05  2.89111875e-04 -4.27235071e-01  5.18883206e-02
  7.08353381e-02  7.37285266e-03 -1.70998631e+00  2.34481250e+00
  2.54064274e+00 -8.28222074e-01  4.10821862e-01 -3.42515772e+00
  4.82589627e+00 -2.98631953e+00  2.19824828e+00  2.86198996e+00
  5.18361207e-02 -5.80418410e-03  4.10485403e-01  2.39463033e-02]
11
[ 0.3   -1.125]
```












