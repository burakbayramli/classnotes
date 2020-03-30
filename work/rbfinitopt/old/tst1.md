


```python
def Rosen2(x):
    return 100*(x[1]-x[0]**2)**2+(1-x[0])**2

def Peaks2(x):
    return 3*(1-x[0])**2*np.exp(-(x[0]**2) - (x[1]+1)**2) \
   - 10*(x[0]/5 - x[0]**3 - x[1]**5)*np.exp(-x[0]**2-x[1]**2) \
   - 1/3*np.exp(-(x[0]+1)**2 - x[1]**2)
   
xs = np.linspace(-2,2,1000); ys = np.linspace(-3,3,1000)

idx = np.random.randint(0,len(xs),100)

x = np.vstack((xs[idx],ys[idx]))
print (x.shape)
#print (xs[idx])
#print (x)
zs = Rosen2(x)
```

```text
(2, 100)
```















```python
from mpl_toolkits.mplot3d import Axes3D
import matplotlib.pyplot as plt
from matplotlib import cm
import numpy as np

np.random.seed(0)
N = 20

def Rosen(x,y):
    return 100*(y-x**2)**2+(1-x)**2

def Peaks(x,y):
    return 3*(1-x)**2*np.exp(-(x**2) - (y+1)**2) \
   - 10*(x/5 - x**3 - y**5)*np.exp(-x**2-y**2) \
   - 1/3*np.exp(-(x+1)**2 - y**2)

x = np.linspace(-2,2,250)
y = np.linspace(-3,3,250)
X, Y = np.meshgrid(x, y)
#Z = Rosenbrock(X, Y)
Z = Peaks(X, Y)

fig = plt.figure(figsize = (8,4))
#ax = fig.gca(projection='3d')
#ax.plot_surface(X,Y,Z,rstride = 5, cstride = 5, cmap = 'jet', alpha = .4, edgecolor = 'none' )
#ax.view_init(10, -133)
plt.contour(X, Y, Z, [-6.5])
plt.savefig('level1.png')
```

```python
def Rosen2(x):
    return 100*(x[1]-x[0]**2)**2+(1-x[0])**2

def Peaks2(x):
    return 3*(1-x[0])**2*np.exp(-(x[0]**2) - (x[1]+1)**2) \
   - 10*(x[0]/5 - x[0]**3 - x[1]**5)*np.exp(-x[0]**2-x[1]**2) \
   - 1/3*np.exp(-(x[0]+1)**2 - x[1]**2)

import scipy.optimize as opt
from scipy.optimize import basinhopping
res = basinhopping(Peaks2, x0=[0,0])
print (res)
```

```text
                        fun: -3.04984940280026
 lowest_optimization_result:       fun: -3.04984940280026
 hess_inv: array([[0.0704488 , 0.01699055],
       [0.01699055, 0.11034875]])
      jac: array([0.0000000e+00, 1.1920929e-07])
  message: 'Optimization terminated successfully.'
     nfev: 36
      nit: 6
     njev: 9
   status: 0
  success: True
        x: array([-1.34739625,  0.20451886])
                    message: ['requested number of basinhopping iterations completed successfully']
      minimization_failures: 0
                       nfev: 3732
                        nit: 100
                       njev: 933
                          x: array([-1.34739625,  0.20451886])
```












