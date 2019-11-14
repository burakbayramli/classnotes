#+LaTeX_HEADER: \newcommand{\ud}{\,\mathrm{d}}
#+LaTeX_HEADER: \newcommand{\mlabel}[1]{\quad \text{(#1)}\quad}
#+LaTeX_HEADER: \usepackage{palatino,eulervm}
#+LaTeX_HEADER: \usepackage{cancel}

```python
from scipy.interpolate import Rbf
from mpl_toolkits.mplot3d import Axes3D
import matplotlib.pyplot as plt
from matplotlib import cm
import numpy as np
import autograd.numpy as anp
import autograd

def random_ball(num_points, dimension, radius=1):
    from numpy import random, linalg
    random_directions = random.normal(size=(dimension,num_points))
    random_directions /= linalg.norm(random_directions, axis=0)
    random_radii = random.random(num_points) ** (1/dimension)
    return radius * (random_directions * random_radii).T

def func(x, y):
    s1 = 0.2; x1 = 36.5; y1 = 32.5
    s2 = 0.4; x2 = 36.1; y2 = 32.8
    g1 = np.exp( -4 *np.log(2) * ((x-x1)**2+(y-y1)**2) / s1**2)
    g2 = np.exp( -2 *np.log(2) * ((x-x2)**2+(y-y2)**2) / s2**2)    
    return g1 + g2 

def dist_matrix(X, Y):
    X = X.reshape(1, X.shape[0])
    sx = anp.sum(X**2, 1)
    sy = anp.sum(Y**2, 1)
    D2 =  sx[:, anp.newaxis] - 2.0*anp.dot(X,Y.T) + sy[anp.newaxis, :] 
    D = anp.sqrt(D2)
    return D

def gaussian(r,eps):
    return anp.exp(-(r/eps)**2)

np.random.seed(0)
N = 20

def rosenbrock(x):
    return (1 + x[0])**2 + 100*(x[1] - x[0]**2)**2

def Rosenbrock(x,y):
    return (1 + x)**2 + 100*(y - x**2)**2

def get_fvals_in_region(xcurr, f, radius):    
    b = random_ball(N, 2, radius)
    pts = xcurr+b
    vals = [f(p) for p in pts]
    return xcurr+b, np.array(vals)

xs,vs = get_fvals_in_region([1.5,0], rosenbrock, 0.5)

res = []
for i in range(vs.shape[0]):
    res.append((xs[i,0],xs[i,1],vs[i]))
res = np.array(res).reshape(vs.shape[0], 3)
```

```python
import itertools
import numpy.linalg as lin

def quad_interpolate(xi, yi):
    xi = np.hstack((xi, np.ones((1,len(xi))).T  ))
    #print (xi)
    D = xi.shape[1]
    print (D)
    X_train = []
    for row in xi:
#        print (row)
        X_train.append([row[i]*row[j] for i,j in itertools.product(range(D),range(D)) ])
    X_train = np.array(X_train)
    print (X_train.shape)
    print (yi.shape)
    coef,_,_,_ = lin.lstsq(X_train, yi)
    return coef

xi = res[:,[0,1]]
yi = res[:,[2]]
fit_q = quad_interpolate(xi,yi)
#print (fit_q)
```

```text
3
(20, 9)
(20, 1)
```

```python
x = np.linspace(-2,2,250)
y = np.linspace(-1,3,250)
X, Y = np.meshgrid(x, y)
Z = Rosenbrock(X, Y)

fig = plt.figure(figsize = (8,4))
ax = fig.gca(projection='3d')
ax.plot3D(res[:,0],res[:,1],res[:,2],'r.')
ax.plot_surface(X,Y,Z,rstride = 5, cstride = 5, cmap = 'jet', alpha = .4, edgecolor = 'none' )
ax.view_init(21, -133)

plt.savefig('/tmp/inter_01.png')
```








```python
def f(x1,x2):
    x = np.array([[x1,x2]])
    A = np.array([[2,3],[4,3]])
    b = np.array( [[1,1]] )
    a = 4
    res = np.dot(np.dot(x,A),x.T) + np.dot(b,x.T) + a
    return np.float(res)
    
x = np.linspace(-2,2,250)
y = np.linspace(-2,2,250)
X, Y = np.meshgrid(x, y)
Z = np.array([f(xx,yy) for xx,yy in zip(X.flatten(),Y.flatten())])
Z = Z.reshape(X.shape)

from mpl_toolkits.mplot3d import Axes3D
fig = plt.figure(figsize = (8,4))
ax = fig.gca(projection='3d')
ax.set_zlim(0,50)
ax.plot_surface(X,Y,Z,rstride = 5, cstride = 5, cmap = 'jet', alpha = .4, edgecolor = 'none' )
plt.savefig('/tmp/inter_01.png')
```








