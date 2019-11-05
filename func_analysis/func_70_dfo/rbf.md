#+LaTeX_HEADER: \newcommand{\ud}{\,\mathrm{d}}
#+LaTeX_HEADER: \newcommand{\mlabel}[1]{\quad \text{(#1)}\quad}
#+LaTeX_HEADER: \usepackage{palatino,eulervm}
#+LaTeX_HEADER: \usepackage{cancel}

```python
def random_ball(num_points, dimension, radius=1):
    from numpy import random, linalg
    random_directions = random.normal(size=(dimension,num_points))
    random_directions /= linalg.norm(random_directions, axis=0)
    random_radii = random.random(num_points) ** (1/dimension)
    return radius * (random_directions * random_radii).T
    
ps = random_ball(400, 2, 1)
center = np.array([0,1])
ps = center + ps

plt.plot(ps[:,0],ps[:,1],'.')
plt.xlim(-3,3)
plt.ylim(-3,3)
plt.savefig('rb_01.png')
```

```python
from mpl_toolkits.mplot3d import Axes3D
import matplotlib.pyplot as plt
from matplotlib import cm

np.random.seed(0)

def func(x, y):
    s1 = 0.2; x1 = 36.5; y1 = 32.5
    s2 = 0.4; x2 = 36.1; y2 = 32.8
    g1 = np.exp( -4 *np.log(2) * ((x-x1)**2+(y-y1)**2) / s1**2)
    g2 = np.exp( -2 *np.log(2) * ((x-x2)**2+(y-y2)**2) / s2**2)    
    return g1 + g2 

D = 100

x = np.linspace(36,37,D)
y = np.linspace(32,33,D)

xx,yy = np.meshgrid(x,y)
zz = func(xx,yy)

xx = xx.reshape(D,D)
yy = yy.reshape(D,D)
zz = func(xx,yy)

from scipy.interpolate import Rbf

S = 5
np.random.seed(0)
idx = np.random.choice(range(D*D),S)
xr = xx.reshape(D*D)[idx].reshape(S,1)
yr = yy.reshape(D*D)[idx].reshape(S,1)
zr = zz.reshape(D*D)[idx].reshape(S,1)

rbfi = Rbf(xr,yr,zr,function='gaussian',epsilon=0.15)
```


```python
def dist_matrix(X, Y):
    X = X.reshape(1, X.shape[0])
    sx = anp.sum(X**2, 1)
    sy = anp.sum(Y**2, 1)
    D2 =  sx[:, anp.newaxis] - 2.0*anp.dot(X,Y.T) + sy[anp.newaxis, :] 
    D = anp.sqrt(D2)
    return D
    
nodes = rbfi.nodes.reshape(1,len(rbfi.nodes))
def gaussian(r,eps): return anp.exp(-(r/eps)**2)

def f_interp(newp):
    nodes = rbfi.nodes.reshape(1,len(rbfi.nodes))    
    newp_dist = dist_matrix(newp, rbfi.xi.T)
    return anp.dot(gaussian(newp_dist, rbfi.epsilon), nodes.T)

test_1 = anp.array([36.0,32.0])
test_1_dist = dist_matrix(test_1, rbfi.xi.T)
print (test_1_dist.shape)
print (test_1_dist[0][:10])

print (f_interp(test_1))

grbf = autograd.grad(f_interp)
hrbf = autograd.hessian(f_interp)
print (grbf(test_1))
print (hrbf(test_1))
```

```text
(1, 5)
[0.4229176  1.08927112 0.72276945 0.76827462 0.96299239]
[[2.1076464e-05]]
[0.00060556 0.00051094]
[[[[0.01552543 0.01468033]
   [0.01468033 0.01051307]]]]
```







