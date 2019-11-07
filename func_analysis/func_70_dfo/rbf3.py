from scipy.interpolate import Rbf
from mpl_toolkits.mplot3d import Axes3D
import matplotlib.pyplot as plt
from matplotlib import cm
import numpy.linalg as lin
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

x0 = anp.array([1.5,0])
xs,vs = get_fvals_in_region(x0, rosenbrock, 0.5)

x = np.linspace(-2,2,250)
y = np.linspace(-1,3,250)
X, Y = np.meshgrid(x, y)
Z = Rosenbrock(X, Y)

fig = plt.figure()
ax = fig.gca(projection='3d')
res = []
for i in range(vs.shape[0]):
    res.append((xs[i,0],xs[i,1],vs[i]))
res = anp.array(res).reshape(vs.shape[0], 3)

rbfi = Rbf(res[:,0],res[:,1],res[:,2],function='gaussian',epsilon=0.15)

def f_interp(newp):
    nodes = rbfi.nodes.reshape(1,len(rbfi.nodes))    
    newp_dist = dist_matrix(newp, rbfi.xi.T)
    return anp.dot(gaussian(newp_dist, rbfi.epsilon), nodes.T)

print (f_interp(x0))
grbf = autograd.grad(f_interp)
hrbf = autograd.hessian(f_interp)
print ('grbf',grbf(x0))
print ('hrbf',hrbf(x0))

b0,b1=x0[0],x0[1]
g_dir = grbf(x0)
d = np.dot(-lin.inv(hrbf(x0).reshape(2,2)),g_dir)
print ('d',d)
g_dir = g_dir / np.sum(g_dir) / 4.0
print ('gdir',g_dir)

ax.quiver(b0, b1, 0, g_dir[0], g_dir[1], 1, color='red')
#ax.quiver(b0, b1, 0, d[0]*10, d[1]*10, 1, color='red')
ax.plot3D([b0], [b1], [0.0], 'b.')

ax.plot3D(res[:,0],res[:,1],res[:,2],'r.')
ax.plot_surface(X,Y,Z,rstride = 5, cstride = 5, cmap = 'jet', alpha = .4, edgecolor = 'none' )
#ax.view_init(21, -133)
ax.view_init(56, 79)
plt.show()
