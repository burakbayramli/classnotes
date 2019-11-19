from scipy.interpolate import Rbf
from mpl_toolkits.mplot3d import Axes3D
import matplotlib.pyplot as plt
from matplotlib import cm
import numpy as np
import itertools
import numpy.linalg as lin
import scipy.linalg as slin

np.random.seed(0)
N = 20

def random_ball(num_points, dimension, radius):
    from numpy import random, linalg
    random_directions = random.normal(size=(dimension,num_points))
    random_directions /= linalg.norm(random_directions, axis=0)
    random_radii = random.random(num_points) ** (1/dimension)
    return radius * (random_directions * random_radii).T

def rosenbrock(x):
    return (1 - x[0])**2 + 100*(x[1] - x[0]**2)**2

def Rosenbrock(x,y):
    return (1 - x)**2 + 100*(y - x**2)**2

def Grad_Rosenbrock(x,y):
    g1 = -400*x*y + 400*x**3 + 2*x -2
    g2 = 200*y -200*x**2
    return np.array([g1,g2])

def Hessian_Rosenbrock(x,y):
    h11 = -400*y + 1200*x**2 + 2
    h12 = -400 * x
    h21 = -400 * x
    h22 = 200
    return np.array([[h11,h12],[h21,h22]])

def get_fvals_in_region(xcurr, f, radius):    
    b = random_ball(N, 2, radius)
    pts = xcurr+b
    vals = [f(p) for p in pts]
    return xcurr+b, np.array(vals)

#x0 = [1.5,0]
#x0 = [-1.0,0]
x0 = [-1.0,2.0]
xs,vs = get_fvals_in_region(x0, rosenbrock, 3.0)

res = []
for i in range(vs.shape[0]):
    res.append((xs[i,0],xs[i,1],vs[i]))
res = np.array(res).reshape(vs.shape[0], 3)


def quad_interpolate(xi, yi):
    xi = np.hstack((xi, np.ones((1,len(xi))).T  ))
    #print (xi)
    D = xi.shape[1]
    print (D)
    X_train = []
    for row in xi:
        X_train.append([row[i]*row[j] for i,j in itertools.product(range(D),range(D)) ])
    X_train = np.array(X_train)
    print (X_train.shape)
    print (yi.shape)
    coef,_,_,_ = lin.lstsq(X_train, yi)
    return coef

xi = res[:,[0,1]]
yi = res[:,[2]]
coef = quad_interpolate(xi,yi)

x = np.linspace(-2,2,250)
y = np.linspace(-1,3,250)
X, Y = np.meshgrid(x, y)
Z = Rosenbrock(X, Y)

fig = plt.figure(figsize = (8,4))
ax = fig.gca(projection='3d')
ax.plot3D(res[:,0],res[:,1],res[:,2],'r.')
ax.plot_surface(X,Y,Z,rstride = 5, cstride = 5, cmap = 'jet', alpha = .5, edgecolor = 'none' )

def q_interp(x1,x2):
    x = np.array([[x1,x2,1]])
    A = coef.reshape(3,3)
    res = np.dot(np.dot(x,A),x.T)
    return np.float(res)

Zi = np.array([q_interp(xx,yy) for xx,yy in zip(X.flatten(),Y.flatten())])
Zi = Zi.reshape(X.shape)
ax.plot_wireframe(X,Y,Zi,rstride=20, cstride=20)

coefs = coef.reshape(3,3)

g = (2 * np.dot(coefs[:2,:2],np.array(x0).reshape(2,1)))
g = g.flatten() / 2.0

g2 = Grad_Rosenbrock(x0[0],x0[1])

print ('g',g)
print ('g2',g2)

g_descent = -(g / np.sum(g))
g_descent2 = -(g2 / np.sum(g2))

ax.set_zlim(0,2500)

ax.quiver(x0[0], x0[1], 0, g_descent[0], g_descent[1], 0, color='red')

hess = 2*coefs[:2,:2]
print ('hess',hess)
print ('sing hess', lin.det(hess))

hess2 = Hessian_Rosenbrock(x0[0],x0[1])

print ('real hess', hess2)
print ('sing hess2', lin.det(hess2))

newton_dir = -np.dot(lin.inv(hess),g)
#newton_dir = -slin.cho_solve(slin.cho_factor(hess), g)
print ('newton dir',newton_dir)

newton_dir2 = -np.dot(lin.inv(hess2),g2)
#newton_dir2 = -slin.cho_solve(slin.cho_factor(hess2), g2)
print ('newton dir2',newton_dir2)


ax.quiver(x0[0], x0[1], 0, newton_dir[0], newton_dir[1], 0, color='green')

ax.plot3D([x0[0]], [x0[1]], [0.0], 'b.')

ax.view_init(21, -133)

plt.show()
