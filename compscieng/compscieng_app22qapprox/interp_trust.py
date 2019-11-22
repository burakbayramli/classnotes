from scipy.interpolate import Rbf
from mpl_toolkits.mplot3d import Axes3D
import matplotlib.pyplot as plt
from matplotlib import cm
import numpy as np
import itertools
import numpy.linalg as lin
import scipy.linalg as slin

from scipy.optimize import  minimize

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

def Grad_Rosenbrock(x):
    g1 = -400*x[0]*x[1] + 400*x[0]**3 + 2*x[0] -2
    g2 = 200*x[1] -200*x[0]**2
    return np.array([g1,g2])

def Hessian_Rosenbrock(x):
    h11 = -400*x[1] + 1200*x[0]**2 + 2
    h12 = -400 * x[0]
    h21 = -400 * x[0]
    h22 = 200
    return np.array([[h11,h12],[h21,h22]])

def get_fvals_in_region(xcurr, f, radius):
    b = random_ball(N, 2, radius)
    pts = xcurr+b
    vals = [f(p) for p in pts]
    return xcurr+b, np.array(vals)

def get_hess_grad_rosenbrock(xcurr):
    xs,vs = get_fvals_in_region(xcurr, rosenbrock, 1.0)
    res = []
    for i in range(vs.shape[0]):
        res.append((xs[i,0],xs[i,1],vs[i]))
    res = np.array(res).reshape(vs.shape[0], 3)
    xi = res[:,[0,1]]
    yi = res[:,[2]]
    xi = np.hstack((xi, np.ones((1,len(xi))).T  ))
    D = xi.shape[1]
    X_train = []
    for row in xi:
        X_train.append([row[i]*row[j] for i,j in itertools.product(range(D),range(D)) ])
    X_train = np.array(X_train)
    coef,_,_,_ = lin.lstsq(X_train, yi)
    coefs = coef.reshape(3,3)
    jac = (2 * np.dot(coefs[:2,:2],np.array(xcurr).reshape(2,1)))
    hess = 2*coefs[:2,:2]
    return jac, hess

def Hessian_interp(xcurr):
    jac, hess = get_hess_grad_rosenbrock(xcurr)
    hess2 = Hessian_Rosenbrock(xcurr)
    return hess

def Grad_interp(xcurr):
    jac, hess = get_hess_grad_rosenbrock(xcurr)
    return jac.flatten()


import warnings
warnings.simplefilter(action='ignore', category=FutureWarning)

x0 = [-1.0,0]
opts = {'maxiter': 5000, 'verbose': 1}
res = minimize (fun=rosenbrock,
                x0=x0,
                method = 'trust-constr',
                jac = "2-point",
                hess= Hessian_interp,
                options=opts)

print (res)
