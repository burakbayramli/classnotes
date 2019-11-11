from scipy.interpolate import Rbf
from mpl_toolkits.mplot3d import Axes3D
import matplotlib.pyplot as plt
from matplotlib import cm
import numpy.linalg as lin
import numpy as np, math
import autograd.numpy as anp
import autograd

def random_ball(num_points, dimension, radius=1):
    from numpy import random, linalg
    random_directions = random.normal(size=(dimension,num_points))
    random_directions /= linalg.norm(random_directions, axis=0)
    random_radii = random.random(num_points) ** (1/dimension)
    return radius * (random_directions * random_radii).T

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

def norm(x):
    M = sqrt(abs(x).power(2).sum(axis=a))

def eval_model(xcurr, f, radius):
    b = random_ball(N, 2, radius)
    pts = xcurr+b
    vals = [f(p) for p in pts]
    xs,vs = pts, np.array(vals)
    res = []    
    for i in range(vs.shape[0]):
        res.append((xs[i,0],xs[i,1],vs[i]))
    res = anp.array(res).reshape(vs.shape[0], 3)
    rbfi = Rbf(res[:,0],res[:,1],res[:,2],function='gaussian')
    def f_interp(xcurr):
        nodes = rbfi.nodes.reshape(1,len(rbfi.nodes))    
        newp_dist = dist_matrix(xcurr, rbfi.xi.T)
        return anp.dot(gaussian(newp_dist, rbfi.epsilon), nodes.T)

    val = f_interp(xcurr)
    jac = autograd.grad(f_interp)
    hess = autograd.hessian(f_interp)
    return val, jac(xcurr), hess(xcurr)
    
x0 = anp.array([1.5,0])
print (eval_model(x0, rosenbrock, 1.0))

initial_trust_radius=5.0
trust_radius = initial_trust_radius
gtol = 1e-4
alpha = 0.1
eta=0.15

xcurr = x0
m = eval_model
f = rosenbrock
val, jac, hess = eval_model(xcurr, rosenbrock, trust_radius)
print (val)
print (jac)
print (hess)
print ('norm',lin.norm(jac))
while lin.norm(jac) >= gtol:
    val, jac, hess = eval_model(xcurr, rosenbrock, trust_radius)
    newton_dir = np.dot(-lin.inv(hess.reshape(2,2)),jac)
    p_best = xcurr + newton_dir
    p_u = xcurr + alpha*jac
    print ('p_u',p_u)
    p_u_norm = lin.norm(p_u)    
    if lin.norm(p_best) < trust_radius:
        hits_boundary,p=False,p_best
        print (hits_boundary,p)
    elif p_u_norm >= trust_radius:
        p_boundary = p_u * (trust_radius / p_u_norm)
        hits_boundary,p=True, p_boundary
        print (hits_boundary,p)
    else:        
        _, tb = get_boundaries_intersections(p_u, p_best - p_u,trust_radius)
        p_boundary = p_u + tb * (p_best - p_u)
        hits_boundary,p=True,p_boundary
        print (tb)

    mv,dummy1,dummy2  = m(p,rosenbrock,trust_radius)
    model_prop_value = np.float(mv)
    mv,dummy1,dummy2  = m(xcurr,rosenbrock,trust_radius)
    model_curr_value = np.float(mv)

    real_prop_value = f(p)
    real_curr_value = f(xcurr)

    print (model_prop_value, model_curr_value, real_prop_value,real_curr_value)

    rho = (real_curr_value-real_prop_value) / (model_curr_value-model_prop_value)
    print (rho)
    if rho < 0.25:
        trust_radius *= 0.25
    elif rho > 0.75 and hits_boundary:
        trust_radius = min(2*trust_radius, max_trust_radius)
        
    if rho > eta:
        xcurr = p

    print ('xcurr',xcurr)
        
    break
