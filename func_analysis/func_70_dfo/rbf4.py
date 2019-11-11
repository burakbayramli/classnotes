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

def norm(x):
    M = sqrt(abs(x).power(2).sum(axis=a))

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

rbfi = Rbf(res[:,0],res[:,1],res[:,2],function='gaussian')

def f_interp(newp):
    nodes = rbfi.nodes.reshape(1,len(rbfi.nodes))    
    newp_dist = dist_matrix(newp, rbfi.xi.T)
    return anp.dot(gaussian(newp_dist, rbfi.epsilon), nodes.T)

def get_boundaries_intersections(z, d, trust_radius):
    """
    Solve the scalar quadratic equation ||z + t d|| == trust_radius.
    This is like a line-sphere intersection.
    Return the two values of t, sorted from low to high.
    """
    a = np.dot(d, d)
    b = 2 * np.dot(z, d)
    c = np.dot(z, z) - trust_radius**2
    print (b,a,c)
    print (b*b - 4*a*c)
    sqrt_discriminant = math.sqrt(b*b - 4*a*c)

    # The following calculation is mathematically
    # equivalent to:
    # ta = (-b - sqrt_discriminant) / (2*a)
    # tb = (-b + sqrt_discriminant) / (2*a)
    # but produce smaller round off errors.
    # Look at Matrix Computation p.97
    # for a better justification.
    aux = b + math.copysign(sqrt_discriminant, b)
    ta = -aux / (2*a)
    tb = -2*c / aux
    return sorted([ta, tb])

xcurr = x0
initial_trust_radius=5.0
trust_radius = initial_trust_radius
gtol = 1e-4
alpha = 0.1
grbf = autograd.grad(f_interp)
jac = grbf(xcurr)
print (jac)
while lin.norm(jac) >= gtol:
    hits_boundary,p_next=None,None
    print (f_interp(xcurr))
    grbf = autograd.grad(f_interp)
    hrbf = autograd.hessian(f_interp)
    print ('grbf',grbf(xcurr))
    print ('hrbf',hrbf(xcurr))

    b0,b1=xcurr[0],xcurr[1]
    jac = grbf(xcurr)
    hess = hrbf(xcurr)
    newton_dir = np.dot(-lin.inv(hess.reshape(2,2)),jac)
    p_best = xcurr + newton_dir
    p_u = xcurr + alpha*jac
    print ('p_u',p_u)
    p_u_norm = lin.norm(p_u)    
    if lin.norm(p_best) < trust_radius:
        hits_boundary,p_next=False,p_best
        print (hits_boundary,p_next)        
    elif p_u_norm >= trust_radius:
        p_boundary = p_u * (trust_radius / p_u_norm)
        hits_boundary,p_next=True, p_boundary
        print (hits_boundary,p_next)
    else:        
        _, tb = get_boundaries_intersections(p_u, p_best - p_u,trust_radius)
        p_boundary = p_u + tb * (p_best - p_u)
        hits_boundary,p_next=True,p_boundary
        print (tb)
    
    break
