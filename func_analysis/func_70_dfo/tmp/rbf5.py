from scipy.interpolate import Rbf
from mpl_toolkits.mplot3d import Axes3D
import matplotlib.pyplot as plt
from matplotlib import cm
import numpy.linalg as lin
import scipy.linalg as slin
import numpy as np, math
import autograd.numpy as anp
import autograd

def get_boundaries_intersections(z, d, trust_radius):
    a = np.dot(d, d)
    b = 2 * np.dot(z, d)
    c = np.dot(z, z) - trust_radius**2
    print (b,a,c)
    print (b*b - 4*a*c)
    sqrt_discriminant = math.sqrt(b*b - 4*a*c)
    aux = b + math.copysign(sqrt_discriminant, b)
    ta = -aux / (2*a)
    tb = -2*c / aux
    return sorted([ta, tb])

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

def cubic(r):
    return r**3

def rosenbrock(x):
    return (1 - x[0])**2 + 100*(x[1] - x[0]**2)**2

def Rosenbrock(x,y):
    return (1 - x)**2 + 100*(y - x**2)**2

def eval_model(xcurr, f, radius):
    b = random_ball(N, 2, radius)
    pts = xcurr+b
    vals = [f(p) for p in pts]
    xs,vs = pts, np.array(vals)
    res = [(xs[i,0],xs[i,1],vs[i]) for i in range(vs.shape[0])]
        
    res = anp.array(res).reshape(vs.shape[0], 3)
    #rbfi = Rbf(res[:,0],res[:,1],res[:,2],function='gaussian')
    rbfi = Rbf(res[:,0],res[:,1],res[:,2],function='cubic')
    def f_interp(xcurr):
        nodes = rbfi.nodes.reshape(1,len(rbfi.nodes))    
        newp_dist = dist_matrix(xcurr, rbfi.xi.T)
        #return anp.dot(gaussian(newp_dist, rbfi.epsilon), nodes.T)
        return anp.dot(cubic(newp_dist), nodes.T)

    val = f_interp(xcurr)
    jac = autograd.grad(f_interp)
    hess = autograd.hessian(f_interp)
    return val, jac(xcurr), hess(xcurr)
    
x0 = anp.array([-2.0,2.0])

np.random.seed(0)
N = 50
initial_trust_radius=1.0
trust_radius = initial_trust_radius
gtol = 1.0
alpha = 1.0
eta=0.15
max_trust_radius=1000.0
model_radius = 1.0

xcurr = x0
m = eval_model
f = rosenbrock
val, jac, hess = eval_model(xcurr, rosenbrock, model_radius)
print (val)
print (jac)
print (hess)
for i in range(40):
    print ('iteration', i)
    print ('norm jac', lin.norm(jac))
    if lin.norm(jac) < gtol: break
    x = np.linspace(-3,3,250)
    y = np.linspace(-3,3,250)
    X, Y = np.meshgrid(x, y)
    Z = Rosenbrock(X, Y)
    fig = plt.figure()
    ax = fig.gca()
    
    ax.contour(X,Y,Z, 50, cmap = 'jet')
    ax.plot(xcurr[0],xcurr[1], 'r.')
    circle=plt.Circle((xcurr[0],xcurr[1]),trust_radius,fill=False)
    ax.add_artist(circle)
    
    val, jac, hess = eval_model(xcurr, rosenbrock, model_radius)
    newton_dir = np.dot(-lin.inv(hess.reshape(2,2)),jac)
    #print ('cho',lin.det(hess[0][0]))
    #cho_info = slin.cho_factor(hess[0][0])
    #newton_dir = -slin.cho_solve(cho_info, jac)
    
    p_best = xcurr + newton_dir
    p_u = jac
    
    print ('p_u',p_u)
    p_u_norm = lin.norm(p_u) 
    if lin.norm(p_best) < trust_radius:
        hits_boundary,p=False,p_best
        print ('method 1',hits_boundary,p)       
    elif p_u_norm >= trust_radius:
        p_boundary = p_u * (trust_radius / p_u_norm)
        hits_boundary,p=True, xcurr-alpha*p_boundary
        print ('method 2',hits_boundary,p)
    else:        
        _, tb = get_boundaries_intersections(p_u, p_best - p_u,trust_radius)
        p_boundary = p_u + tb * (p_best - p_u)
        hits_boundary,p=True,p_boundary
        print ('method 3',tb)

    mv,dummy1,dummy2  = m(p,rosenbrock,trust_radius)
    model_prop_value = np.float(mv)
    mv,dummy1,dummy2  = m(xcurr,rosenbrock,trust_radius)
    model_curr_value = np.float(mv)

    real_prop_value = f(p)
    real_curr_value = f(xcurr)

    print ('model params',model_prop_value, model_curr_value, real_prop_value,real_curr_value)

    rho = (real_curr_value-real_prop_value) / (model_curr_value-model_prop_value)
    print ('rho',rho)
    if rho < 0.25:
        trust_radius *= 0.25
    elif rho > 0.75 and hits_boundary:
        print ('larger')
        trust_radius = min(2*trust_radius, max_trust_radius)
        
    if rho > eta:
        xcurr = p
    else:
        print ('do nothing')

    print ('xcurr',xcurr)
    print ('\n')
    ax.plot(xcurr[0],xcurr[1], 'rx')
    
    plt.savefig('/tmp/rbf/out-%d.png' % i)

