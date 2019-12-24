import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
from scipy.spatial.distance import cdist
from matplotlib import cm

epsilon = np.sqrt(np.finfo(float).eps)

def trapz(y, dx):
    vals = y[1:-1]
    vals = vals[vals>0.0]
    return (y[0]+np.sum(vals*2.0)+y[-1])*(dx/2.0)

def _approx_fprime_helper(xk, f):
    f0 = f(xk)
    grad = np.zeros((len(xk),), float)
    ei = np.zeros((len(xk),), float)
    for k in range(len(xk)):
        ei[k] = 1.0
        d = epsilon * ei
        df = (f(xk + d) - f0) / d[k]
        if not np.isscalar(df):
            try:
                df = df.item()
            except (ValueError, AttributeError):
                raise ValueError("The user-provided "
                                 "objective function must "
                                 "return a scalar value.")
        grad[k] = df
        ei[k] = 0.0
    return grad

def gfunc(x):
    t = x[0]
    x = a0 + a1*t + a2*t**2 + a3*t**3 + a4*t**4 
    y = b0 + b1*t + b2*t**2 + b3*t**3 + b4*t**4
    s1 = 2.2; x1 = 2.0; y1 = 2.0
    g1 = np.exp( -4 *np.log(2) * ((x-x1)**2+(y-y1)**2) / s1**2)
    return g1

def calc_int(pars):
    pars = a0,a1,a2,a3,a4,b0,b1,b2,b3,b4
    ts = np.linspace(0,1.0,100)
    # for z 
    dzs = np.array([_approx_fprime_helper([t],gfunc)[0] for t in ts])
    tmp = 1 + np.sqrt(1+dzs**2)
    Iv = trapz(tmp, 1/100.)
    print (Iv)
    tmp = np.array([np.sqrt(b1 + 2*b2*t + 3*b3*t**2 - 112.0*t**3 + (a1 + 2*a2*t + 3*a3*t**2 - 65.2*t**3)**2) for t in ts])
    Ih = trapz(tmp, 1/100.)
    return Iv*5 + Ih*1
        
    
def test_fprime():
    rosen = lambda x: (1-x[0])**2 + 100*(x[1]-x[0]**2)**2
    def rosen_d(x):
        return np.array([2*100*(x[1] - x[0]**2)*(-2*x[0]) - 2*(1.-x[0]), 2*100*(x[1]-x[0]**2)])
    x = [0.5, 0.5]
    res = _approx_fprime_helper(x, rosen)
    print ('analitik', res, 'sayisal', rosen_d(x))    
    

# 1. gidis yolunun tanimi, uzun yoldan dolanarak gidiyor
#a1,a2,a3 = 1.5, 8.1, 4.0
#b1,b2,b3 = 0.3, 0.4, 23.3
a1,a2,a3 = 1.5, 3.0, 1.0
b1,b2,b3 = 0.0, 1.0, 1.0
a0,b0=(1.0,1.0)
ex,ey=(0.3,4.0)
a4 = ex - a0 - (a1+a2+a3)
b4 = ey - b0 - (b1+b2+b3)
test_coefs1 = (a0,a1,a2,a3,a4,b0,b1,b2,b3,b4)
print (calc_int(test_coefs1))
test_fprime()
