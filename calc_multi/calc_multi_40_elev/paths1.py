import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
from scipy.spatial.distance import cdist
from matplotlib import cm
import util

def trapz(y, dx):
    vals = y[1:-1]
    vals = vals[vals>0.0]
    return (y[0]+np.sum(vals*2.0)+y[-1])*(dx/2.0)

def calc_int(pars):
    a0,a1,a2,a3,a4,b0,b1,b2,b3,b4=pars
    def gfunc(t):
        t = t[0]
        x = a0 + a1*t + a2*t**2 + a3*t**3 + a4*t**4 
        y = b0 + b1*t + b2*t**2 + b3*t**3 + b4*t**4
        s1 = 2.2; x1 = 2.0; y1 = 2.0
        g1 = np.exp( -4 *np.log(2) * ((x-x1)**2+(y-y1)**2) / s1**2)
        return g1*10.0
    
    ts = np.linspace(0,1.0,100)
    # for z 
    dzs = np.array([util._approx_fprime_helper([t],gfunc)[0] for t in ts])
    tmp = np.sqrt(1+(dzs**2))
    Iv = trapz(tmp, 1/100.)
    #print (Iv)
    tmp = np.array([b1 + 2*b2*t + 3*b3*t**2 - 112.0*t**3 + (a1 + 2*a2*t + 3*a3*t**2 - 65.2*t**3)**2 for t in ts])
    tmp = tmp[tmp>0.0]
    print (tmp)
    tmp = np.sqrt(tmp)
    Ih = trapz(tmp, 1/100.)
    return Iv*5 + Ih*1
            

# 1. gidis yolunun tanimi, uzun yoldan dolanarak gidiyor
#a1,a2,a3 = 1.5, 8.1, 4.0
#b1,b2,b3 = 0.3, 0.4, 23.3
#a1,a2,a3 = 1.5, 3.0, 1.0
#b1,b2,b3 = 0.0, 1.0, 1.0
a1,a2,a3 = 0.5, 0.1, 0.0
b1,b2,b3 = -0.3, -1.4, 1.3
a0,b0=(1.0,1.0)
ex,ey=(0.3,4.0)
a4 = ex - a0 - (a1+a2+a3)
b4 = ey - b0 - (b1+b2+b3)
test_coefs1 = (a0,a1,a2,a3,a4,b0,b1,b2,b3,b4)
print (calc_int(test_coefs1))
