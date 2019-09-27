import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
from scipy.spatial.distance import cdist
from matplotlib import cm

a0,b0=1.0,1.0
ex,ey=(0.3,4.0)
#ex,ey=(4.0,4.0)

def gfunc(x, y):
    s1 = 2.2; x1 = 2.0; y1 = 2.0
    g1 = np.exp( -4 *np.log(2) * ((x-x1)**2+(y-y1)**2) / s1**2)
    return g1 

def plot_surf_path(a0,a1,a2,a3,a4,b0,b1,b2,b3,b4):

    D = 50
    x = np.linspace(0,5,D)
    y = np.linspace(0,5,D)
    xx,yy = np.meshgrid(x,y)
    zz = gfunc(xx,yy)

    fig = plt.figure()
    ax = fig.gca(projection='3d')
    ax.set_xlim(0,5)
    ax.set_ylim(0,5)
    surf = ax.plot_wireframe(xx, yy, zz,rstride=10, cstride=10)

    t = np.linspace(0,1.0,100)

    x = a0 + a1*t + a2*t**2 + a3*t**3 + a4*t**4 
    y = b0 + b1*t + b2*t**2 + b3*t**3 + b4*t**4

    ax.plot3D(x, y, gfunc(x,y),'r.')
    
(a1,a2,a3,b1,b2,b3) = (-1.13290130e-16,  5.22791479e-11,  8.12783700e+00, -2.29279835e-10,
        2.31603917e+00,  1.66561786e+00)
#(a1,a2,a3,b1,b2,b3) = (4.17443857e-14, 5.67750798e+00, 9.19625760e+00, 1.36988150e-14,       1.41137148e-14, 8.40236500e-15)
a4 = ex - a0 - (a1+a2+a3)
b4 = ey - b0 - (b1+b2+b3)
plot_surf_path(a0,a1,a2,a3,a4,b0,b1,b2,b3,b4)

plt.show()
