import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
from scipy.spatial.distance import cdist
from matplotlib import cm

rho = 7.0
def sig(x,a):
   return (x-a)*1/(1+np.exp(-rho*(x-a)))

def gfunc(x, y):
    s1 = 2.2; x1 = 2.0; y1 = 2.0
    g1 = np.exp( -4 *np.log(2) * ((x-x1)**2+(y-y1)**2) / s1**2)
    s2 = 1.2; x2 = 4.0; y2 = 1.0
    g2 = np.exp( -4 *np.log(2) * ((x-x2)**2+(y-y2)**2) / s2**2)
    return g1*10.0 + g2*10.0

def plot_surf_path(a0,a1,a2,a3,b0,b1,b2,b3):

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

    t = np.linspace(0,5.0,100)

    def sigx(t):
        t = t[0]
        x = a0 + \
            a1*sig(t,1) + \
            a2*sig(t,2) + \
            a3*sig(t,3)
        return x
    
    def sigy(t):
        t = t[0]
        y = b0 + \
            b1*sig(t,1) + \
            b2*sig(t,2) + \
            b3*sig(t,3)
        return y


    xs = np.array([sigx([tt]) for tt in t])
    ys = np.array([sigy([tt]) for tt in t])

    ax.view_init(elev=45, azim=-113)
    ax.plot3D(xs, ys, gfunc(xs,ys),'r.')

a1,a2,a3,b1,b2,b3=0.54187919,  0.15991569,  0.17636809, -0.20027283,  0.26755009,  0.49922053
a0,b0=(1.0,1.0)
plot_surf_path(a0,a1,a2,a3,b0,b1,b2,b3)
#plt.show()
plt.savefig('calc_multi_40_elev_06.png')
# -113,45
