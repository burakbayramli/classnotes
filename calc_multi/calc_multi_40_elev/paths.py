import pandas as pd
import numpy as np
from scipy import optimize
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
from scipy.spatial.distance import cdist
from matplotlib import cm

def trapz(y, dx):
    vals = y[1:-1]
    vals = vals[vals>0.0]
    return (y[0]+np.sum(vals*2.0)+y[-1])*(dx/2.0)

def gfunc(x, y, offset=0.0):
    s1 = 2.2; x1 = 2.0; y1 = 2.0
    g1 = np.exp( -4 *np.log(2) * ((x-x1)**2+(y-y1)**2) / s1**2)
    return g1+offset

def plot_surf_path(azim,elev,a0,a1,a2,a3,a4,b0,b1,b2,b3,b4):

    D = 50
    x = np.linspace(0,5,D)
    y = np.linspace(0,5,D)
    xx,yy = np.meshgrid(x,y)
    zz = gfunc(xx,yy)

    fig = plt.figure()
    ax = fig.gca(projection='3d')
    ax.set_xlim(0,5)
    ax.set_ylim(0,5)
    ax.view_init(elev=elev, azim=azim)
    surf = ax.plot_wireframe(xx, yy, zz,rstride=10, cstride=10)

    t = np.linspace(0,1.0,100)

    x = a0 + a1*t + a2*t**2 + a3*t**3 + a4*t**4 
    y = b0 + b1*t + b2*t**2 + b3*t**3 + b4*t**4

    ax.plot3D(x, y, gfunc(x,y),'r.')

def find_path(ex,ey,a0,b0,offset):
    
    cons=({'type': 'ineq','fun': lambda x: 4.0-x[0]}, # y<30
          {'type': 'ineq','fun': lambda x: 4.0-x[1]},
          {'type': 'ineq','fun': lambda x: 4.0-x[2]},
          {'type': 'ineq','fun': lambda x: 4.0-x[3]},
          {'type': 'ineq','fun': lambda x: 4.0-x[4]},
          {'type': 'ineq','fun': lambda x: 4.0-x[5]},
          {'type': 'ineq','fun': lambda x: x[0]}, # y>0
          {'type': 'ineq','fun': lambda x: x[1]},
          {'type': 'ineq','fun': lambda x: x[2]},
          {'type': 'ineq','fun': lambda x: x[3]},
          {'type': 'ineq','fun': lambda x: x[4]},
          {'type': 'ineq','fun': lambda x: x[5]},
    )


    # rasgele secilmis baslangic degerleri
    a1,a2,a3 = 0,0,0
    b1,b2,b3 = 0,0,0
    x0 = a1,a2,a3,b1,b2,b3

    def pintval(p):
        a1,a2,a3,b1,b2,b3 = p
        t = np.linspace(0,1.0,100)
        a4 = ex - a0 - (a1+a2+a3)
        b4 = ey - b0 - (b1+b2+b3)   
        sq = np.sqrt(b1 + 2*b2*t + 3*b3*t**2 - 112.0*t**3 + (a1 + 2*a2*t + 3*a3*t**2 - 65.2*t**3)**2)
        x = a0 + a1*t + a2*t**2 + a3*t**3 + a4*t**4 
        y = b0 + b1*t + b2*t**2 + b3*t**3 + b4*t**4
        x = np.array(x)
        y = np.array(y)   
        z = gfunc(x,y)   
        res = z * sq
        T = trapz(res, 1.0/len(t))
        return T
        
    sol = optimize.minimize(pintval,
                            x0,
                            method = 'COBYLA',
                            callback=print,
                            tol=0.001,
                            constraints=cons)

    print (sol.x)
    return sol.x
    
a0,b0=1.0,1.0
ex,ey=0.3,4.0
res = find_path(ex,ey,a0,b0,offset=0.5)
a1,a2,a3,b1,b2,b3 = res
a4 = ex - a0 - (a1+a2+a3)
b4 = ey - b0 - (b1+b2+b3)
print (a0,a1,a2,a3,a4,b0,b1,b2,b3,b4)
plot_surf_path(-124,58,a0,a1,a2,a3,a4,b0,b1,b2,b3,b4)
plt.savefig('calc_multi_40_elev_04.png')
#plt.show()

ex,ey=4.0,4.0
res = find_path(ex,ey,a0,b0,offset=0.5)
a1,a2,a3,b1,b2,b3 = res
a4 = ex - a0 - (a1+a2+a3)
b4 = ey - b0 - (b1+b2+b3)
print (a0,a1,a2,a3,a4,b0,b1,b2,b3,b4)
plot_surf_path(-124,58,a0,a1,a2,a3,a4,b0,b1,b2,b3,b4)
plt.savefig('calc_multi_40_elev_05.png')
#plt.show()


