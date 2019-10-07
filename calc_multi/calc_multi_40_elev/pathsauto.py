import pandas as pd
import numpy as np
from mpl_toolkits.mplot3d import Axes3D
import matplotlib.pyplot as plt
from matplotlib import cm

OFFSET = 1.0
np.random.seed(0)

def func(x, y):
    s1 = 0.2; x1 = 36.5; y1 = 32.5
    s2 = 0.4; x2 = 36.1; y2 = 32.8
    g1 = np.exp( -4 *np.log(2) * ((x-x1)**2+(y-y1)**2) / s1**2)
    g2 = np.exp( -2 *np.log(2) * ((x-x2)**2+(y-y2)**2) / s2**2)    
    return g1 + g2 

D = 100

x = np.linspace(36,37,D)
y = np.linspace(32,33,D)

xx,yy = np.meshgrid(x,y)
zz = func(xx,yy)

from scipy.interpolate import Rbf

S = 50
np.random.seed(0)
idx = np.random.choice(range(D*D),S)
xr = xx.reshape(D*D)[idx].reshape(S,1)
yr = yy.reshape(D*D)[idx].reshape(S,1)
zr = zz.reshape(D*D)[idx].reshape(S,1)

rbfi = Rbf(xr,yr,zr,function='gaussian',epsilon=0.15)

from autograd import numpy as anp
import autograd

def dist_matrix(X, Y):
    sx = anp.sum(anp.power(X,2), 1)
    sy = anp.sum(anp.power(Y,2), 1)
    D2 =  sx[:, anp.newaxis] - 2.0*anp.dot(X,Y.T) + sy[anp.newaxis, :] 
    D = anp.sqrt(D2)
    return D

def gaussian(r,eps): return anp.exp(-anp.power((r/eps),2))

def f_interp(newp):    
    nodes = rbfi.nodes.reshape(1,len(rbfi.nodes))
    newp_dist = dist_matrix(newp, rbfi.xi.T)
    elev = anp.dot(gaussian(newp_dist, rbfi.epsilon), nodes.T)
    return elev

nodes = rbfi.nodes.reshape(1,len(rbfi.nodes))

def trapz(y, dx):
    vals = anp.array([_ if anp.isnan(_)==False else OFFSET for _ in y[1:-1]])
    tmp = anp.sum(vals*2.0)    
    return (y[0]+tmp+y[-1])*(dx/2.0)

t = np.linspace(0,1,100)
a0,b0=(36.0,32.0)
ex,ey=(36.4,32.8)

def intval(a1,a2,a3,b1,b2,b3):
   a4 = ex - a0 - (a1+a2+a3)
   b4 = ey - b0 - (b1+b2+b3)
   sq = anp.sqrt(b1 + 2*b2*t + 3*b3*t**2 - 112.0*t**3 + (a1 + 2*a2*t + 3*a3*t**2 - 65.2*t**3)**2)
   x = a0 + a1*t + a2*t**2 + a3*t**3 + a4*t**4 
   y = b0 + b1*t + b2*t**2 + b3*t**3 + b4*t**4
   z = [f_interp(anp.array([[xx,yy]]))[0][0] for xx,yy in zip(x,y)]
   res = z * sq
   T = trapz(res, 1.0/len(t))
   P = anp.power(a1,a1) + anp.power(a2,a2)  + anp.power(a3,a3) + anp.power(b1,b1) + anp.power(b2,b2)  + anp.power(b3,b3)
   T = T + P
   return T._value


intval_grad_a1 = autograd.grad(intval,0)
intval_grad_a2 = autograd.grad(intval,1)
intval_grad_a3 = autograd.grad(intval,2)
intval_grad_b1 = autograd.grad(intval,3)
intval_grad_b2 = autograd.grad(intval,4)
intval_grad_b3 = autograd.grad(intval,5)

DIV = 2.0
a1,a2,a3 = np.random.randn()/DIV,np.random.randn()/DIV,np.random.randn()/DIV
b1,b2,b3 = np.random.randn()/DIV,np.random.randn()/DIV,np.random.randn()/DIV

alpha = 0.1
newx = anp.array([a1,a2,a3,b1,b2,b3])

for i in range(3):
    oldx = newx
    a1,a2,a3,b1,b2,b3 = newx

    print (a1,a2,a3)
    
    a4 = ex - a0 - (a1+a2+a3)
    b4 = ey - b0 - (b1+b2+b3)

    fig = plt.figure()
    ax = fig.gca(projection='3d')
    ax.view_init(elev=29, azim=29)
    ax.set_xlim(36,37)
    ax.set_ylim(32,33)
    test_3 = anp.column_stack((xx.ravel(), yy.ravel()))
    znewnew = f_interp(test_3).reshape(xx.shape)
    surf = ax.plot_wireframe(xx, yy, znewnew, rstride=10, cstride=10)
    
    t = np.linspace(0,1,100)
    x = a0 + a1*t + a2*t**2 + a3*t**3 + a4*t**4 
    y = b0 + b1*t + b2*t**2 + b3*t**3 + b4*t**4
    z = [f_interp(anp.array([[xx,yy]]))[0][0] for xx,yy in zip(x,y)]
    ax.plot3D(x, y, z,'r.')
    plt.title(", ".join([str(np.round(xcurr,2)) for xcurr in newx]))
    plt.savefig('linear_app88rbf_08-%d.png' % i)

    grad_1 = [intval_grad_a1(a1,a2,a3,b1,b2,b3),\
              intval_grad_a2(a1,a2,a3,b1,b2,b3),\
              intval_grad_a3(a1,a2,a3,b1,b2,b3),\
              intval_grad_b1(a1,a2,a3,b1,b2,b3),\
              intval_grad_b2(a1,a2,a3,b1,b2,b3),\
              intval_grad_b3(a1,a2,a3,b1,b2,b3)]
    grad_1 = anp.array(grad_1)    
    newx = newx - alpha * grad_1
    print ('after update', newx, anp.abs(anp.sum(oldx-newx)))    
