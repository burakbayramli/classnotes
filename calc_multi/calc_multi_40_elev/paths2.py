import numpy as np, numpy.linalg as lin
from mpl_toolkits.mplot3d import Axes3D
import matplotlib.pyplot as plt
from matplotlib import cm

OFFSET = 0.0
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


DIV = 2.0
alpha = 0.1
LIM = 2.0
mu = 2.0
a1,a2,a3 = np.random.randn()/DIV,np.random.randn()/DIV,np.random.randn()/DIV
b1,b2,b3 = np.random.randn()/DIV,np.random.randn()/DIV,np.random.randn()/DIV
newx = anp.array([a1,a2,a3,b1,b2,b3])

for i in range(5):
    t = np.linspace(0,1,100)
    a0,b0=(36.0,32.0)
    ex,ey=(36.4,32.8)

    def obj(xarg):
        a1,a2,a3,b1,b2,b3=xarg[0],xarg[1],xarg[2],xarg[3],xarg[4],xarg[5]
        a4 = ex - a0 - (a1+a2+a3)
        b4 = ey - b0 - (b1+b2+b3)
        sq = anp.sqrt(b1 + 2*b2*t + 3*b3*t**2 - 112.0*t**3 + (a1 + 2*a2*t + 3*a3*t**2 - 65.2*t**3)**2)
        x = a0 + a1*t + a2*t**2 + a3*t**3 + a4*t**4 
        y = b0 + b1*t + b2*t**2 + b3*t**3 + b4*t**4
        z = [f_interp(anp.array([[xx,yy]]))[0][0] for xx,yy in zip(x,y)]
        res = z * sq
        T = trapz(res, 1.0/len(t))        
        cons = mu * (anp.log(LIM+a1) + anp.log(LIM-a1) + \
                     anp.log(LIM+a2) + anp.log(LIM-a2) + \
                     anp.log(LIM+a3) + anp.log(LIM-a3) + \
                     anp.log(LIM+b1) + anp.log(LIM-b1) + \
                     anp.log(LIM+b2) + anp.log(LIM-b2) + \
                     anp.log(LIM+b3) + anp.log(LIM-b3))
        print ('as bs',xarg)
        print ('cons',cons)
        T = T - cons
        print ('T',T)
        if ('ArrayBox' not in str(type(T))):
            return float(T)
        return T._value
    
    a1,a2,a3,b1,b2,b3=newx[0],newx[1],newx[2],newx[3],newx[4],newx[5]
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
    #plt.savefig('/tmp/out.png')
    plt.show()
    
    print ('newx',newx)
    print (obj(newx))
    
    # h = autograd.hessian(obj)
    # H = h(newx)
    # j = autograd.jacobian(obj)
    # J = j(newx)
    # if lin.det(H)==0.0:
    #     H = H + np.eye(6,6)*1e-2
    # print (H)
    # d = np.dot(-lin.inv(H), J)
    # mu = mu * 0.1
    # newx = newx + d

    j = autograd.jacobian(obj)
    J = j(newx)
    print (J)
    d = J
    print (d)    
    newx = newx + alpha*d

