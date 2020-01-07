from scipy.optimize import minimize, Bounds, SR1, BFGS
import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
from matplotlib import cm
import util

rho = 7.0
def sig(x,a):
   return (x-a)*1/(1+np.exp(-rho*(x-a)))

def trapz(y, dx):
    vals = y[1:-1]
    vals = vals[vals>0.0]
    return (y[0]+np.sum(vals*2.0)+y[-1])*(dx/2.0)

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
    
    ax.plot3D(xs, ys, gfunc(xs,ys),'r.')

 
def find_path(ex,ey,a0,b0):
    
    def calc_int(pars):
        a1,a2,a3,b1,b2,b3=pars
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
        
        def gfunc(t):
            t = t[0]
            x = sigx([t])	   
            y = sigy([t])
            s1 = 2.2; x1 = 2.0; y1 = 2.0
            g1 = np.exp( -4 *np.log(2) * ((x-x1)**2+(y-y1)**2) / s1**2)
            s2 = 1.2; x2 = 4.0; y2 = 1.0
            g2 = np.exp( -4 *np.log(2) * ((x-x2)**2+(y-y2)**2) / s2**2)
            return g1*10.0 + g2*10.0
         
        ts = np.linspace(0.0,5.0,100)
        dzs = np.array([util._approx_fprime_helper([t],gfunc)[0] for t in ts])
        tmp = np.sqrt(1.0+(dzs**2.0))
        Iv = trapz(tmp, 5./100)
        dxs = np.array([util._approx_fprime_helper([t],sigx)[0] for t in ts])
        dys = np.array([util._approx_fprime_helper([t],sigy)[0] for t in ts])
        tmp = np.power(dxs,2) + np.power(dys,2)
        tmp = tmp[tmp>0.0]
        tmp = np.sqrt(tmp)
        Ih = trapz(tmp, 5./100)
        res = Iv*5.0 + Ih*1.0
        #print (res)
        return res 
    
    LIM = 2.0

    a1,a2,a3,b1,b2,b3 = 0.1, 0.1, 0.1, 0.1, 0.1, 0.1
    x0 = a1,a2,a3,b1,b2,b3

    opts = {'maxiter': 50, 'verbose': 0}
    
    def conx(x):
        aa1,aa2,aa3,bb1,bb2,bb3 = x
        a = a0+aa1*(5.0-1.0)+aa2*(5.0-2.0)+aa3*(5.0-3.0)-ex
        return a
    
    def cony(x):
        aa1,aa2,aa3,bb1,bb2,bb3 = x
        b = b0+bb1*(5.0-1.0)+bb2*(5.0-2.0)+bb3*(5.0-3.0)-ey
        return b
    
    cons = [{'type':'eq', 'fun': conx}, {'type':'eq', 'fun': cony}]
    
    res = minimize (fun=calc_int,
                    x0=x0,
                    method='trust-constr',
                    hess = BFGS (),
                    bounds=Bounds([-LIM, -LIM, -LIM, -LIM, -LIM, -LIM],
                                  [ LIM,  LIM,  LIM,  LIM,  LIM,  LIM]),
                    constraints=cons,
                    options=opts)
    

    return res

a0,b0=(1.0,1.0)
ex,ey=(4.0,2.0)
res = find_path(ex,ey,a0,b0)
print  ('res',res)
print  ('res',res['x'])
