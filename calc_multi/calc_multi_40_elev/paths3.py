from scipy.optimize import minimize, Bounds, SR1, BFGS
import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
from scipy.spatial.distance import cdist
from matplotlib import cm
import util

rho = 7.0
def sig(x,a):
   return (x-a)*1/(1+np.exp(-rho*(x-a)))

def trapz(y, dx):
    vals = y[1:-1]
    vals = vals[vals>0.0]
    return (y[0]+np.sum(vals*2.0)+y[-1])*(dx/2.0)

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
            g1 = 10.0 * np.exp( -4 *np.log(2) * ((x-x1)**2+(y-y1)**2) / s1**2)
            return g1
        ts = np.linspace(0.0,5.0,100)
        dzs = np.array([util._approx_fprime_helper([t],gfunc)[0] for t in ts])
        tmp = np.sqrt(1.0+(dzs**2.0))
        Iv = trapz(tmp, 5./100)
        dxs = np.array([util._approx_fprime_helper([t],sigx)[0] for t in ts])
        dys = np.array([util._approx_fprime_helper([t],sigy)[0] for t in ts])
        #print (dxs)
        #print (dys)        
        tmp = np.power(dxs,2) + np.power(dys,2)
        #print (tmp)
        tmp = tmp[tmp>0.0]
        tmp = np.sqrt(tmp)
        #print (tmp)
        Ih = trapz(tmp, 5./100)
        res = Iv*5.0 + Ih*1.0
        print (res)
        return res 
    
    LIM = 2.0

    a1,a2,a3 = 0.1,0.1,0.1
    b1,b2,b3 = 0.1,0.1,0.1
    x0 = a1,a2,a3,b1,b2,b3

    opts = {'maxiter': 300, 'verbose': 0}
    
    def con1(x):
        aa1,aa2,aa3,bb1,bb2,bb3 = x
        a = a0+aa1*(5.0-1.0)+aa2*(5.0-2.0)+aa3*(5.0-3.0)-ex
        return a
    
    def con2(x):
        aa1,aa2,aa3,bb1,bb2,bb3 = x
        b = b0+bb1*(5.0-1.0)+bb2*(5.0-2.0)+bb3*(5.0-3.0)-ey
        return b
    
    cons = [{'type':'eq', 'fun': con1}, {'type':'eq', 'fun': con2}]
    
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
ex,ey=(4.0,1.0)
res = find_path(ex,ey,a0,b0)
print  ('res',res)
print  ('res',res['x'])
