from autograd import numpy as np
from autograd.numpy import sqrt
from autograd.numpy import exp
from autograd.numpy import nan
from autograd.numpy import abs
from autograd.numpy import e
from autograd.numpy import log
from autograd.numpy import power
from autograd.numpy import sum
from scipy import optimize
import autograd
import numpy as np

def trapz(y, dx):
    vals = y[1:-1]
    vals = vals[vals>0.0]
    return (y[0]+sum(vals*2.0)+y[-1])*(dx/2.0)

def gfunc(x, y):
    s1 = 2.2; x1 = 2.0; y1 = 2.0
    tmp = -4.0 *log(2.0) * ((x-x1)**2.0+(y-y1)**2.0) / s1**2.0
    g1 = np.array([power(e,_) if _ != nan else 0.0 for _ in tmp])
    #for x in g1: print (x)
    #g1 = power(e,tmp)
    return g1 

def pintval(p):
   a0,b0=1.0,1.0
   a1,a2,a3,b1,b2,b3 = p
   ex,ey=(0.3,4.0)
   a4 = ex - a0 - (a1+a2+a3)
   b4 = ey - b0 - (b1+b2+b3)   
   t = np.linspace(0,1,100)
   tmp = b1 + 2.0*b2*t + 3.0*b3*t**2.0 - 112.0*t**3.0 + (a1 + 2.0*a2*t + 3.0*a3*t**2.0 - 65.2*t**3.0)**2.0
   sq = [sqrt(_) if _ != nan else 0.0 for _ in tmp]
   #sq = [sqrt(x) for x in arr]
   x = a0 + a1*t + a2*t**2.0 + a3*t**3.0 + a4*t**4.0
   y = b0 + b1*t + b2*t**2.0 + b3*t**3.0 + b4*t**4.0
   x = np.array(x)
   y = np.array(y)   
   z = gfunc(x,y)   
   res = z * sq
   #for x in res: print (x)
   T = trapz(res, 1.0/len(t))
   print ('T',T)
   return T

a1,a2,a3 = 1.0,1.0,1.0
b1,b2,b3 = 1.0,1.0,1.0
x0 = a1,a2,a3,b1,b2,b3

pintval_grad = autograd.grad(pintval)

print (pintval_grad(x0))

cons=({'type': 'ineq','fun': lambda x: x[0]**x[0]-400},
      {'type': 'ineq','fun': lambda x: x[1]**x[1]-400},
      {'type': 'ineq','fun': lambda x: x[2]**x[2]-400},
      {'type': 'ineq','fun': lambda x: x[3]**x[3]-400},
      {'type': 'ineq','fun': lambda x: x[4]**x[4]-400},
      {'type': 'ineq','fun': lambda x: x[5]**x[5]-400},
      {'type': 'ineq','fun': lambda x: x[6]**x[6]-400},
      {'type': 'ineq','fun': lambda x: x[0]},
      {'type': 'ineq','fun': lambda x: x[1]},
      {'type': 'ineq','fun': lambda x: x[2]},
      {'type': 'ineq','fun': lambda x: x[3]},
      {'type': 'ineq','fun': lambda x: x[4]},
      {'type': 'ineq','fun': lambda x: x[5]},
      {'type': 'ineq','fun': lambda x: x[6]}      
      )
      

sol = optimize.minimize(pintval,
                        x0,
                        jac = pintval_grad,
                        method = 'BFGS',
                        #method = 'Newton-CG',
                        callback=print,
                        tol=0.5,
                        constraints=cons)

print (sol)

#cons=({'type': 'ineq',
#       'fun': lambda x: x[0]**3 - x[1]**2 - 1},
#      {'type': 'ineq',
#       'fun': lambda x: x[0]},
#      {'type': 'ineq',
#       'fun': lambda x: x[2]})
# https://stackoverflow.com/questions/35432478/scipy-minimize-constrained-function
