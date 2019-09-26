from autograd import numpy as np
from scipy import optimize
import autograd

def intval(t,a0,a1,a2,a3,a4,b0,b1,b2,b3,b4):
   sq = np.sqrt(b1 + 2*b2*t + 3*b3*t**2 - 112.0*t**3 + (a1 + 2*a2*t + 3*a3*t**2 - 65.2*t**3)**2)
   x = a0 + a1*t + a2*t**2 + a3*t**3 + a4*t**4 
   y = b0 + b1*t + b2*t**2 + b3*t**3 + b4*t**4
   x = np.array(x)
   y = np.array(y)   
   z = gfunc(x,y)   
   res = z * sq
   T = trapz(res, 1.0/len(t))
   return T

def pintval(p):
    a0,a1,a2,a3,a4,b0,b1,b2,b3,b4 = p
    t = np.linspace(0,1,100)
    intval(t,a0,a1,a2,a3,a4,b0,b1,b2,b3,b4)


