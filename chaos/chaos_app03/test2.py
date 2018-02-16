import pandas as pd
import numpy as np
import scipy as sp
from scipy.integrate.odepack import odeint
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

def kappa(x,k0,k1,k2): return k0+k1*np.exp(k2*x)

def philips(lam,phi0,phi1): return ( (phi1 / (1.0-lam)**2) - phi0)

def rhs(u,t,alpha,beta,delta,r,k0,k1,k2,v,phi0,phi1):
    omega, lam, d, a, N = u
    tmp = kappa(1.0-omega-r*d,k0,k1,k2) / v
    res = [omega*(philips(lam,phi0,phi1)-alpha), \
           lam*(tmp - alpha - beta - delta), \
           d * ( r - tmp + alpha ) + tmp*v - (1.0-omega), \
           alpha*a, \
           beta*N]
    return res
           
alpha = 0.025
beta = 0.02
delta = 0.01
v = 3.0
phi0 = 0.04 / (1.0-0.04**2.0)
phi1 = 0.04**3.0 / (1.0-0.04**2.0)
r = 0.03
k0 = -0.0065
k1 = np.exp(-5.0)
k2 = 20.0

omega0 = 0.50
lam0 = 0.50
d0 = 0.1
a0 = 0.8;
N0 = 300.

t=np.linspace(0.0,300.0,1000.0)
args = (alpha,beta,delta,r,k0,k1,k2,v,phi0,phi1)
res=odeint(rhs,[omega0, lam0, d0, a0, N0],t,args=args)
omega1, lam1, d1, a1, N1=res[:, 0],res[:, 1],res[:, 2],res[:, 3],res[:, 4]

Y = lam1*N1

plt.figure()
plt.plot(t,d1)
plt.show()
