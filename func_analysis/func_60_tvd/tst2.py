import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import autograd as ad
from autograd import numpy as anp
import scipy.sparse as sps

MU = 50.0
EPSILON = 0.001
n = 300
np.random.seed(0)
xorig = np.random.randn(n,n)
data = np.array([-1*np.ones(n), np.ones(n)])
diags = np.array([0, 1])
D = sps.spdiags(data, diags, n, n).todense()
#print (D)
#print (xorig)

def f(u):
    return anp.power(EPSILON**2 + anp.power(u,2),0.5) - EPSILON

def obj(xvec):
    x = xvec.reshape(n,n)
    Ux = D.dot(x)
    Uy = D.T.dot(x.T).T
    Ux = f(Ux).sum()
    Uxsum = f(Ux).sum()
    Uysum = f(Uy).sum()
    phi_atv = Uxsum + Uysum
    #print (phi_atv)

    E = xorig-x
    diff = anp.power(E,2).sum()

    psi = diff + MU*phi_atv
    
    #print (psi)
    return psi

    
#x0 = np.array([[1.,2.,3.],[2.,3.,4.],[4.,5.,6.]])
x0 = np.random.randn(n,n)
#print (x0)
obj(x0)

grad = ad.grad(obj)
x00 = ad.numpy.reshape(x0,n*n)
res = grad(x00)
