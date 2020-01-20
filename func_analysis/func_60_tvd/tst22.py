import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import scipy.sparse as sps
import chumpy as ch

MU = ch.Ch(50.0)
EPSILON = ch.Ch(0.001)
n = 300
np.random.seed(0)
xorig = ch.array(np.random.randn(n,n))
data = np.array([-1*np.ones(n), np.ones(n)])
diags = np.array([0, 1])
D = sps.spdiags(data, diags, n, n)

def f(u):
    return ch.sqrt(EPSILON**2 + ch.power(u,2)) - EPSILON

xvec = ch.array(ch.zeros(n*n))
x = xvec.reshape(n,n)
Ux = D.dot(x)
Uy = D.T.dot(x.T).T
Uxsum = f(Ux).sum()
Uysum = f(Uy).sum()
phi_atv = Uxsum + Uysum
print (type(phi_atv))
E = xorig-x
diff = ch.power(E,2).sum()
print (diff)
print (type(diff))
psi = diff + MU*phi_atv
print (psi)
res = psi.dr_wrt(x)
print (res)






