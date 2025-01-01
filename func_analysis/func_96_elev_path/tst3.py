from scipy.optimize import minimize, Bounds, SR1, BFGS
import sympy
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

vars = 't a0 a1 a2 a3 a4 b0 b1 b2 b3 b4 gamma x y'
t, a0, a1, a2, a3, a4, b0, b1, b2, b3, b4, gamma, x, y = sympy.symbols(vars)

xdef = a0 + a1*t + a2*t**2 + a3*t**3 + a4*t**4
ydef = b0 + b1*t + b2*t**2 + b3*t**3 + b4*t**4

sqrdef = sympy.diff(xdef,t)**2 + sympy.diff(ydef,t)

pa0,pb0=(1.0,1.0)
pex,pey=(4.0,2.0)

def gfunc2(x, y):
    s1 = 2.2; x1 = 2.0; y1 = 2.0
    g1 = np.exp( -4 *np.log(2) * ((x-x1)**2+(y-y1)**2) / s1**2)
    s2 = 1.2; x2 = 4.0; y2 = 1.0
    g2 = np.exp( -4 *np.log(2) * ((x-x2)**2+(y-y2)**2) / s2**2)
    return (g1*10.0 + g2*10.0)


ts = np.linspace(0,1,50)
def calcint_g2(pars):
    pa1,pa2,pa3,pb1,pb2,pb3=pars
    pa4 = pex - pa0 - (pa1+pa2+pa3)
    pb4 = pey - pb0 - (pb1+pb2+pb3)
    argsubs = {a1:pa1, a2:pa2, a3:pa3, a4:pa4, \
               b1:pb1, b2:pb2, b3:pb3, b4:pb4}
    ys = []
    for tcurr in ts:
       sqrval = sqrdef.subs(argsubs).subs({t:tcurr})
       if sqrval < 0: sqrval = 0    
       xval = xdef.subs(argsubs).subs({a0: pa0}).subs({t:tcurr})
       yval = ydef.subs(argsubs).subs({b0: pb0}).subs({t:tcurr})
       prod2 = gfunc2(float(xval),float(yval))*sqrval
       ys.append(prod2)
    W = np.trapz(ys,x=ts)
    return W

LIM = 5.0
# rasgele secilmis baslangic degerleri
pa1,pa2,pa3,pb1,pb2,pb3 = 0,0,0,0,0,0
x0 = pa1,pa2,pa3,pb1,pb2,pb3

opts = {'maxiter': 40, 'verbose': 3}

res = minimize (fun=calcint_g2,x0=x0,
                method='Nelder-Mead',
                bounds=Bounds([-LIM, -LIM, -LIM, -LIM, -LIM, -LIM],
                              [LIM, LIM, LIM, LIM, LIM, LIM]),
                options=opts)
print (res['x'])

# [-0.35651176 -2.58076893 -3.26280297 -3.8745274   1.12274598  1.7020112 ]
