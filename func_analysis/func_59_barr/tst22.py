import numpy as np
from scipy.optimize import linprog
import barr2

A = [[1.,  1., 1., 0.],
     [1.,  3., 0., 1.],
     [9.,  1., -3., 1.]]
b = [5., 7., -1]
c = [-1., -5., 0., 0. ]

A = np.array(A)
b = np.array(b)
c = np.array(c)

res = linprog(c, A_eq=A, b_eq=b, options={"disp": True})
print ('linprog ===============')
print (res)

#       x: array([4.3750000e-01, 2.1875000e+00, 2.3750000e+00, 1.0991855e-11])
