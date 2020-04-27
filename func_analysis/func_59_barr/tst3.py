import numpy as np

import barr3

A = [[1.,  1., 1., 0.],
     [1.,  3., 0., 1.],
     [9.,  1., -3., 1.]]
b = [[5., 7., -1]]
c = [[-1., -5., 0., 0. ]]

A = np.array(A)
b = np.array(b).T
c = np.array(c).T

barr3.lp_solve(A,b,c)


