import numpy as np
from scipy.optimize import linprog
import barr

A = np.array([[1,  1, 1, 0],
              [1,  3, 0, 1]])

b = np.array([5,7])

c = np.array([-1, -5, 0, 0 ])

x0 = np.array([0.1, 2.3, 2.6, 0.1])

res = linprog(c, A_eq=A, b_eq=b, options={"disp": True})
print (res)

solver = barr.LPSolver(mu=10, tol=1e-4)
solver.solve(A, b, c)
print (solver.x_opt)

