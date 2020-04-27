import numpy as np
from scipy.optimize import linprog
import barr2

A = np.array([[4,  -3, 8, -1],
              [4,  0, -1, 4]])

b = np.array([20., 18.])

c = np.array([-2, 4, 2, 2 ])

#solver = barr.LPSolver(mu=10, tol=1e-4)
print ('log bariyer ==========')
#solver.solve(A, b, c)
#print ('soln', solver.x_opt)
x_star,p_star,gap,status,nsteps = barr2.lp_solve(A,b,c)
print ('soln',x_star,p_star,gap,status,nsteps)

res = linprog(c, A_eq=A, b_eq=b, options={"disp": True})
print ('linprog ===============')
print (res)
