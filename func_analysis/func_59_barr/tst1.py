import numpy as np
from scipy.optimize import linprog
import barr2

A = np.array([[1.,  1., 1., 0.],
              [1.,  3., 0., 1.]])

b = np.array([5.,7.])

c = np.array([-1., -5., 0., 0. ])

#print ('log bariyer ==========')
#solver = barr.LPSolver(mu=10, tol=1e-4)
#solver.solve(A, b, c)
#print ('soln', solver.x_opt)
#x_star,p_star,gap,status,nsteps = barr2.lp_solve(A,b,c)
#print ('soln',x_star,p_star,gap,status,nsteps)


res = linprog(c, A_eq=A, b_eq=b, options={"disp": True})
print ('linprog ===============')
print (res)
