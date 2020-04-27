# -*- coding: utf-8 -*-
"""
Created on Tue Apr 15 07:30:07 2014
The example for linear programing using barrier method
@author: admin
"""
import numpy as np
from barr2 import lp_solve
import matplotlib.pyplot as plt
"""
input: A, b, c, and x_0 as start point
output: x_opt, v_opt 
"""

#m =100
#n = 500
m =4
n = 5
plt.close('all')
#
#A = np.random.rand(m,n)
#x_0 = np.random.rand(n)
#c = np.random.rand(n)
#b = np.dot(A,x_0)
#x_star,v_star,lambdahist = lp_solver.lp_acent(A,b,c,x_0)
#print np.dot(A.T,v_star)+c-1/x_star

#test for barrier method
#A = np.random.rand(m-1,n)
#A = np.vstack((A,np.ones((1,n))))
#x_0 = np.random.rand(n)+0.1
#b = np.dot(A,x_0)
#c = np.random.rand(n)
## analytic center
#plt.figure()
#x_star, nu_star,lambda_hist = lp_solver.lp_acent(A,b,c,x_0)
#plt.semilogy(lambda_hist,'bo-')
#plt.xlabel('iters')
#plt.ylabel('lambdasqr/2')
#
#plt.figure()
#x_star,nu_star,history,gap = lp_solver.lp_barrier(A,b,c,x_0)
#print gap

A = np.random.rand(m-1,n)
A = np.vstack((A,np.ones((1,n))))
#x_0 = np.random.rand(n)+0.1
b = np.random.rand(m,1)
c = np.random.rand(n)

print (A)
print (b)
print (c)

x_star,p_star,gap,status,nsteps = lp_solve(A,b,c)

#feasible problem
A = np.random.rand(m-1,n)
A = np.vstack((A,np.ones((1,n))))
#x_0 = np.random.rand(n)+0.1
v = np.random.rand(n,1)+0.1
b = np.dot(A,v)
c = np.random.rand(n)

x_star,p_star,gap,status,nsteps = lp_solve(A,b,c)
