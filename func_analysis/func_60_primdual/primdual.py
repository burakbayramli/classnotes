import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import numpy.linalg as lin
import scipy.linalg as slin

MAXITERS = 200;
TOL = 1e-6;
m=3;n = 3
RESTOL = 1e-8;
MU = 10;
ALPHA = 0.01;
BETA = 0.5;
x = np.zeros((n,1));

b = np.ones((n,1))*10.
q = np.ones((n,1))*3.
#A = np.random.randn(n,n)
A = np.array( [[2, 3, 5],
               [3, 4, 5],
               [4, 5, 3]] )
#P = np.random.rand(n,n)
P = np.array( [[1, 2, 4],
               [2, 3, 4],
               [1, 1, 1]] )

s = b-np.dot(A,x);
z = 1./s;

for iters in (range(MAXITERS)):
  gap = np.dot(s.T,z)
  res = np.dot(P,x) + q + np.dot(A.T,z)
  if (gap < TOL) & (lin.norm(res) < RESTOL):
      print ('breaking')
      break
  tinv = gap/(m*MU)

  tmp1 = np.hstack((P, A.T))
  tmp2 = np.hstack((A, np.diag(  (-s/z).T[0]  )))
  tmp3 = -np.vstack((tmp1,tmp2))
  tmp4 = np.vstack(( np.dot(P,x)+q+np.dot(A.T,z), -s+tinv*(1.0/z) ))
  print (tmp3)
  print (tmp4)
 
  sol = lin.solve(tmp3, tmp4)
  print (sol)

#  print (tmp1)
#  print (tmp2)
  break
