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

A = np.array( [[2, 3, 5],
               [3, 4, 5],
               [4, 5, 3]] )

P = np.array( [[1, 2, 4],
               [2, 4, 4],
               [1, 1, 1]] )

s = b-np.dot(A,x);
z = 1./s;

for iters in (range(MAXITERS)):
  gap = np.dot(s.T,z)
  res = np.dot(P,x) + q + np.dot(A.T,z)
  if (gap < TOL) & (lin.norm(res) < RESTOL):
      break
  tinv = gap/(m*MU)

  tmp1 = -np.vstack((np.hstack((P, A.T)),
                     np.hstack((A, np.diag(  (-s/z).T[0]  )))))
  tmp2 = np.vstack(( np.dot(P,x)+q+np.dot(A.T,z), -s+tinv*(1.0/z) )) 
  sol = lin.solve(tmp1, tmp2)
  dx = sol[0:n]
  dz = sol[n:n+m]
  ds = -np.dot(A,dx)
  r = np.vstack((np.dot(P,x)+q+np.dot(A.T,z),
                 z*s-tinv))
  step = np.min([1.0, 0.99/np.max(-dz/z)]);
  while (np.min(s+step*ds) <= 0):
    step = BETA*step
    print (step)
    
  newz = z+step*dz
  newx = x+step*dx
  news = s+step*ds

  tmp1 = np.dot(P,newx)+q+np.dot(A.T,newz)
  tmp2 = newz*news-tinv
  newr = np.vstack((tmp1,tmp2))
  while (lin.norm(newr) > (1-ALPHA*step)*lin.norm(r)):
    step = BETA*step;
    newz = z+step*dz
    newx = x+step*dx
    news = s+step*ds
    newr = np.vstack((np.dot(P,newx)+q+np.dot(A.T,newz),
                      newz*news-tinv))
    
  x = x+step*dx
  z = z +step*dz
  s = b-np.dot(A,x)

print (x)
  
