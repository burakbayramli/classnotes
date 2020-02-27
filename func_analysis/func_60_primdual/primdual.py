import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

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
P = np.random.rand(n,n)

s = b-np.dot(A,x);
z = 1./s;

print (s)

for iters in (range(MAXITERS)):
  gap = np.dot(s.T,z)
  res = np.dot(P,x) + q + np.dot(A.T,z)
  print (res)
  break
