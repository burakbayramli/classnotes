import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import scipy.linalg as lin

def  addblock_svd_update( Uarg, Sarg, Varg, Aarg, force_orth ):
  U = Varg
  V = Uarg
  S = Sarg
  A = Aarg.T
    
  current_rank = U.shape[1]
  m = np.dot(U.T,A)
  print m
  p = A - np.dot(U,m)
  P = lin.orth(p)
  print P

  return None,None,None


X = [[ 2.180116,   2.493767,  -0.047867],
     [-1.562426,  2.292670,   0.139761],
     [0.919099,  -0.887082,  -1.197149],
     [0.333190,  -0.632542,  -0.013330]]
X = np.array(X)

A = np.array([[1, 1, 1]])

X2 = np.vstack((X,A))

U,S,V = np.linalg.svd(X, full_matrices=False)
U = [[   0.77045  -0.62822  -0.10593],
     [0.56969,   0.62909,   0.46671],
     [-0.23689,  -0.43623,   0.86792],
     [-0.16047,  -0.13890,  -0.13296]]

V = [[   0.144322,  -0.967520,  -0.207550],
     [ 0.985296,   0.121124,   0.120501],
     [0.091448,   0.221889,  -0.970774 ]]

U = np.array(U)
V = np.array(V)  

Up,Sp,Vp = addblock_svd_update(U, S, V, A, True)

 






