import pandas as pd
import numpy as np
import scipy.sparse as sps
import matplotlib.pyplot as plt
import scipy.linalg as lin

def ccpca(lam, U, x, n, q):
    tol=1e-7;
    k = lam.shape
    print type(x)
    x = x.T
    V = np.zeros(U.shape);
    U
    for jjj in range(q):
        u_ = U[:,jjj]
        tmp2 = np.dot(np.dot((n+1),np.dot(x.T*u_)),x)
        exit;


df = pd.read_csv('irisdata.txt',comment="%",sep='\t',header=None)
X = np.array(df)[:,:4]
D = 4
n = df.shape[1]
Xmean = np.mean(X,axis=1).reshape((X.shape[0],1))
A = X - Xmean*np.ones((X.shape[0],1))
U,S,V = lin.svd(A);    

q = 2
values = np.ones((q,1)) / q
vectors = np.ones((D,q)) / (D*q)
ccpca(values, vectors, sps.lil_matrix(A[0, :]), n, q)



















