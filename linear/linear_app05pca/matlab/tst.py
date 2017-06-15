import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import scipy.linalg as lin

def ccpca(lam, U, x, n, q):
    tol=1e-7;
    k = length(lam);
    print k

df = pd.read_csv('irisdata.txt',comment="%",sep='\t',header=None)
X = np.array(df)[:,:4]
D = 4
n = df.shape[1]
Xmean = np.mean(X,axis=1).reshape((X.shape[0],1))
A = X - Xmean*np.ones((X.shape[0],1))
U,S,V = lin.svd(A);    


