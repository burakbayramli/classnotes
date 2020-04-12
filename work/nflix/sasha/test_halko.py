# Pure Halko
# randomized SVD
import numpy as np
import numpy.random as rand
import numpy.linalg as lin
import matplotlib.pyplot as plt
import pandas as pd

k = 7
df = pd.read_csv("../w1.csv",sep=';',header=None)
A = np.array(df)[:,1:]
print "A",A.shape

rand.seed(1000)
Omega = rand.randn(A.shape[1],k)

Y = np.dot(A, Omega) 
print "Y", Y.shape

Q, R = lin.qr(Y) 

BT = np.dot(A.T, Q)

print "Q", Q.shape
print "BT", BT.shape

x, x, V = lin.svd(BT)
print 'V', V.shape
Uhat = V.T # because B=USV', B'=VSU' for U of B we need V'

print "Uhat", Uhat.shape

U = np.dot(Q, Uhat) 

print "U", U.shape

plt.plot(U[:,0],U[:,1],'r+')

plt.hold(True)
        
# compare with real SVD

U, Sigma, V = lin.svd(A);
plt.plot(U[:,0],-U[:,1],'bx')

plt.show()
