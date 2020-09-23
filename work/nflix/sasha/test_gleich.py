# Gleich style QR and SVD
import numpy as np
import numpy.random as rand
import numpy.linalg as lin
import matplotlib.pyplot as plt
import pandas as pd

#n = 4
#df = pd.read_csv("iris.csv",sep=',')
#A = np.array(df)[:,0:n]

#n = 5
#A = np.loadtxt('A_matrix')

n = 29
df = pd.read_csv("../w1.csv",sep=';')
A = np.array(df)[:,3:]

print A.shape

AtA = np.dot(A.T,A)

R = lin.cholesky(AtA).T

print "R", R.shape

Q = np.dot(A,lin.inv(R))

print "Q", Q.shape

Ur,x,x =  lin.svd(R)

print "Ur", Ur.shape

U = np.dot(Q,Ur)

plt.plot(-U[:,0],-U[:,1],'r+')

plt.hold(True)

# compare with real SVD

U, Sigma, V = lin.svd(A);
plt.plot(U[:,0],U[:,1],'bx')

plt.show()
