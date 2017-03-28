'''
Read sasha output of movielens and run SVD on the original data, overlay
plots
'''
import scipy.sparse as sps
import numpy as np
import numpy.random as rand
import scipy.sparse.linalg as slin
import matplotlib.pyplot as plt
import pandas as pd

print "please wait.. this might take few seconds.."

fin = open("/tmp/U_final.dat")
arr = []
for x in fin.readlines():
    [id,row] = x.strip().split('\t')
    arr.append(map(np.float,row.split(';')))
    
U = np.array(arr)
plt.plot(-U[:,0],U[:,1],'o')
plt.hold(True)
        
df = pd.read_csv("/home/burak/Downloads/movielens1.csv",sep=';')
df = df.fillna(0) # force sparsity
dfs = sps.coo_matrix(np.array(df.ix[:,1:]))

U,S,V=slin.svds(dfs,k=7)
plt.plot(U[:,0], U[:,1],'+')

plt.show()
