from sklearn.metrics.pairwise import euclidean_distances
import itertools, numpy as np, os
import matplotlib.pyplot as plt
import pandas as pd

syn = pd.read_csv("synthetic.txt",names=['a','b'],sep="   ")
print syn.shape
data = np.array(syn)

plt.scatter(data[:,0],data[:,1])
plt.savefig('test_02.png')

X = euclidean_distances(data, data)
print X.shape
print X[1,2]

import scipy.sparse as sps
from scipy.io import mmwrite, mmread
print X.shape
X2 = X.copy()
X2[X > 2000] = 0.0
print X2.shape
X3 = sps.lil_matrix(X2)
print len(X3.nonzero()[0])
print X3.shape

print len(X3.nonzero()[0])
X4 = sps.triu(X3)
print len(X4.nonzero()[0])

import scipy.sparse as sps
from scipy.io import mmwrite, mmread
mmwrite('/tmp/syndist', X4)

os.system("../felzclust/felzclust /tmp/syndist.mtx 25000 100  > /tmp/out")

df = pd.read_csv('/tmp/out',sep=';')

syn['cluster'] = df['cluster']
print len(syn['cluster'].unique()), 'clusters found'
print syn[:5]

import random
for clust in syn['cluster'].unique():
    tmp = np.array(syn[syn['cluster'] == clust][['a','b']])
    plt.scatter(tmp[:,0], tmp[:,1], c=np.random.rand(3,1))

plt.show()
