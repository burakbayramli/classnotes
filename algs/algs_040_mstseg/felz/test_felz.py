import scipy.sparse as sps
import scipy.io as io, random
import pandas as pd, os, sys, felz
import matplotlib.pyplot as plt
import itertools, numpy as np

syn = pd.read_csv("./doc/synthetic.txt",names=['a','b'],sep="   ")
data = np.array(syn)

from sklearn.metrics.pairwise import euclidean_distances
X = euclidean_distances(data, data)

X2 = X.copy()

# filter out large values / distances so matrix can be sparse
X2[X > 2000] = 0.0
X3 = sps.lil_matrix(X2)
#Take only upper triangular part of the matrix because it is
#symmetric, cuts the number of non-zero cells by half,
X4 = sps.triu(X3)
print 'non-zero items', len(X4.nonzero()[0])
print X4.shape

clf = felz.Felzenswalb(min_size=20,c=800)
clf.fit(X4)

# assign labels
syn['cluster'] = clf.labels_
print len(syn['cluster'].unique())

import random
for clust in syn['cluster'].unique():
    tmp = np.array(syn[syn['cluster'] == clust][['a','b']])
    plt.scatter(tmp[:,0], tmp[:,1], c=np.random.rand(3,1))
    
plt.show()
