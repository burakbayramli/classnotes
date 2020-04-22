import pandas as pd, os, sys
import numpy as np
import matplotlib.pyplot as plt
import scipy.sparse as sps

syn = pd.read_csv("../doc/synthetic.txt",names=['a','b'],sep="   ")
data = np.array(syn)

from sklearn.metrics.pairwise import euclidean_distances
X = euclidean_distances(data, data)

X2 = X.copy()
# filter out large values / distances so matrix can be sparse
X2[X > 2000] = 0.0
X3 = sps.lil_matrix(X2)
X4 = sps.triu(X3)
print 'non-zero items', len(X4.nonzero()[0])
print X4.shape

import scipy.sparse as sps
from scipy.io import mmwrite, mmread
mmwrite('/tmp/syndist', X4)

os.system("../felzclust/felzclust /tmp/syndist.mtx 20000 100  > /tmp/out")

df = pd.read_csv('/tmp/out',sep=';')

syn['cluster'] = df['cluster']
print syn[:5]

import matplotlib.cm as cm
print len(syn['cluster'].unique()), 'clusters found'
for clust in syn['cluster'].unique():
    tmp = np.array(syn[syn['cluster'] == clust][['a','b']])
    plt.scatter(tmp[:,0], tmp[:,1], c=np.random.rand(3,1))

plt.show()
