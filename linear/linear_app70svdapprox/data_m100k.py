# http://files.grouplens.org/datasets/movielens/ml-100k.zip
from scipy.io import mmread, mmwrite
import os, numpy as np
from scipy import sparse

file_path = '%s/Downloads/ml-100k/u1.base' % os.environ['HOME']
data = np.array([[int(tok) for tok in line.split('\t')[:3]] for line in open(file_path)])
ij = data[:, :2]
ij -= 1
values = data[:, 2]
A = sparse.csc_matrix((values, ij.T)).astype(float)
mmwrite('%s/Downloads/A_m100k_train'  % os.environ['HOME'], A)

file_path = '%s/Downloads/ml-100k/u1.test' % os.environ['HOME']
data = np.array([[int(tok) for tok in line.split('\t')[:3]]  for line in open(file_path)])
ij = data[:, :2]
ij -= 1
values = data[:, 2]
A = sparse.csc_matrix((values, ij.T)).astype(float)
mmwrite('%s/Downloads/A_m100k_test'  % os.environ['HOME'], A)

