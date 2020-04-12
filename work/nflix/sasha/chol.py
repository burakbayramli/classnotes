'''
Read context of input file, run Cholesky on the contents
output the result in another file
'''
import numpy as np, sys, itertools
from scipy import sparse
import random, re, sys, proj
import numpy.linalg as lin

arr = []; fin = open(sys.argv[1])
for x in fin.readlines():
    [id,row] = x.strip().split('\t')
    arr.append(map(np.float,row.split(';')))
    
res =  np.array(arr)
c = lin.cholesky(res)

np.savetxt(sys.argv[2], c.T, delimiter=';')
