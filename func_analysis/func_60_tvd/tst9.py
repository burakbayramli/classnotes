import pylab
from skimage import io
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import tensorflow as tf

MU = 50.0
#MU = 0.001
EPSILON = 0.001
n = 225

np.random.seed(0)

xorig = tf.cast(tf.constant( io.imread('lena-noise.jpg', as_gray=True)),dtype=tf.float32)
x = tf.placeholder(dtype="float",shape=[n,n],name="x")

D = np.zeros((n,n))
idx1, idx2 = [], []
for i in range(n):
    idx1.append([i,i])
    if i<n-1: idx2.append([i,i+1])
idx = idx1 + idx2
ones = [1.0 for i in range(n)]
negs = [-1.0 for i in range(n-1)]
vals = ones + negs
vals = np.array(vals).astype(np.float32)
D = tf.SparseTensor(indices=idx, values=vals, dense_shape=[n, n])

diff = tf.square(tf.norm(xorig-x, ord='euclidean'))

Ux = tf.sparse_tensor_dense_matmul(D, x)
Uy = tf.sparse_tensor_dense_matmul(tf.sparse_transpose(D), tf.transpose(x))
Uy = tf.transpose(Uy)

fUx = tf.reduce_sum(tf.sqrt(EPSILON**2 + tf.square(Ux)) - EPSILON)
fUy = tf.reduce_sum(tf.sqrt(EPSILON**2 + tf.square(Uy)) - EPSILON)
phi_atv = fUx + fUy
psi = diff + MU*phi_atv
g = tf.gradients(psi, x)
g = tf.reshape(g,[n*n])

init = tf.global_variables_initializer()

sess = tf.Session()

sess.run(init)

def tv(xvec):
    xmat = xvec.reshape(n,n)
    p = sess.run(psi, {x: xmat} )
    return p

def tv_grad(xvec):
    xmat = xvec.reshape(n,n)
    gres = sess.run(g, {x: xmat} )
    return gres
    
MUG = 0.001
x0 = np.zeros(n*n)
xnew = x0

from scipy.optimize import SR1

for i in range(500):
    gnew = tv_grad(xnew)
    chg = np.sum(np.abs(MUG*gnew))
    print (tv(xnew),chg)
    xnew = xnew - MUG*gnew
    xnew /= xnew.max()/255.0 

io.imsave('/tmp/lenad2.jpg', np.reshape(xnew,(n,n)))

# MUG = 0.001, iter 500
