import pylab
from skimage import io
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import tensorflow as tf

MU = 1.0
#MU = 0.001
EPSILON = 0.001
n = 225

np.random.seed(0)

xorig = tf.cast(tf.constant( io.imread('lena-noise.jpg', as_gray=True)),dtype=tf.float32)
#x = tf.Variable(np.zeros((n,n)),name="x")
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

Ux = tf.sparse_tensor_dense_matmul(D, x)
Uy = tf.sparse_tensor_dense_matmul(tf.sparse_transpose(D), tf.transpose(x))
Uy = tf.transpose(Uy)

fUx = tf.reduce_sum(tf.sqrt(EPSILON**2 + tf.square(Ux)) - EPSILON)
fUy = tf.reduce_sum(tf.sqrt(EPSILON**2 + tf.square(Uy)) - EPSILON)
phi_atv = fUx + fUy
E = xorig-x
diff = tf.reduce_sum(tf.square(E))
psi = diff + MU*phi_atv
g = tf.gradients(psi, x)
g = tf.reshape(g,[n,n])

init = tf.global_variables_initializer()

sess = tf.Session()

sess.run(init)

sess.run(tf.global_variables_initializer())
print (type(np.zeros((n,n))))

gcurr = sess.run(g, {x: np.zeros((n,n)) })
xcurr = sess.run(x, {x: np.zeros((n,n)) })
io.imsave('/tmp/lenad1.jpg', io.imread('lena-noise.jpg', as_gray=True))
MUG = 0.001

for i in range(1000):
    gnew = sess.run(g, {x: xcurr - MUG*gcurr} )
    xnew = sess.run(x, {x: xcurr - MUG*gcurr} )
    chg = np.sum(np.abs(MUG*gcurr))
    print (chg)
    xcurr = xnew
    gcurr = gnew
    #print (xcurr)
    #print (gcurr)

io.imsave('/tmp/lenad2.jpg', xcurr)

