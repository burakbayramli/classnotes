import pylab
from skimage import io
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import tensorflow as tf

MU = 50.0
EPSILON = 0.001
n = 225

np.random.seed(0)

xorig = tf.Variable( io.imread('lena-noise.jpg', as_gray=True) )
x = tf.Variable(np.ones((n,n)))
D = np.zeros((n,n))
idx1, idx2 = [], []
for i in range(n):
    idx1.append([i,i])
    if i<n-1: idx2.append([i,i+1])
idx = idx1 + idx2

ones = [1.0 for i in range(n)]
negs = [-1.0 for i in range(n-1)]
vals = ones + negs
vals = np.array(vals).astype(np.float64)
D = tf.SparseTensor(indices=idx, values=vals, dense_shape=[n, n])
Ux = tf.sparse_tensor_dense_matmul(D, x)
fUx = tf.reduce_sum(tf.sqrt(EPSILON**2 + tf.square(Ux)) - EPSILON)
Uy = tf.transpose(tf.sparse_tensor_dense_matmul(tf.sparse_transpose(D), tf.transpose(x)))
fUy = tf.reduce_sum(tf.sqrt(EPSILON**2 + tf.square(Uy)) - EPSILON)
phi_atv = Ux + Uy
E = xorig-x
diff = tf.reduce_sum(tf.square(E))
psi = diff + MU*phi_atv
g = tf.gradients(psi, x)

with tf.Session() as sess:
    sess.run(tf.global_variables_initializer())
    step = sess.run(g,feed_dict={x: np.zeros((n,n))})
    print (np.sum(np.abs(step[0])))
    for i in range(100):
        step = sess.run(g,feed_dict={x: (x - 0.0000001*step[0]).eval() } )
        print (np.sum(np.abs(step[0])))
    arr = x.eval()
    print (arr)
    pylab.imsave('lena-denoised.jpg',arr)

        
