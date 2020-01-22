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
#    print ('tv',xvec.shape)
    xmat = xvec.reshape(n,n)
    p = sess.run(psi, {x: xmat} )
#    print (p)
    return p

def tv_grad(xvec):
#    print ('tv grad',xvec.shape)
    xmat = xvec.reshape(n,n)
    gres = sess.run(g, {x: xmat} )
    #gres = np.reshape(gres[0],(n*n))
#    print (gres.shape)
    return gres
    
#img = np.random.randn(n*n)
#print (img.shape)
#print (tv(img))
#print (tv_grad(img))

#exit()

from scipy.optimize import minimize

def callbackx(cx, state):
    print (cx)
    state.x /= state.x.max()/255.0    
    return False

x0 = np.zeros(n*n)
res = minimize(fun=tv,
               x0=x0,
               #method='Newton-CG',
               method='trust-constr',
               #method='BFGS',
               callback=callbackx,
               jac = tv_grad,
               #jac = '2-point',
               options={'disp': True})

io.imsave('/tmp/lenad2.jpg', np.reshape(res.x,(n,n)))
