

```python
import tensorflow as tf

MU = 50.0
EPSILON = 0.001
n = 4
xorig = tf.Variable(np.ones((n,n)))
xvec = tf.Variable(np.ones((1,n*n)))
D = np.zeros((n,n))
idx1 = []
idx2 = []
for i in range(n):
    idx1.append([i,i])
    if i<n-1: idx2.append([i,i+1])
idx = idx1 + idx2
print (idx)

ones = [1.0 for i in range(n)]
negs = [-1.0 for i in range(n-1)]
vals = ones + negs
vals = np.array(vals).astype(np.float64)
D = tf.SparseTensor(indices=idx, values=vals, dense_shape=[n, n])
x = tf.reshape(xvec, [n,n])
Ux = tf.sparse_tensor_dense_matmul(D, x)
Uy = tf.transpose(tf.sparse_tensor_dense_matmul(tf.sparse_transpose(D), tf.transpose(x)))

with tf.Session() as sess:
    sess.run(tf.global_variables_initializer())
    print (sess.run(xorig))
    print (sess.run(Ux))
    print (sess.run(Uy))
```

```text
[[0, 0], [1, 1], [2, 2], [3, 3], [0, 1], [1, 2], [2, 3]]
[[1. 1. 1. 1.]
 [1. 1. 1. 1.]
 [1. 1. 1. 1.]
 [1. 1. 1. 1.]]
[[0. 0. 0. 0.]
 [0. 0. 0. 0.]
 [0. 0. 0. 0.]
 [1. 1. 1. 1.]]
[[1. 0. 0. 0.]
 [1. 0. 0. 0.]
 [1. 0. 0. 0.]
 [1. 0. 0. 0.]]
```









https://www.tensorflow.org/api_docs/python/tf/gradients

https://www.tensorflow.org/api_docs/python/tf/Variable

https://stackoverflow.com/questions/47662143/what-is-the-difference-between-tensors-and-sparse-tensors


```python
import tensorflow as tf

# y + 2x = 2x + x
x = tf.constant(0.)
y = 2 * x
g1 = tf.gradients(x + y, [x, y])

xvec = tf.Variable([[1.], [2.],[3.]]) 
yvec = tf.Variable([[3.], [4.],[5.]]) 
xvec = xvec + np.array([[1, 1, 1]]).T
zvec = tf.matmul(tf.transpose(xvec),yvec)
g2 = tf.gradients(zvec, xvec)

#xs = tf.SparseTensor(values=[7,8],indices=[[1],[5]],dense_shape=[6])
xs = tf.SparseTensor(indices=[[0, 0], [0, 2]], values=[1., 2.], dense_shape=[1, 3])
xxs = tf.sparse_tensor_dense_matmul(xs, xvec)

g3 = tf.gradients(xxs, xvec)

with tf.Session() as sess:
    sess.run(tf.global_variables_initializer())
    print ('g1',sess.run(g1))
    print ('xvec',sess.run(xvec))
    print ('yvec',sess.run(yvec))
    print ('zvec',sess.run(zvec))
    print ('g2',sess.run(g2))
    print ('xs',sess.run(xs))
    print ('xxs',sess.run(xxs))
    print ('g3',sess.run(g3))
```

```text
g1 [3.0, 1.0]
xvec [[2.]
 [3.]
 [4.]]
yvec [[3.]
 [4.]
 [5.]]
zvec [[38.]]
g2 [array([[3.],
       [4.],
       [5.]], dtype=float32)]
xs SparseTensorValue(indices=array([[0, 0],
       [0, 2]]), values=array([1., 2.], dtype=float32), dense_shape=array([1, 3]))
xxs [[10.]]
g3 [array([[1.],
       [0.],
       [2.]], dtype=float32)]
```



```python
import tensorflow as tf

n = 4
A = np.zeros((n,n))
idx1 = []
idx2 = []
for i in range(n):
    idx1.append([i,i])
    if i<n-1: idx2.append([i,i+1])
idx = idx1 + idx2
print (idx)

ones = [1.0 for i in range(n)]
negs = [-1.0 for i in range(n-1)]
vals = ones + negs
vals = np.array(vals).astype(np.float64)
A = tf.SparseTensor(indices=idx, values=vals, dense_shape=[n, n])
x = tf.Variable(np.ones((n,n))) 
Ax = tf.sparse_tensor_dense_matmul(A, x)

with tf.Session() as sess:
    sess.run(tf.global_variables_initializer())
    print ('A',sess.run(A))
    print ('x',sess.run(x))
    print ('Ax',sess.run(Ax))
```

```text
[[0, 0], [1, 1], [2, 2], [3, 3], [0, 1], [1, 2], [2, 3]]
A SparseTensorValue(indices=array([[0, 0],
       [1, 1],
       [2, 2],
       [3, 3],
       [0, 1],
       [1, 2],
       [2, 3]]), values=array([ 1.,  1.,  1.,  1., -1., -1., -1.]), dense_shape=array([4, 4]))
x [[1. 1. 1. 1.]
 [1. 1. 1. 1.]
 [1. 1. 1. 1.]
 [1. 1. 1. 1.]]
Ax [[0. 0. 0. 0.]
 [0. 0. 0. 0.]
 [0. 0. 0. 0.]
 [1. 1. 1. 1.]]
```
















