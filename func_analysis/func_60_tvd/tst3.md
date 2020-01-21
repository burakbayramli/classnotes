

```python
import tensorflow as tf

MU = 50.0
EPSILON = 0.001
n = 4
xorig = tf.Variable(np.ones((n,n)))
with tf.Session() as sess:
    sess.run(tf.global_variables_initializer())
    print (sess.run(xorig))

```

```text
[[1. 1. 1. 1.]
 [1. 1. 1. 1.]
 [1. 1. 1. 1.]
 [1. 1. 1. 1.]]
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












