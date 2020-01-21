

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
xs = tf.SparseTensor(indices=[[0, 0], [1, 2]], values=[1., 2.], dense_shape=[3, 3])
xxs = tf.sparse_tensor_dense_matmul(xs, xvec)

with tf.Session() as sess:
    sess.run(tf.global_variables_initializer())
    print ('xs',sess.run(xs))
    print ('xxs',sess.run(xxs))
    print (sess.run(g1))
    print (sess.run(xvec))
    print (sess.run(yvec))
    print (sess.run(zvec))
    print (sess.run(g2))
```

```text
xs SparseTensorValue(indices=array([[0, 0],
       [1, 2]]), values=array([1., 2.], dtype=float32), dense_shape=array([3, 3]))
xxs [[2.]
 [8.]
 [0.]]
[3.0, 1.0]
[[2.]
 [3.]
 [4.]]
[[3.]
 [4.]
 [5.]]
[[38.]]
[array([[3.],
       [4.],
       [5.]], dtype=float32)]
```












