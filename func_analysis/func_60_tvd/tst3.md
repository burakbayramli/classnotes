
https://www.tensorflow.org/api_docs/python/tf/gradients

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

with tf.Session() as sess:
    sess.run(tf.global_variables_initializer())
    print (sess.run(g1))
    print (sess.run(xvec))
    print (sess.run(yvec))
    print (sess.run(zvec))
    print (sess.run(g2))
```

```text
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












