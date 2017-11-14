
```python
import util
print util.randomstring()
import util, ockre
from PIL import Image

(s,t) = util.randomstring()
print s, t
dataset = util.paint_text(s,512,64,rotate=True,ud=True,multi_fonts=True)
plt.imshow(dataset.reshape(64,512),cmap='gray',interpolation="none")
plt.savefig('out1.png')
print dataset.shape
dataset = dataset.reshape(1,512,64,1)
print dataset.shape
```

```text
('21. 3. 2019', 'date_due')
CZ90696352 sender_dic
(1, 64, 512)
(1, 512, 64, 1)
```

```python
import tensorflow as tf

def weight_variable(shape):
  initial = tf.truncated_normal(shape, stddev=0.1)
  return tf.Variable(initial)

def bias_variable(shape):
  initial = tf.constant(0.1, shape=shape)
  return tf.Variable(initial)

def conv2d(x, W):
  return tf.nn.conv2d(x, W, strides=[1, 1, 1, 1], padding='SAME')

def max_pool_2x2(x):
  return tf.nn.max_pool(x, ksize=[1, 2, 2, 1],
                        strides=[1, 2, 2, 1], padding='SAME')

def max_pool_1x1(x):
  return tf.nn.max_pool(x, ksize=[1, 1, 1, 1],
                        strides=[1, 1, 1, 1], padding='SAME')

inputs = tf.placeholder(tf.float32, [None, 512, 64, 1])

W_conv1 = weight_variable([3, 3, 1, 16])
b_conv1 = bias_variable([16])
h_conv1 = tf.nn.relu(conv2d(inputs, W_conv1) + b_conv1)
h_pool1 = max_pool_1x1(h_conv1)

print inputs.shape
with tf.Session() as sess:
     sess.run(tf.global_variables_initializer())
     output = sess.run(h_pool1, feed_dict={inputs: dataset})
     print output.shape
```

```text
(?, 512, 64, 1)
(1, 512, 64, 16)
```













