
```python
import util

w = 128; h = 64

import random
np.random.seed(0)
random.seed(0)

print util.randomstring()
import util, ockre
from PIL import Image

(s,t) = util.randomstring()
print s, t
dataset = util.paint_text(s,w,h,rotate=True,ud=True,multi_fonts=True)
plt.imshow(dataset.reshape(h,w),cmap='gray',interpolation="none")
plt.savefig('out1.png')
print dataset.shape
dataset = dataset.reshape(1,w,h,1)
print dataset.shape
```

```text
('CZ74254731', 'sender_dic')
29.8.2000 date_issue
(1, 64, 128)
(1, 128, 64, 1)
```

```python
w = 128; h = 64
# junk image, only one
dataset = np.zeros((1,w,h,1))

import tensorflow as tf

pool_size = 1
num_filters = 16

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

inputs = tf.placeholder(tf.float32, [None, w, h, 1])

W_conv1 = weight_variable([3, 3, 1, num_filters])
b_conv1 = bias_variable([num_filters])
h_conv1 = tf.nn.relu(conv2d(inputs, W_conv1) + b_conv1)
h_pool1 = max_pool_2x2(h_conv1)

W_conv2 = weight_variable([3, 3, num_filters, num_filters])
b_conv2 = bias_variable([num_filters])
h_conv2 = tf.nn.relu(conv2d(h_pool1, W_conv2) + b_conv2)
h_pool2 = max_pool_2x2(h_conv2)

h_pool2_flat = tf.reshape(h_pool2, [-1, 32, 256])

W_fc1 = weight_variable([256, 32])
b_fc1 = bias_variable([32])
h_fc1 = tf.nn.relu(tf.matmul(h_pool2_flat, W_fc1) + b_fc1)

print inputs.shape

with tf.Session() as sess:
     sess.run(tf.global_variables_initializer())
     output = sess.run(h_pool2_flat, feed_dict={inputs: dataset})
     print 'output',output.shape
```

```text
(?, 128, 64, 1)
(128, 1024)
output (1, 32, 256)
```











