python -u image_ocr.py 
Using TensorFlow backend.
(?, 128, 64, 1)
(3, 3)
Tensor("conv1/Relu:0", shape=(?, 128, 64, 16), dtype=float32)
2
Tensor("max1/MaxPool:0", shape=(?, 64, 32, 16), dtype=float32)
(3, 3)
relu
Tensor("conv2/Relu:0", shape=(?, 64, 32, 16), dtype=float32)
Tensor("max2/MaxPool:0", shape=(?, 32, 16, 16), dtype=float32)
Tensor("reshape/Reshape:0", shape=(?, 32, 256), dtype=float32)
32
Tensor("reshape/Reshape:0", shape=(?, 32, 256), dtype=float32)
Tensor("dense1/Relu:0", shape=(?, 32, 32), dtype=float32)
rnn_size 512
Tensor("gru1/transpose_1:0", shape=(?, ?, 512), dtype=float32)
Tensor("gru1_b/transpose_1:0", shape=(?, ?, 512), dtype=float32)
Tensor("add_1/add:0", shape=(?, ?, 512), dtype=float32)
Tensor("gru2/transpose_1:0", shape=(?, ?, 512), dtype=float32)
Tensor("gru2_b/transpose_1:0", shape=(?, ?, 512), dtype=float32)




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
#dataset = np.zeros((1,w,h,1))

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

W_fc1 = tf.contrib.layers.fully_connected(h_pool2_flat, 32)

print inputs.shape

with tf.Session() as sess:
     sess.run(tf.global_variables_initializer())
     output = sess.run(W_fc1, feed_dict={inputs: dataset})
     print 'output',output.shape
```

```text
(?, 128, 64, 1)
output (1, 32, 32)
```











