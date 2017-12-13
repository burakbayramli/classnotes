

```python
from tensorflow.contrib.framework.python.ops import audio_ops as contrib_audio
import tensorflow as tf

def weight_variable(shape):
  initial = tf.truncated_normal(shape, stddev=0.1)
  return tf.Variable(initial)

def bias_variable(shape):
  initial = tf.constant(0.1, shape=shape)
  return tf.Variable(initial)

def conv2d(x, W):
  return tf.nn.conv2d(x, W, strides=[1, 1, 1, 1], padding='SAME')

def max_pool_3x3(x):
  return tf.nn.max_pool(x, ksize=[1, 3, 3, 1],
                        strides=[1, 2, 2, 1], padding='SAME')


init_op = tf.global_variables_initializer()

data = tf.placeholder(tf.float32, [None, None])

stfts = tf.contrib.signal.stft(data,
			       frame_length=110,
			       frame_step=125,
			       fft_length=1678)
spectrograms = tf.abs(stfts)
spectrograms2 = tf.reshape(spectrograms,(-1,128,840,1))

W_conv1 = weight_variable([7, 7, 1, 16])
b_conv1 = bias_variable([16])
h_conv1 = tf.nn.relu(conv2d(spectrograms2, W_conv1) + b_conv1)
print 'conv 1', h_conv1.shape
h_pool1 = max_pool_3x3(h_conv1)
print 'pool 1', h_pool1.shape

W_conv2 = weight_variable([5, 5, 16, 32])
b_conv2 = bias_variable([32])
h_conv2 = tf.nn.relu(conv2d(h_pool1, W_conv2) + b_conv2)
print 'conv 2', h_conv2.shape
h_pool2 = max_pool_3x3(h_conv2)
print 'pool 2', h_pool2.shape

W_conv3 = weight_variable([3, 3, 32, 32])
b_conv3 = bias_variable([32])
h_conv3 = tf.nn.relu(conv2d(h_pool2, W_conv3) + b_conv3)
print 'conv 3', h_conv3.shape
h_pool3 = max_pool_3x3(h_conv3)
print 'pool 3', h_pool3.shape

W_conv4 = weight_variable([3, 3, 32, 32])
b_conv4 = bias_variable([32])
h_conv4 = tf.nn.relu(conv2d(h_pool3, W_conv4) + b_conv4)
print 'conv 4', h_conv4.shape
h_pool4 = max_pool_3x3(h_conv4)
print 'pool 4', h_pool4.shape

from tensorflow.python.ops import random_ops
s = np.random.rand(2,16000)
with tf.Session() as sess:
     sess.run(tf.global_variables_initializer())
     res = sess.run(h_pool4, feed_dict={data: s })  
print res.shape
```

```text
conv 1 (?, 128, 840, 16)
pool 1 (?, 64, 420, 16)
conv 2 (?, 64, 420, 32)
pool 2 (?, 32, 210, 32)
conv 3 (?, 32, 210, 32)
pool 3 (?, 16, 105, 32)
conv 4 (?, 16, 105, 32)
pool 4 (?, 8, 53, 32)
(2, 8, 53, 32)
```


















