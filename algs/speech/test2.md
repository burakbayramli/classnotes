

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
print spectrograms2

W_conv1 = weight_variable([7, 7, 1, 16])
b_conv1 = bias_variable([16])
h_conv1 = tf.nn.relu(conv2d(spectrograms2, W_conv1) + b_conv1)
h_pool1 = max_pool_3x3(h_conv1)

from tensorflow.python.ops import random_ops
s = np.random.rand(2,16000)
with tf.Session() as sess:
     sess.run(tf.global_variables_initializer())
     res = sess.run(h_pool1, feed_dict={data: s })  
print res.shape
```

```text
Tensor("Reshape_4:0", shape=(?, 128, 840, 1), dtype=float32)
(2, 64, 420, 16)
```


















