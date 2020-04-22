


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
from PIL import Image
import util

w = 512; h = 64
num_classes = 68

(t,s) = util.randomstring()
print s, t
res = util.paint_text(s,512,64,rotate=True,ud=True,multi_fonts=True)
res = res.reshape(64,512)
print res.shape
#im = Image.fromarray(res)
#plt.imshow(res)
plt.imshow(res,cmap='gray',interpolation="none")
plt.savefig('out1.png')
dataset = res.reshape(1,w,h,1)
print dataset.shape
```

```text
pIm6idNRDQwTn [15, 34, 12, 58, 8, 3, 39, 43, 29, 42, 22, 45, 13]
(64, 512)
(1, 512, 64, 1)
```

```python
import tensorflow as tf
# junk image, only one
#dataset = np.zeros((1,w,h,1))

pool_size = 1
num_filters = 16
hidden_layer_size = 512

def weight_variable(shape):
  initial = tf.truncated_normal(shape, stddev=0.1)
  return tf.Variable(initial)

def bias_variable(shape):
  initial = tf.constant(0.1, shape=shape)
  return tf.Variable(initial)

def conv2d(x, W):
  return tf.nn.conv2d(x, W, strides=[1, 1, 1, 1], padding='SAME')

def max_pool_1x1(x):
  return tf.nn.max_pool(x, ksize=[1, 1, 1, 1],
                        strides=[1, 1, 1, 1], padding='SAME')
def max_pool_2x2(x):
  return tf.nn.max_pool(x, ksize=[1, 2, 2, 1],
                        strides=[1, 2, 2, 1], padding='SAME')

def ctc_loss_layer(rnn_logits, sequence_labels, sequence_length):
    loss = tf.nn.ctc_loss(sequence_labels,rnn_logits,sequence_length,time_major=True)
    total_loss = tf.reduce_mean(loss)
    return total_loss

tf.reset_default_graph()

inputs = tf.placeholder(tf.float32, [None, w, h, 1])
print 'inputs', inputs.shape

W_conv1 = weight_variable([3, 3, 1, num_filters])
b_conv1 = bias_variable([num_filters])
h_conv1 = tf.nn.relu(conv2d(inputs, W_conv1) + b_conv1)
h_pool1 = max_pool_1x1(h_conv1)

W_conv2 = weight_variable([3, 3, num_filters, num_filters])
b_conv2 = bias_variable([num_filters])
h_conv2 = tf.nn.relu(conv2d(h_pool1, W_conv2) + b_conv2)
h_pool2 = max_pool_1x1(h_conv2)
print 'h_pool2',h_pool2

h_pool2_flat = tf.reshape(h_pool2, [-1, 32, 256])
print 'h_pool2_flat',h_pool2_flat

W_fc1 = tf.contrib.layers.fully_connected(h_pool2_flat, 32)
print 'W_fc1',W_fc1

cell = tf.contrib.rnn.GRUCell(hidden_layer_size)
rnnout, states = tf.nn.dynamic_rnn(cell, h_pool2_flat, dtype=tf.float32)
print 'rnnshape', rnnout.shape

rnn_logits = tf.layers.dense( rnnout, num_classes+1, 
                              activation=tf.nn.relu,
                              kernel_initializer=tf.contrib.layers.variance_scaling_initializer(),
                              bias_initializer=tf.constant_initializer(value=0.0),
                              name='logits')

print 'logits', rnn_logits.shape

#loss = ctc_loss_layer(rnn_logits,label,sequence_length)
targets = tf.sparse_placeholder(tf.int32)
seq_len = tf.placeholder(tf.int32, [None])
loss = tf.nn.ctc_loss(targets, rnn_logits, seq_len)

with tf.Session() as sess:
     sess.run(tf.global_variables_initializer())
     #train_targets = sparse_tuple_from([train_targets_2])
     output = sess.run(rnnout, feed_dict={inputs: dataset})
```

```text
inputs (?, 512, 64, 1)
h_pool2 Tensor("MaxPool_1:0", shape=(?, 512, 64, 16), dtype=float32)
h_pool2_flat Tensor("Reshape:0", shape=(?, 32, 256), dtype=float32)
W_fc1 Tensor("fully_connected/Relu:0", shape=(?, 32, 32), dtype=float32)
rnnshape (?, 32, 512)
logits (?, 32, 69)
```











