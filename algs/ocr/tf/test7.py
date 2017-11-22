import tensorflow as tf
import pandas as pd
import numpy as np
import util
import matplotlib.pyplot as plt

num_epochs = 1
w = 512; h = 64
num_filters = 16
hidden_layer_size = 512
num_classes = len(util.all_chars_idx)

def sparse_tuple_from(sequences, dtype=np.int32):
    indices = []
    values = []
    for n, seq in enumerate(sequences):
        indices.extend(zip([n] * len(seq), range(len(seq))))
        values.extend(seq)
    indices = np.asarray(indices, dtype=np.int64)
    values = np.asarray(values, dtype=dtype)
    shape = np.asarray([len(sequences), np.asarray(indices).max(0)[1] + 1], dtype=np.int64)
    return indices, values, shape

def get_minibatch(batch_size=1):
    vals = []; labels = []; seq_lens = []
    for i in range(batch_size):
    	(idxs,str) = util.randomstring()
    	res = util.paint_text(str,w,h,rotate=True,ud=True,multi_fonts=True)
    	vals.append(res)    
    	labels.append(idxs)
        seq_lens.append(len(str))
    return np.reshape(vals, (batch_size,w,h,1)), sparse_tuple_from(labels), seq_lens

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

rnn_logits = tf.layers.dense( rnnout,
                              num_classes+1, 
                              activation=tf.nn.relu,
                              kernel_initializer=tf.contrib.layers.variance_scaling_initializer(),
                              bias_initializer=tf.constant_initializer(value=0.0),
                              name='logits')

print 'rnn logits', rnn_logits.shape

rnn_logits2 = tf.reshape(rnn_logits, [1, -1, 63])

print 'rnn logits 2', rnn_logits2.shape

targets = tf.sparse_placeholder(tf.int32)
seq_len = tf.placeholder(tf.int32, [None])
loss = tf.nn.ctc_loss(targets, rnn_logits2, seq_len)
cost = tf.reduce_mean(loss)

optimizer = tf.train.MomentumOptimizer(learning_rate=0.005,
                                       momentum=0.9).minimize(cost)

train_seq_len = [32]

with tf.Session() as sess:
     sess.run(tf.global_variables_initializer())

     for curr_epoch in range(num_epochs):
       train_data, train_targets, train_seq_lens = get_minibatch(batch_size=1)
       print train_data.shape
       feed_dict={inputs: train_data, targets: train_targets, seq_len: train_seq_len}
       batch_cost, batch_opt = sess.run([cost, optimizer], feed_dict)










