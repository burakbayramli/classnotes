import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

from tensorflow.examples.tutorials.mnist import input_data
mnist = input_data.read_data_sets("MNIST_data/", one_hot=True)

filt = (mnist.train.labels[:,0]==1) | (mnist.train.labels[:,1]==1)
X_train = mnist.train.images[filt]
y_train = mnist.train.labels[filt]
print X_train.shape, y_train.shape
filt = (mnist.test.labels[:,0]==1) | (mnist.test.labels[:,1]==1)
X_test = mnist.train.images[filt]
y_test = mnist.test.labels[filt]
print X_test.shape, y_test.shape

# randomly take n elements
n = 80
smallidx = np.random.choice(len(X_train),size=n,replace=False)
X_train_small = X_train[smallidx]
y_train_small = y_train[smallidx]
X_test_small = X_test[:300]
y_test_small = y_test[:300]
print len(X_train_small), len(y_test_small)

print y_train_small

import tensorflow as tf

FLAGS = None

def deepnn(x):
  with tf.name_scope('reshape'):
    x_image = tf.reshape(x, [-1, 28, 28, 1])

  with tf.name_scope('conv1'):
    W_conv1 = weight_variable([3, 3, 1, 32])
    b_conv1 = bias_variable([32])
    h_conv1 = tf.nn.relu(conv2d(x_image, W_conv1) + b_conv1)

  with tf.name_scope('conv2'):
    W_conv2 = weight_variable([3, 3, 32, 32])
    b_conv2 = bias_variable([32])
    h_conv2 = tf.nn.relu(conv2d(h_conv1, W_conv2) + b_conv2)
    
  with tf.name_scope('pool1'):
    h_pool1 = max_pool_2x2(h_conv2)

  with tf.name_scope('conv3'):
    W_conv3 = weight_variable([3, 3, 32, 64])
    b_conv3 = bias_variable([64])
    h_conv3 = tf.nn.relu(conv2d(h_pool1, W_conv3) + b_conv3)

  with tf.name_scope('conv4'):
    W_conv4 = weight_variable([3, 3, 64, 64])
    b_conv4 = bias_variable([64])
    h_conv4 = tf.nn.relu(conv2d(h_conv3, W_conv4) + b_conv4)
    
  with tf.name_scope('pool2'):
    h_pool2 = max_pool_2x2(h_conv4)

  with tf.name_scope('fc1'):
    W_fc1 = weight_variable([7 * 7 * 64, 1024])
    b_fc1 = bias_variable([1024])
    h_pool2_flat = tf.reshape(h_pool2, [-1, 7*7*64])
    h_fc1 = tf.nn.relu(tf.matmul(h_pool2_flat, W_fc1) + b_fc1)

  with tf.name_scope('dropout1'):
    keep_prob = tf.placeholder(tf.float32)
    h_fc1_drop = tf.nn.dropout(h_fc1, 0.2)

  with tf.name_scope('fc2'):
    W_fc2 = weight_variable([1024, 10])
    b_fc2 = bias_variable([10])
    y_conv = tf.matmul(h_fc1_drop, W_fc2) + b_fc2
  return y_conv


def conv2d(x, W):
  return tf.nn.conv2d(x, W, strides=[1, 1, 1, 1], padding='SAME')

def max_pool_2x2(x):
  return tf.nn.max_pool(x, ksize=[1, 2, 2, 1],
                        strides=[1, 2, 2, 1], padding='SAME')

def weight_variable(shape):
  initial = tf.truncated_normal(shape, stddev=0.1)
  return tf.Variable(initial)

def bias_variable(shape):
  initial = tf.constant(0.1, shape=shape)
  return tf.Variable(initial)


# Create the model
x = tf.placeholder(tf.float32, [None, 784])

# Define loss and optimizer
y_ = tf.placeholder(tf.float32, [None, 10])

# Build the graph for the deep net
y_conv = deepnn(x)

with tf.name_scope('loss'):
  cross_entropy = tf.nn.softmax_cross_entropy_with_logits(labels=y_,
                                                          logits=y_conv)

cross_entropy = tf.reduce_mean(cross_entropy)

with tf.name_scope('adam_optimizer'):
  train_step = tf.train.AdamOptimizer(1e-2).minimize(cross_entropy)

with tf.name_scope('accuracy'):
  correct_prediction = tf.equal(tf.argmax(y_conv, 1), tf.argmax(y_, 1))
  correct_prediction = tf.cast(correct_prediction, tf.float32)
accuracy = tf.reduce_mean(correct_prediction)

with tf.Session() as sess:
  sess.run(tf.global_variables_initializer())
  for i in range(1000):
    for j in range(10):
      X = X_train[i*10:(i+1)*10]; y = y_train[i*10:(i+1)*10]
      print X.shape
      train_accuracy = accuracy.eval(feed_dict={ x: X, y_: y})
      train_step.run(feed_dict={x: X_train_small, y_: y_train_small})
      test_accuracy = accuracy.eval(feed_dict={x: X_test_small, y_: y_test_small})
      print('accuracy', train_accuracy, test_accuracy)
    
