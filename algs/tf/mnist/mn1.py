import pandas as pd
import numpy as np
from keras.datasets import mnist
import tensorflow as tf
from keras.utils.np_utils import to_categorical
import matplotlib.pyplot as plt

def deepnn(x):
  """deepnn builds the graph for a deep net for classifying digits.

  Args:
    x: an input tensor with the dimensions (N_examples, 784), where 784 is the
    number of pixels in a standard MNIST image.

  Returns:
    A tuple (y, keep_prob). y is a tensor of shape (N_examples, 10), with values
    equal to the logits of classifying the digit into one of 10 classes (the
    digits 0-9). keep_prob is a scalar placeholder for the probability of
    dropout.
  """
  # Reshape to use within a convolutional neural net.
  # Last dimension is for "features" - there is only one here, since images are
  # grayscale -- it would be 3 for an RGB image, 4 for RGBA, etc.
  with tf.name_scope('reshape'):
    x_image = tf.reshape(x, [-1, 28, 28, 1])

  # First convolutional layer - maps one grayscale image to 32 feature maps.
  with tf.name_scope('conv1'):
    W_conv1 = weight_variable([5, 5, 1, 32])
    b_conv1 = bias_variable([32])
    h_conv1 = tf.nn.relu(conv2d(x_image, W_conv1) + b_conv1)

  # Pooling layer - downsamples by 2X.
  with tf.name_scope('pool1'):
    h_pool1 = max_pool_2x2(h_conv1)

  # Second convolutional layer -- maps 32 feature maps to 64.
  with tf.name_scope('conv2'):
    W_conv2 = weight_variable([5, 5, 32, 64])
    b_conv2 = bias_variable([64])
    h_conv2 = tf.nn.relu(conv2d(h_pool1, W_conv2) + b_conv2)

  # Second pooling layer.
  with tf.name_scope('pool2'):
    h_pool2 = max_pool_2x2(h_conv2)

  # Fully connected layer 1 -- after 2 round of downsampling, our 28x28 image
  # is down to 7x7x64 feature maps -- maps this to 1024 features.
  with tf.name_scope('fc1'):
    W_fc1 = weight_variable([7 * 7 * 64, 1024])
    b_fc1 = bias_variable([1024])
    h_pool2_flat = tf.reshape(h_pool2, [-1, 7*7*64])
    h_fc1 = tf.nn.relu(tf.matmul(h_pool2_flat, W_fc1) + b_fc1)

  # Dropout - controls the complexity of the model, prevents co-adaptation of
  # features.
  with tf.name_scope('dropout'):
    keep_prob = tf.placeholder(tf.float32)
    h_fc1_drop = tf.nn.dropout(h_fc1, keep_prob)

  # Map the 1024 features to 10 classes, one for each digit
  with tf.name_scope('fc2'):
    W_fc2 = weight_variable([1024, 2])
    b_fc2 = bias_variable([2])

    y_conv = tf.matmul(h_fc1_drop, W_fc2) + b_fc2
  return y_conv, keep_prob


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
y_ = tf.placeholder(tf.float32, [None, 2])

# Build the graph for the deep net
y_conv, keep_prob = deepnn(x)

with tf.name_scope('loss'):
  cross_entropy = tf.nn.softmax_cross_entropy_with_logits(labels=y_,
                                                          logits=y_conv)

cross_entropy = tf.reduce_mean(cross_entropy)

with tf.name_scope('adam_optimizer'):
  train_step = tf.train.AdamOptimizer(1e-3).minimize(cross_entropy)

with tf.name_scope('accuracy'):
  correct_prediction = tf.equal(tf.argmax(y_conv, 1), tf.argmax(y_, 1))
  correct_prediction = tf.cast(correct_prediction, tf.float32)
accuracy = tf.reduce_mean(correct_prediction)


## Load the data ##
(X_train, y_train), (X_test, y_test) = mnist.load_data()

## Set the number of training samples to use ##
batch_size = 1

X_train = np.expand_dims(X_train, axis=3)
X_test = np.expand_dims (X_test, axis=3)

# Get training data for 0 and 1
inds = np.where((y_train == 0) | (y_train == 1))[0]
train_size = 80
X_train_small = X_train[inds]
X_train_small = X_train_small[:train_size]
y_train_small = y_train[inds]
y_train_small = y_train_small[:train_size]

# Get test data for 0 and 1 only
inds_test = np.where((y_test == 0) | (y_test == 1))
X_test_01 = X_test[inds_test]
y_test_01 = y_test[inds_test]

# split into validation and test sets
test_start_ind = int(np.floor((len(X_test_01)/2)))
X_test_small = X_test_01[:test_start_ind]
X_final_test = X_test_01[test_start_ind:]

y_test_small = y_test_01[:test_start_ind]
y_final_test = y_test_01[test_start_ind:]

# one hot labels
one_hot_train = to_categorical(y_train_small, 2)
one_hot_test = to_categorical(y_test_small, 2)
one_hot_final_test = to_categorical(y_final_test, 2)

n_classes = one_hot_train.shape[1]

X_test_flat = X_test_small.reshape((len(X_test_small),784))
X_final_test_flat = X_final_test.reshape((len(X_final_test),784))

with tf.Session() as sess:
  sess.run(tf.global_variables_initializer())
  for i in range(1000):
    index = 0
    for i in range(5):
        fold_inds = np.random.choice(inds,train_size)
        X_train_fold = X_train[fold_inds]
        print X_train_fold.shape
        X_train_fold = X_train_fold.reshape((len(X_train_fold),784))
        y_train_fold = y_train[fold_inds]
        one_hot_fold = to_categorical(y_train_fold, 2)
        print X_train_fold.shape, one_hot_fold.shape
        X = X_train_fold; y = one_hot_fold
        train_step.run(feed_dict={x: X, y_: y, keep_prob: 0.5})
        train_accuracy = accuracy.eval(feed_dict={ x: X, y_: y, keep_prob: 1.0})
        X_test_small = X_test_small.reshape((len(X_test_small),784))
        test_accuracy = accuracy.eval(feed_dict={x: X_test_small, y_: y_test_small, keep_prob: 1.0})
        print('accuracy', train_accuracy, test_accuracy)
        
