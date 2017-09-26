#! /usr/bin/env python

import tensorflow as tf
import numpy as np
import os
import time
import datetime
import data_helpers
from tensorflow.contrib import learn
import constants

FLAGS = tf.flags.FLAGS
FLAGS._parse_flags()
print("\nParameters:")
for attr, value in sorted(FLAGS.__flags.items()):
    print("{}={}".format(attr.upper(), value))
print("")


print("Loading data...")
x_text, y = data_helpers.load_data_and_labels(FLAGS.positive_data_file, FLAGS.negative_data_file)

# Build vocabulary
max_document_length = max([len(x.split(" ")) for x in x_text])
vocab_processor = learn.preprocessing.VocabularyProcessor(max_document_length)
x = np.array(list(vocab_processor.fit_transform(x_text)))

# Randomly shuffle data
np.random.seed(10)
shuffle_indices = np.random.permutation(np.arange(len(y)))
x_shuffled = x[shuffle_indices]
y_shuffled = y[shuffle_indices]

# Split train/test set
# TODO: This is very crude, should use cross-validation
dev_sample_index = -1 * int(FLAGS.dev_sample_percentage * float(len(y)))
x_train, x_dev = x_shuffled[:dev_sample_index], x_shuffled[dev_sample_index:]
y_train, y_dev = y_shuffled[:dev_sample_index], y_shuffled[dev_sample_index:]
print("Vocabulary Size: {:d}".format(len(vocab_processor.vocabulary_)))
print("Train/Dev split: {:d}/{:d}".format(len(y_train), len(y_dev)))

with tf.Graph().as_default():
    session_conf = tf.ConfigProto(
      allow_soft_placement=FLAGS.allow_soft_placement,
      log_device_placement=FLAGS.log_device_placement)
    sess = tf.Session(config=session_conf)
    
    with sess.as_default():
        
        num_classes=y_train.shape[1]
        sequence_length=x_train.shape[1]
        num_filters=FLAGS.num_filters
        filter_sizes=list(map(int, FLAGS.filter_sizes.split(",")))
        l2_reg_lambda=0.0
        
        input_x = tf.placeholder(tf.int32, [None, sequence_length], name="input_x")
        input_y = tf.placeholder(tf.float32, [None, num_classes], name="input_y")
        dropout_keep_prob = tf.placeholder(tf.float32, name="dropout_keep_prob")

        l2_loss = tf.constant(0.0)
        W = tf.Variable(tf.random_uniform([len(vocab_processor.vocabulary_), FLAGS.embedding_dim], -1.0, 1.0))
        embedded_chars = tf.nn.embedding_lookup(W, input_x)
        embedded_chars_expanded = tf.expand_dims(embedded_chars, -1)



        #####################################################3

            
        sess.run(tf.global_variables_initializer())

        batches = data_helpers.batch_iter(
            list(zip(x_train, y_train)), FLAGS.batch_size, FLAGS.num_epochs)

        for batch in batches:
            x_batch, y_batch = zip(*batch)
            feed_dict = {
                input_x: x_batch,
                input_y: y_batch,
                dropout_keep_prob: FLAGS.dropout_keep_prob
            }
            res = sess.run(train_op, feed_dict)
            res = sess.run(accuracy, feed_dict)
            print res







        
