import util
import pandas as pd
import numpy as np
import tensorflow as tf
import scipy.io.wavfile, zipfile
import io, time, os, random, re

fs = 16000
batch_size = 40
num_epochs = 100
num_cell = 100
num_layers = 3
mfile = "/tmp/speech1.ckpt"
train_dir = '/home/burak/Downloads/voice_cmd_small'
labels = ['up','down','yes','no']


import tensorflow as tf

tf.reset_default_graph()

dropout_prob = tf.placeholder(tf.float32)

data = tf.placeholder(tf.float32, [None, fs])

stfts = tf.contrib.signal.stft(data, frame_length=256, frame_step=128, fft_length=256)

fingerprint = tf.abs(stfts)

print fingerprint

y = tf.placeholder(tf.float32, shape=[None, len(labels)])

cells = []
for _ in range(num_layers):
    cell = tf.contrib.rnn.LSTMCell(num_cell)
    cell = tf.contrib.rnn.DropoutWrapper(cell, output_keep_prob=1-dropout_prob)
    cells.append(cell)
cell = tf.contrib.rnn.MultiRNNCell(cells)
output, states = tf.nn.dynamic_rnn(cell, fingerprint, dtype=tf.float32)
last = states[-1][0]

print last

logits = tf.contrib.layers.fully_connected(inputs=last,
                                           num_outputs=len(labels),
                                           activation_fn=None)

softmax = tf.nn.softmax_cross_entropy_with_logits(logits=logits,labels=y) 

cross_entropy = tf.reduce_mean(softmax)

train_step = tf.train.AdamOptimizer(0.001).minimize(cross_entropy)

correct_prediction = tf.equal(tf.argmax(y,1), tf.argmax(logits,1))

evaluation_step = tf.reduce_mean(tf.cast(correct_prediction, tf.float32))

sess = tf.Session()

sess.run(tf.global_variables_initializer())

saver = tf.train.Saver()

if os.path.isfile(mfile + ".index"):
     print 'restoring'
     saver.restore(sess, mfile)

for i in range(num_epochs):
    x_batch, y_batch = util.get_minibatch(batch_size)
    acc, _ = sess.run([evaluation_step, train_step], feed_dict={ data:x_batch,
                                                                 y:y_batch,
                                                                 dropout_prob:0.0})
    print i, 'accuracy', acc 
    if i % 5 == 0:
	val_x, val_y = util.get_minibatch(batch_size,validation=True)
        acc = sess.run(evaluation_step, feed_dict={ data:val_x,
                                                    y:val_y,
                                                    dropout_prob:0})
        print i, 'validation accuracy', acc        
    
saver.save(sess, mfile)
