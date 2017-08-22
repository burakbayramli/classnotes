import matplotlib.pyplot as plt
import tensorflow as tf
import pandas as pd
import numpy as np

t_min, t_max = 0, 30
resolution = 0.01

def f(t): return t * np.sin(t) / 3 + 2 * np.sin(t*5)

def next_batch(batch_size, n_steps):
    t = np.linspace(t_min, t_max, int((t_max - t_min) / resolution))
    print len(t)
    piece = n_steps + 1
    x0 = []; x1 = []
    for i in range(len(t)):
    	tb = t[piece:(piece+n_steps+1)]
	yb = f(tb)
	if len(x0) < batch_size:
	    x0.append(yb[:n_steps])
	    x1.append(yb[1:])
        else:
            yield np.array(x0).reshape(-1,n_steps,1), np.array(x1).reshape(-1,n_steps,1)
            x0 = []; x1 = []


import tensorflow as tf

def reset_graph(seed=42):
    tf.reset_default_graph()
    tf.set_random_seed(seed)
    np.random.seed(seed)

reset_graph()

n_steps = 20
n_inputs = 1
n_neurons = 100
n_outputs = 1
n_iterations = 1
batch_size = 50
print batch_size, n_steps

X = tf.placeholder(tf.float32, [None, n_steps, n_inputs])
y = tf.placeholder(tf.float32, [None, n_steps, n_outputs])

cell = tf.contrib.rnn.OutputProjectionWrapper(
    tf.contrib.rnn.BasicRNNCell(num_units=n_neurons, activation=tf.nn.relu),
    output_size=n_outputs)

outputs, states = tf.nn.dynamic_rnn(cell, X, dtype=tf.float32)

learning_rate = 0.001

loss = tf.reduce_mean(tf.square(outputs - y)) # MSE
optimizer = tf.train.AdamOptimizer(learning_rate=learning_rate)
training_op = optimizer.minimize(loss)

init = tf.global_variables_initializer()

saver = tf.train.Saver()
mfile = "/home/burak/Downloads/scikit-data/my_time_series_model"
with tf.Session() as sess:
    init.run()
    for X_batch, y_batch in next_batch(batch_size, n_steps):
        sess.run(training_op, feed_dict={X: X_batch, y: y_batch})
        mse = loss.eval(feed_dict={X: X_batch, y: y_batch})
        print("\tMSE:", mse)

    saver.save(sess, mfile) # not shown in the book    

    X_new = np.copy(y_batch[-1])
    y_pred = sess.run(outputs, feed_dict={X: X_new})
    print y_pred
