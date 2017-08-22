

```python
np.random.seed(1)

t_min, t_max = 0, 30
resolution = 0.1

def f(t):
    return t * np.sin(t) / 3 + 2 * np.sin(t*5)

def next_batch(batch_size, n_steps):
    t0 = np.random.rand(batch_size, 1) * (t_max - t_min - n_steps * resolution)
    Ts = t0 + np.arange(0., n_steps + 1) * resolution
    ys = f(Ts)
    return ys[:, :-1].reshape(-1, n_steps, 1), ys[:, 1:].reshape(-1, n_steps, 1)
```

```python
t = np.linspace(t_min, t_max, int((t_max - t_min) / resolution))
y = f(t)
plt.plot(t,y)

batch_size = 4
n_steps = 3
t0 = np.random.rand(batch_size, 1) * (t_max - t_min - n_steps * resolution)
Ts = t0 + np.arange(0., n_steps + 1) * resolution
ys = f(Ts)
print Ts
print ys
print ys[:, :-1]
print ys[:, 1:]
plt.plot(Ts,ys,'r.')

plt.savefig('time_03.png')
```

```text
[[  1.23855535e+01   1.24855535e+01   1.25855535e+01   1.26855535e+01]
 [  2.13936375e+01   2.14936375e+01   2.15936375e+01   2.16936375e+01]
 [  3.39693208e-03   1.03396932e-01   2.03396932e-01   3.03396932e-01]
 [  8.97927741e+00   9.07927741e+00   9.17927741e+00   9.27927741e+00]]
[[-2.3141651  -1.12233854  0.27200624  1.6253068 ]
 [ 4.31878159  4.63599743  4.61528818  4.112844  ]
 [ 0.03397153  0.99207952  1.71474727  2.02731967]
 [ 2.87376693  3.00044595  2.62695206  1.77847377]]
[[-2.3141651  -1.12233854  0.27200624]
 [ 4.31878159  4.63599743  4.61528818]
 [ 0.03397153  0.99207952  1.71474727]
 [ 2.87376693  3.00044595  2.62695206]]
[[-1.12233854  0.27200624  1.6253068 ]
 [ 4.63599743  4.61528818  4.112844  ]
 [ 0.99207952  1.71474727  2.02731967]
 [ 3.00044595  2.62695206  1.77847377]]
```



```python
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

n_iterations = 400
batch_size = 50

saver = tf.train.Saver()
mfile = "/home/burak/Downloads/scikit-data/my_time_series_model"
with tf.Session() as sess:
    init.run()
    for iteration in range(n_iterations):
        X_batch, y_batch = next_batch(batch_size, n_steps)
        sess.run(training_op, feed_dict={X: X_batch, y: y_batch})
        if iteration % 100 == 0:
            mse = loss.eval(feed_dict={X: X_batch, y: y_batch})
            print(iteration, "\tMSE:", mse)

    saver.save(sess, mfile) # not shown in the book
```

```text
(0, '\tMSE:', 13.8245)
(100, '\tMSE:', 0.57806575)
(200, '\tMSE:', 0.17610031)
(300, '\tMSE:', 0.086107321)
```

```python
with tf.Session() as sess:
    saver.restore(sess, mfile)
    X_new = f(np.array(t_instance[:-1].reshape(-1, n_steps, n_inputs)))
    y_pred = sess.run(outputs, feed_dict={X: X_new})

plt.title("Testing the model", fontsize=14)
plt.plot(t_instance[:-1], f(t_instance[:-1]), "bo", markersize=10, label="instance")
plt.plot(t_instance[1:], f(t_instance[1:]), "w*", markersize=10, label="target")
plt.plot(t_instance[1:], y_pred[0,:,0], "r.", markersize=10, label="prediction")
plt.legend(loc="upper left")
plt.xlabel("Time")
plt.savefig('time_02.png')
```

```python
t_start = 29.
y_pred = np.array([f(t_start+i*resolution) for i in range(20)]).reshape(1,20,1)
res = []
n_more = 20
with tf.Session() as sess:
    saver.restore(sess, mfile)
    for i in range(n_more):
    	y_pred = sess.run(outputs, feed_dict={X: y_pred})
    	y_pred_last = y_pred[0,-1,0]
    	res.append(y_pred_last)
    print res
```

```text
[-5.8834252, -5.0631075, -4.1249928, -9.0933685, -41.177116, -153.64326, -473.6687, -1248.9138, -2962.9839, -6417.5703, -12782.941, -23587.061, -40725.324, -65648.945, -97754.734, -132288.48, -156914.47, -144039.14, -51826.223, 175098.73]
```

```python
t = np.linspace(t_min, t_max, int((t_max - t_min) / resolution))
y = f(t)
plt.plot(t,y)
t2 = [t_start+i*resolution for i in range(n_more)]
plt.plot(t2,res,'r.')
plt.savefig('time_04.png')
```

```text
[29.0, 29.1, 29.2, 29.3, 29.4, 29.5, 29.6, 29.7, 29.8, 29.9, 30.0, 30.1, 30.2, 30.3, 30.4, 30.5, 30.6, 30.7, 30.8, 30.9, 31.0, 31.1, 31.2, 31.3, 31.4, 31.5, 31.6, 31.7, 31.8, 31.9, 32.0, 32.1, 32.2, 32.3, 32.4, 32.5, 32.6, 32.7, 32.8, 32.9, 33.0, 33.1, 33.2, 33.3, 33.4, 33.5, 33.6, 33.7, 33.8, 33.9, 34.0, 34.1, 34.2, 34.3, 34.4, 34.5, 34.6, 34.7, 34.8, 34.9, 35.0, 35.1, 35.2, 35.3, 35.4, 35.5, 35.6, 35.7, 35.8, 35.9, 36.0, 36.1, 36.2, 36.3, 36.4, 36.5, 36.6, 36.7, 36.8, 36.9, 37.0, 37.1, 37.2, 37.3, 37.4, 37.5, 37.6, 37.7, 37.8, 37.9, 38.0, 38.1, 38.2, 38.3, 38.4, 38.5, 38.6, 38.7, 38.8, 38.9]
[-5.8834252, -5.0631075, -4.1249928, -9.0933685, -41.177116, -153.64326, -473.6687, -1248.9138, -2962.9839, -6417.5703, -12782.941, -23587.061, -40725.324, -65648.945, -97754.734, -132288.48, -156914.47, -144039.14, -51826.223, 175098.73, 630088.06, 1517119.6, 3098140.5, 5800337.5, 10086815.0, 16679038.0, 26258170.0, 39617620.0, 58068640.0, 84255240.0, 1.2029465e+08, 1.6930414e+08, 2.3406667e+08, 3.1807875e+08, 4.2244995e+08, 5.5233869e+08, 7.1302272e+08, 9.1169446e+08, 1.1542769e+09, 1.4471328e+09, 1.7969559e+09, 2.2125389e+09, 2.7092419e+09, 3.3063593e+09, 4.0266545e+09, 4.8892877e+09, 5.9200978e+09, 7.129431e+09, 8.5373957e+09, 1.0151328e+10, 1.1901251e+10, 1.3610634e+10, 1.5092417e+10, 1.5849656e+10, 1.5294643e+10, 1.2229847e+10, 4.8435374e+09, -1.0221348e+10, -3.680607e+10, -7.590862e+10, -1.3062527e+11, -2.0005711e+11, -2.8449862e+11, -3.8383606e+11, -5.0110995e+11, -6.3909829e+11, -8.0108218e+11, -9.9106095e+11, -1.2136582e+12, -1.4740898e+12, -1.7784875e+12, -2.1339184e+12, -2.5484515e+12, -3.0313168e+12, -3.5930615e+12, -4.2457099e+12, -5.0033076e+12, -5.8836721e+12, -6.9045967e+12, -8.0861237e+12, -9.4508543e+12, -1.1028566e+13, -1.284908e+13, -1.4945924e+13, -1.7354269e+13, -2.01129e+13, -2.3265888e+13, -2.6872803e+13, -3.0981394e+13, -3.5642231e+13, -4.0909996e+13, -4.6843455e+13, -5.3495579e+13, -6.092698e+13, -6.9219664e+13, -7.844356e+13, -8.8672569e+13, -9.9983567e+13, -1.1245521e+14, -1.2618673e+14]
```





















