

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
t_start = 29.
y_pred = np.array([f(t_start+i*resolution) for i in range(20)]).reshape(1,20,1)
res = []
n_more = 30
with tf.Session() as sess:
    saver.restore(sess, mfile)
    for i in range(n_more):
    	y_pred_new = sess.run(outputs, feed_dict={X: y_pred})
	y_pred[0,0:n_steps-1,0] = y_pred[0,1:n_steps,0]
	y_pred[0,-1,0] = y_pred_new[0,-1,0]
    	res.append(y_pred_new[0,-1,0])
    print res
```

```text
[-5.8834252, -5.1626925, -3.9008322, -2.2296906, -0.25611138, 1.7267134, 3.5069623, 4.9867578, 5.9304171, 6.3465719, 6.3335509, 5.8966804, 5.3917837, 4.8942008, 4.4733119, 4.3858418, 4.5858736, 5.0024323, 5.4861588, 5.9528174, 6.2715974, 6.3185401, 6.1884699, 5.9183979, 5.6478801, 5.4518418, 5.3501239, 5.3882165, 5.4763312, 5.5698323]
```

```python
t = np.linspace(t_min, t_max, int((t_max - t_min) / resolution))
y = f(t)
plt.plot(t,y)
t2 = [t_start+i*resolution for i in range(n_more)]
plt.plot(t2,res,'r.')
y2 = np.array([f(tt) for tt in t2])
plt.plot(t2,y2,'g.')
plt.savefig('time_04.png')
```






















