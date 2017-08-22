

```python
np.random.seed(1)

t_min, t_max = 0, 30
resolution = 0.1

def f(t):
    return t * np.sin(t) / 3 + 2 * np.sin(t*5)

def next_batch(batch_size, n_steps):
    t0 = np.random.rand(batch_size, 1) * (t_max - t_min - n_steps * resolution)
    Ts = t0 + np.arange(0., n_steps + 1) * resolution
    print Ts
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
[[ 16.58794156  16.68794156  16.78794156  16.88794156]
 [ 20.49976768  20.59976768  20.69976768  20.79976768]
 [ 13.44974895  13.54974895  13.64974895  13.74974895]
 [ 18.66077841  18.76077841  18.86077841  18.96077841]]
[[-2.35844326 -2.65475091 -3.38969473 -4.45689625]
 [ 8.6561097   8.00416163  6.97801504  5.79912712]
 [ 1.55173044  1.80116401  2.49618869  3.52725931]
 [-2.7869727  -1.41335638  0.18271955  1.75728351]]
[[-2.35844326 -2.65475091 -3.38969473]
 [ 8.6561097   8.00416163  6.97801504]
 [ 1.55173044  1.80116401  2.49618869]
 [-2.7869727  -1.41335638  0.18271955]]
[[-2.65475091 -3.38969473 -4.45689625]
 [ 8.00416163  6.97801504  5.79912712]
 [ 1.80116401  2.49618869  3.52725931]
 [-1.41335638  0.18271955  1.75728351]]
```

```python
np.random.seed(1)
batch_size = 2
n_steps = 3
res = next_batch(batch_size, n_steps)
print len(res)
print res[0]
print '----------'
print res[1]
```

```text
[[ 12.38555354  12.48555354  12.58555354  12.68555354]
 [ 21.39363746  21.49363746  21.59363746  21.69363746]]
2
[[[-2.3141651 ]
  [-1.12233854]
  [ 0.27200624]]

 [[ 4.31878159]
  [ 4.63599743]
  [ 4.61528818]]]
----------
[[[-1.12233854]
  [ 0.27200624]
  [ 1.6253068 ]]

 [[ 4.63599743]
  [ 4.61528818]
  [ 4.112844  ]]]
```

```python
np.random.seed(1)
for b in next_batch(4, 3):
    print b
    print '---'
```

```text
[[[-2.3141651 ]
  [-1.12233854]
  [ 0.27200624]]

 [[ 4.31878159]
  [ 4.63599743]
  [ 4.61528818]]

 [[ 0.03397153]
  [ 0.99207952]
  [ 1.71474727]]

 [[ 2.87376693]
  [ 3.00044595]
  [ 2.62695206]]]
---
[[[-1.12233854]
  [ 0.27200624]
  [ 1.6253068 ]]

 [[ 4.63599743]
  [ 4.61528818]
  [ 4.112844  ]]

 [[ 0.99207952]
  [ 1.71474727]
  [ 2.02731967]]

 [[ 3.00044595]
  [ 2.62695206]
  [ 1.77847377]]]
---
```



```python
n_steps = 20
t_instance = np.linspace(12.2, 12.2 + resolution * (n_steps + 1), n_steps + 1)

plt.figure(figsize=(11,4))
plt.subplot(121)
plt.title("A time series (generated)", fontsize=14)
plt.plot(t, f(t), label=r"$t . \sin(t) / 3 + 2 . \sin(5t)$")
plt.plot(t_instance[:-1], f(t_instance[:-1]), "b-", linewidth=3, label="A training instance")
plt.legend(loc="lower left", fontsize=14)
plt.axis([0, 30, -17, 13])
plt.xlabel("Time")
plt.ylabel("Value")

plt.subplot(122)
plt.title("A training instance", fontsize=14)
plt.plot(t_instance[:-1], f(t_instance[:-1]), "bo", markersize=10, label="instance")
plt.plot(t_instance[1:], f(t_instance[1:]), "w*", markersize=10, label="target")
plt.legend(loc="upper left")
plt.xlabel("Time")
plt.savefig('time_01.png')
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



























