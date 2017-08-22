

```python
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
n_neurons = 200
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
```

```text
50 20
3000
('\tMSE:', 3.6949453)
('\tMSE:', 2.889744)
('\tMSE:', 2.1613321)
('\tMSE:', 1.471259)
('\tMSE:', 0.8101126)
('\tMSE:', 0.59605801)
('\tMSE:', 0.94443166)
('\tMSE:', 0.64896882)
('\tMSE:', 0.39506903)
('\tMSE:', 0.31831622)
('\tMSE:', 0.31820294)
('\tMSE:', 0.32339942)
('\tMSE:', 0.31279948)
('\tMSE:', 0.27872539)
('\tMSE:', 0.23611405)
('\tMSE:', 0.19936807)
('\tMSE:', 0.18842071)
('\tMSE:', 0.20183484)
('\tMSE:', 0.20521601)
('\tMSE:', 0.1878894)
('\tMSE:', 0.1656661)
('\tMSE:', 0.15171456)
('\tMSE:', 0.14757751)
('\tMSE:', 0.14785361)
('\tMSE:', 0.14748633)
('\tMSE:', 0.14424694)
('\tMSE:', 0.13854083)
('\tMSE:', 0.13253471)
('\tMSE:', 0.12858653)
('\tMSE:', 0.12774439)
('\tMSE:', 0.12812631)
('\tMSE:', 0.12664956)
('\tMSE:', 0.12298907)
('\tMSE:', 0.11914897)
('\tMSE:', 0.11675814)
('\tMSE:', 0.11569568)
('\tMSE:', 0.11490765)
('\tMSE:', 0.11348779)
('\tMSE:', 0.11121773)
('\tMSE:', 0.10858805)
('\tMSE:', 0.10632709)
('\tMSE:', 0.10482271)
('\tMSE:', 0.10381659)
('\tMSE:', 0.10267785)
('\tMSE:', 0.10102737)
('\tMSE:', 0.099052012)
('\tMSE:', 0.097225048)
('\tMSE:', 0.095828474)
('\tMSE:', 0.094747514)
('\tMSE:', 0.093683079)
('\tMSE:', 0.092421569)
('\tMSE:', 0.090984739)
('\tMSE:', 0.089552283)
('\tMSE:', 0.088303559)
('\tMSE:', 0.087264463)
('\tMSE:', 0.086291812)
('\tMSE:', 0.085230716)
('\tMSE:', 0.084047563)
```

```python
t_start = 10.
y_pred = np.array([f(t_start+i*resolution) for i in range(20)]).reshape(1,20,1)
print y_pred
```

```text
[[[-2.33815341]
  [-2.27076041]
  [-2.20216979]
  [-2.13262364]
  [-2.06236608]
  [-1.99164271]
  [-1.92069994]
  [-1.84978437]
  [-1.77914221]
  [-1.70901859]
  [-1.63965699]
  [-1.57129863]
  [-1.50418179]
  [-1.43854129]
  [-1.37460785]
  [-1.31260752]
  [-1.25276107]
  [-1.19528352]
  [-1.14038348]
  [-1.08826271]]]
```

```python
res = []
n_more = 1000
with tf.Session() as sess:
    saver.restore(sess, mfile)
    for i in range(n_more):
    	y_pred = sess.run(outputs, feed_dict={X: y_pred})
    	y_pred_last = y_pred[0,-1,0]
    	res.append(y_pred_last)
    print res[:4]
```

```text
[-0.076219745, 0.22508824, 0.46822298, 0.66771907]
```

```python
t = np.linspace(t_min, t_max, int((t_max - t_min) / resolution))
y = f(t)
plt.plot(t,y)
t2 = [t_start+i*resolution for i in range(1000)]
plt.plot(t2,res,'r.')
plt.savefig('time_03.png')
```

```text
[30.0, 30.01, 30.02, 30.03, 30.04, 30.05, 30.06, 30.07, 30.08, 30.09, 30.1, 30.11, 30.12, 30.13, 30.14, 30.15, 30.16, 30.17, 30.18, 30.19, 30.2, 30.21, 30.22, 30.23, 30.24, 30.25, 30.26, 30.27, 30.28, 30.29, 30.3, 30.31, 30.32, 30.33, 30.34, 30.35, 30.36, 30.37, 30.38, 30.39, 30.4, 30.41, 30.42, 30.43, 30.44, 30.45, 30.46, 30.47, 30.48, 30.49, 30.5, 30.51, 30.52, 30.53, 30.54, 30.55, 30.56, 30.57, 30.58, 30.59, 30.6, 30.61, 30.62, 30.63, 30.64, 30.65, 30.66, 30.67, 30.68, 30.69, 30.7, 30.71, 30.72, 30.73, 30.74, 30.75, 30.76, 30.77, 30.78, 30.79, 30.8, 30.81, 30.82, 30.83, 30.84, 30.85, 30.86, 30.87, 30.88, 30.89, 30.9, 30.91, 30.92, 30.93, 30.94, 30.95, 30.96, 30.97, 30.98, 30.99]
```





















