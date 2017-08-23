

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
t_start = 29.
y_pred = np.array([f(t_start+i*resolution) for i in range(20)]).reshape(1,20,1)
res = []
n_more = 100
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
[-0.6629504, 0.050956257, 0.73007333, 1.1516103, 1.3957571, 1.4715619, 1.5664629, 1.5943199, 1.6249188, 1.6481301, 1.6754981, 1.6976655, 1.7142141, 1.7293358, 1.7446311, 1.7588849, 1.7724127, 1.7853421, 1.7974946, 1.8091525, 1.8200871, 1.8305565, 1.840474, 1.8498775, 1.8587828, 1.8672113, 1.8751944, 1.8827505, 1.8899049, 1.8966782, 1.9030911, 1.9091623, 1.9149102, 1.9203525, 1.9255053, 1.9303836, 1.9350028, 1.9393762, 1.9435167, 1.9474375, 1.9511492, 1.9546635, 1.9579912, 1.9611418, 1.9641248, 1.9669486, 1.9696229, 1.9721546, 1.9745517, 1.9768213, 1.9789701, 1.9810046, 1.9829308, 1.9847544, 1.9864812, 1.988116, 1.9896638, 1.9911295, 1.9925171, 1.9938308, 1.995075, 1.9962525, 1.9973676, 1.9984232, 1.9994229, 2.0003691, 2.001265, 2.0021136, 2.0029171, 2.0036774, 2.0043974, 2.005079, 2.0057247, 2.006336, 2.0069144, 2.0074625, 2.0079811, 2.0084724, 2.0089371, 2.0093772, 2.009794, 2.0101888, 2.0105622, 2.010916, 2.011251, 2.0115683, 2.0118685, 2.0121527, 2.0124218, 2.0126767, 2.012918, 2.0131466, 2.0133626, 2.0135677, 2.0137613, 2.0139449, 2.0141187, 2.0142834, 2.0144393, 2.0145867]
```

```python
t = np.linspace(t_min, t_max, int((t_max - t_min) / resolution))
y = f(t)
plt.plot(t,y)
t2 = [t_start+i*resolution for i in range(n_more)]
plt.plot(t2,res,'r.')
plt.savefig('time_04.png')
```



















