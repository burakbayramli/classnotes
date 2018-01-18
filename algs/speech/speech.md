


```python
import pandas as pd
import numpy as np
import tensorflow as tf
import scipy.io.wavfile, zipfile
import io, time, os, random, re

fs = 16000
batch_size = 20
num_epochs = 20
num_cell = 30
num_layers = 2
mfile = "/tmp/speech.ckpt"
train_dir = '/home/burak/Downloads/voice_cmd_small'
labels = ['up','down','yes','no']

```


```python
all_train_files = []
all_train_files2 = []

for d, r, f in os.walk(train_dir):
    for filename in f:
    	all_train_files.append(os.path.join(d,filename))

for x in all_train_files:
    if ".wav" in x: 
       label = re.findall(".*/(.*?)/.*?.wav",x)[0]
       if label in labels: all_train_files2.append(x)

all_train_files2 = np.array(all_train_files2)
print len(all_train_files2)
print all_train_files2[:5]

idx = np.random.permutation(np.arange(len(all_train_files2)))
N = float(len(indices))
tidx = idx[0:int(N*0.9)]
vidx = idx[int(N*0.9):]
train_files = all_train_files2[tidx]
val_files = all_train_files2[vidx]
```

```text
400
['/home/burak/Downloads/voice_cmd_small/up/1cc80e39_nohash_0.wav'
 '/home/burak/Downloads/voice_cmd_small/up/1b459024_nohash_0.wav'
 '/home/burak/Downloads/voice_cmd_small/up/1a073312_nohash_0.wav'
 '/home/burak/Downloads/voice_cmd_small/up/1eddce1d_nohash_1.wav'
 '/home/burak/Downloads/voice_cmd_small/up/1c6e5447_nohash_0.wav']
```

```python
def adj_volume(vec):
    vol_multiplier = np.mean(np.abs(vec)) / 500.
    if vol_multiplier == 0: return vec
    vnew = vec.astype(float) / vol_multiplier
    return vnew

def get_minibatch(batch_size, validation=False):
    files = train_files
    if validation: files = val_files
    res = np.zeros((batch_size, fs))
    y = np.zeros((batch_size,len(labels) ))
    for i in range(batch_size):
    	f = random.choice(files)
	wav = io.BytesIO(open(f).read())
	v = scipy.io.wavfile.read(wav)
	res[i, 0:len(v[1])] = adj_volume(v[1])
        label = re.findall(".*/(.*?)/.*?.wav",f)[0]
	y[i, labels.index(label)] = 1.0

    return res, y

x,y = get_minibatch(10)
print x.shape
print y
```

```text
(10, 16000)
[[ 0.  0.  1.  0.]
 [ 1.  0.  0.  0.]
 [ 0.  1.  0.  0.]
 [ 0.  1.  0.  0.]
 [ 0.  0.  1.  0.]
 [ 0.  0.  0.  1.]
 [ 0.  0.  0.  1.]
 [ 0.  0.  1.  0.]
 [ 1.  0.  0.  0.]
 [ 1.  0.  0.  0.]]
```

```python
import tensorflow as tf

dropout_prob = tf.placeholder(tf.float32)

data = tf.placeholder(tf.float32, [None, fs])

stfts = tf.contrib.signal.stft(data, frame_length=256, frame_step=64, fft_length=256)

fingerprint = tf.abs(stfts)

y = tf.placeholder(tf.float32, shape=[None, 12])

cells = []
for _ in range(num_layers):
    cell = tf.contrib.rnn.LSTMCell(num_cell)
    cell = tf.contrib.rnn.DropoutWrapper(cell, output_keep_prob=1-dropout_prob)
    cells.append(cell)
cell = tf.contrib.rnn.MultiRNNCell(cells)
output, states = tf.nn.dynamic_rnn(cell, fingerprint, dtype=tf.float32)
last = states[-1][0]

logits = tf.contrib.layers.fully_connected(inputs=last,
                                           num_outputs=12,
                                           activation_fn=None)

softmax = tf.nn.softmax_cross_entropy_with_logits(logits=logits,labels=y) 

cross_entropy = tf.reduce_mean(softmax)

train_step = tf.train.AdamOptimizer(0.001).minimize(cross_entropy)

correct_prediction = tf.equal(tf.argmax(y,1), tf.argmax(logits,1))

accuracy = (tf.reduce_mean(tf.cast(correct_prediction, tf.float32)))*100.

sess = tf.Session()

sess.run(tf.global_variables_initializer())

for i in range(num_epochs):
    x_batch, y_batch = get_minibatch(batch_size)
    if i % 5 == 0:
        acc = sess.run(accuracy,feed_dict={ data:x_batch, y:y_batch, dropout_prob: 0.0})
	val_x, val_y = get_minibatch(batch_size,validation=True)
        acc = sess.run(accuracy,feed_dict={data:val_x, y:val_y, dropout_prob: 0.0 })
        print i, 'accuracy', acc        
    sess.run(train_step,feed_dict={ data:x_batch, y:y_batch, dropout_prob: 0.5 })
    
    if i % 30 == 0: 
        saver.save(sess, mfile)
        acc = sess.run(accuracy,feed_dict={data:val_x, y:val_y, dropout_prob: 0.0 })
        print i, 'validation accuracy', acc
```





