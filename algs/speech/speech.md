
```python
import pandas as pd
import numpy as np
import tensorflow as tf
import scipy.io.wavfile, zipfile
import io, time, os, random, re

lrate = 0.001
numcep = 26
fs = 16000
batch_size = 20
num_epochs = 10000
num_cell = 256
num_layers = 4
mfile = "/tmp/speech.ckpt"
train_dir = '/home/burak/Downloads/voice_cmd_small'
labels = ['up','down','yes','no']

```


```python
all_train_files = []
train_files = []

for d, r, f in os.walk(train_dir):
    for filename in f:
    	all_train_files.append(os.path.join(d,filename))

for x in all_train_files:
    if ".wav" in x: 
       label = re.findall(".*/(.*?)/.*?.wav",x)[0]
       if label in labels: train_files.append(x)

print train_files[:5]
```

```text
['/home/burak/Downloads/voice_cmd_small/up/01d22d03_nohash_0.wav', '/home/burak/Downloads/voice_cmd_small/up/01b4757a_nohash_0.wav', '/home/burak/Downloads/voice_cmd_small/up/0137b3f4_nohash_3.wav', '/home/burak/Downloads/voice_cmd_small/up/023a61ad_nohash_0.wav', '/home/burak/Downloads/voice_cmd_small/up/01bb6a2a_nohash_0.wav']
```

```python
def adj_volume(vec):
    vol_multiplier = np.mean(np.abs(vec)) / 500.
    if vol_multiplier == 0: return vec
    vnew = vec.astype(float) / vol_multiplier
    return vnew

def get_minibatch(batch_size):
    res = np.zeros((batch_size, fs))
    y = np.zeros((batch_size,len(labels) ))
    for i in range(batch_size):
    	f = random.choice(train_files)
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
[[ 0.  0.  0.  1.]
 [ 1.  0.  0.  0.]
 [ 0.  0.  1.  0.]
 [ 0.  1.  0.  0.]
 [ 1.  0.  0.  0.]
 [ 0.  0.  0.  1.]
 [ 0.  0.  0.  1.]
 [ 1.  0.  0.  0.]
 [ 0.  0.  0.  1.]
 [ 1.  0.  0.  0.]]
```






