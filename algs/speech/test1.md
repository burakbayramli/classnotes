

```python
import scipy.io.wavfile, zipfile
import io, time, os, random, re
fs = 16000
train_dir = '/home/burak/Downloads/train/audio'
labels = ['down','go','left','no','off','on','right','stop','up','yes']
all_labels = labels + ['unknown','silence']
```

```python
f = train_dir + '/sheila/019fa366_nohash_0.wav'
wav = io.BytesIO(open(f).read())
v = scipy.io.wavfile.read(wav)
print v[1]
vol_multiplier = np.mean(np.abs(v[1])) / 500.
print vol_multiplier
vnew = v[1].astype(float) / vol_multiplier
vnew = vnew.astype(np.int16)
scipy.io.wavfile.write('/tmp/tmp.wav', fs, vnew)    
```

```text
[0 3 5 ..., 4 4 3]
1.786834
```

```python
plt.plot(vnew)
plt.savefig('test1_1.png')
```


```python
all_train_files = []
for d, r, f in os.walk(train_dir):
    for filename in f:
    	all_train_files.append(os.path.join(d,filename))
noise_files = [x for x in all_train_files if "_background_noise_" in x and ".wav" in x]
train_files = []
unknown_files = []
for x in all_train_files:
    if ".wav" in x: 
       label = re.findall(".*/(.*?)/.*?.wav",x)[0]
       if label not in labels and x not in noise_files: unknown_files.append(x)
       elif x not in noise_files: train_files.append(x)

noise_chunks = []
for f in noise_files:
    wav = io.BytesIO(open(f).read())
    v = scipy.io.wavfile.read(wav)
    chunks = int(len(v[1]) / fs) - 1
    for i in range(chunks):
    	fr = int(i * fs)
    	to = int((i+1)*fs)
    	chunk_byte = v[1][fr:to]
	noise_chunks.append(chunk_byte)
    
```

```python
print all_train_files[:10]
print train_files[:10]
print unknown_files[:10]
```

```text
['/home/burak/Downloads/train/audio/_background_noise_/pink_noise.wav', '/home/burak/Downloads/train/audio/_background_noise_/README.md', '/home/burak/Downloads/train/audio/_background_noise_/white_noise.wav', '/home/burak/Downloads/train/audio/_background_noise_/exercise_bike.wav', '/home/burak/Downloads/train/audio/_background_noise_/running_tap.wav', '/home/burak/Downloads/train/audio/_background_noise_/dude_miaowing.wav', '/home/burak/Downloads/train/audio/_background_noise_/doing_the_dishes.wav', '/home/burak/Downloads/train/audio/on/789e4ee7_nohash_0.wav', '/home/burak/Downloads/train/audio/on/c103a2d5_nohash_0.wav', '/home/burak/Downloads/train/audio/on/36050ef3_nohash_3.wav']
['/home/burak/Downloads/train/audio/on/789e4ee7_nohash_0.wav', '/home/burak/Downloads/train/audio/on/c103a2d5_nohash_0.wav', '/home/burak/Downloads/train/audio/on/36050ef3_nohash_3.wav', '/home/burak/Downloads/train/audio/on/f953e1af_nohash_2.wav', '/home/burak/Downloads/train/audio/on/126a31d2_nohash_0.wav', '/home/burak/Downloads/train/audio/on/6414258b_nohash_0.wav', '/home/burak/Downloads/train/audio/on/2c7c33e8_nohash_0.wav', '/home/burak/Downloads/train/audio/on/39999a0f_nohash_0.wav', '/home/burak/Downloads/train/audio/on/763188c4_nohash_3.wav', '/home/burak/Downloads/train/audio/on/340c8b10_nohash_0.wav']
['/home/burak/Downloads/train/audio/happy/3efef882_nohash_0.wav', '/home/burak/Downloads/train/audio/happy/789e4ee7_nohash_0.wav', '/home/burak/Downloads/train/audio/happy/19b05529_nohash_0.wav', '/home/burak/Downloads/train/audio/happy/9080f6d3_nohash_0.wav', '/home/burak/Downloads/train/audio/happy/126a31d2_nohash_0.wav', '/home/burak/Downloads/train/audio/happy/c392e01d_nohash_0.wav', '/home/burak/Downloads/train/audio/happy/fffcabd1_nohash_0.wav', '/home/burak/Downloads/train/audio/happy/d750966e_nohash_0.wav', '/home/burak/Downloads/train/audio/happy/340c8b10_nohash_0.wav', '/home/burak/Downloads/train/audio/happy/fb24c826_nohash_0.wav']
```


```python
def adj_volume(vec):
    vol_multiplier = np.mean(np.abs(vec)) / 500.
    vnew = vec.astype(float) / vol_multiplier
    return vnew

def get_minibatch(batch_size, silence_percent=0.10, unknown_percent=0.15):
    res = np.zeros((batch_size, fs))
    y = np.zeros((batch_size,len(labels)+2 ))
    for i in range(batch_size):
        if random.choice(range(int(1/silence_percent))) == 0:	   
           chunk_byte = random.choice(noise_chunks)
	   res[i, :] = chunk_byte
	   y[i, all_labels.index('silence')] = 1.0 # silence
        elif random.choice(range(int(1/unknown_percent))) == 0:	   
           f = random.choice(unknown_files)
	   wav = io.BytesIO(open(f).read())
	   v = scipy.io.wavfile.read(wav)
	   res[i, 0:len(v[1])] = v[1]
	   y[i, all_labels.index('unknown')] = 1.0 # unknown
	else:
	   f = random.choice(train_files)
	   wav = io.BytesIO(open(f).read())
	   v = scipy.io.wavfile.read(wav)
	   if i==0: scipy.io.wavfile.write('/tmp/tmp1.wav', fs, v[1])	   
	   res[i, 0:len(v[1])] = v[1]
           label = re.findall(".*/(.*?)/.*?.wav",f)[0]
	   y[i, labels.index(label)] = 1.0
	   
    return res, y
    
x,y = get_minibatch(20)
print y
```

```text
[[ 0.  0.  0.  0.  0.  0.  1.  0.  0.  0.  0.  0.]
 [ 1.  0.  0.  0.  0.  0.  0.  0.  0.  0.  0.  0.]
 [ 0.  0.  0.  0.  0.  0.  0.  0.  0.  1.  0.  0.]
 [ 0.  0.  0.  0.  0.  0.  0.  0.  0.  0.  1.  0.]
 [ 0.  0.  0.  0.  1.  0.  0.  0.  0.  0.  0.  0.]
 [ 0.  0.  0.  0.  0.  0.  0.  0.  0.  0.  1.  0.]
 [ 0.  0.  0.  0.  0.  0.  0.  1.  0.  0.  0.  0.]
 [ 0.  0.  0.  0.  0.  0.  1.  0.  0.  0.  0.  0.]
 [ 0.  0.  0.  0.  0.  0.  1.  0.  0.  0.  0.  0.]
 [ 0.  0.  0.  0.  0.  0.  0.  0.  0.  0.  1.  0.]
 [ 0.  1.  0.  0.  0.  0.  0.  0.  0.  0.  0.  0.]
 [ 0.  0.  0.  0.  0.  0.  0.  0.  0.  1.  0.  0.]
 [ 0.  0.  0.  0.  0.  0.  1.  0.  0.  0.  0.  0.]
 [ 0.  0.  0.  0.  0.  0.  1.  0.  0.  0.  0.  0.]
 [ 0.  0.  0.  0.  0.  1.  0.  0.  0.  0.  0.  0.]
 [ 0.  0.  0.  1.  0.  0.  0.  0.  0.  0.  0.  0.]
 [ 0.  0.  1.  0.  0.  0.  0.  0.  0.  0.  0.  0.]
 [ 0.  0.  0.  0.  0.  0.  0.  0.  0.  0.  0.  1.]
 [ 0.  0.  0.  0.  0.  0.  0.  1.  0.  0.  0.  0.]
 [ 0.  0.  0.  1.  0.  0.  0.  0.  0.  0.  0.  0.]]
```


```python
vv = x[5,:].astype(np.int16)
scipy.io.wavfile.write('/tmp/tmp2.wav', fs, vv)
vv = x[8,:].astype(np.int16)
scipy.io.wavfile.write('/tmp/tmp3.wav', fs, vv)
i = 8
plt.specgram(x[i,:], Fs=fs, NFFT=1024)
print y[i]
plt.savefig('test1_2.png')
```

```text
[ 0.  0.  0.  0.  0.  0.  0.  1.  0.  0.  0.  0.]
```


```python
from tensorflow.contrib.framework.python.ops import audio_ops as contrib_audio
import tensorflow as tf

init_op = tf.global_variables_initializer()

data = tf.placeholder(tf.float32, [None, 16000])

print data

stfts = tf.contrib.signal.stft(data, frame_length=256, frame_step=64, fft_length=256)

spec = tf.abs(stfts)

from tensorflow.python.ops import random_ops
with tf.Session() as sess:
     sess.run(tf.global_variables_initializer())
     res = sess.run(spec, feed_dict={data: x[0].reshape(1,fs) })  
print res.shape
```

```text
Tensor("Placeholder:0", shape=(?, 16000), dtype=float32)
(1, 247, 129)
```





















