

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

def get_minibatch(batch_size, silence_percent=0.10, unknown_percent=0.15):
    res = np.zeros((batch_size, fs))    
    y = np.zeros((batch_size,len(labels)+2 ))
    for i in range(batch_size):
        if random.choice(range(int(1/silence_percent))) == 0:
	   # silence
           f = random.choice(noise_files)
	   wav = io.BytesIO(open(f).read())
	   v = scipy.io.wavfile.read(wav)
           chunks = int(len(v[1]) / fs) - 1
           chosen_chunk = random.choice(range(chunks))
           fr = int(chosen_chunk * fs)
           to = int((chosen_chunk+1)*fs)
           chunk_byte = v[1][fr:to]
	   res[i, :] = chunk_byte
	   y[i, 11] = 1.0
        elif random.choice(range(int(1/unknown_percent))) == 0:
	   # unknown
           f = random.choice(unknown_files)
	   wav = io.BytesIO(open(f).read())
	   v = scipy.io.wavfile.read(wav)
	   res[i, 0:len(v[1])] = v[1]
	   y[i, 10] = 1.0
	else:
	   f = random.choice(train_files)
	   wav = io.BytesIO(open(f).read())
	   v = scipy.io.wavfile.read(wav)
	   res[i, 0:len(v[1])] = v[1]
           label = re.findall(".*/(.*?)/.*?.wav",f)[0]
	   y[i, labels.index(label)] = 1.0
	   
    return res, y
    
x,y = get_minibatch(20)
print x
```

```text
[[  0.00000000e+00   3.00000000e+00   5.00000000e+00 ...,   9.00000000e+00
    9.00000000e+00   6.00000000e+00]
 [ -4.00000000e+01  -1.10000000e+01   1.00000000e+01 ...,   0.00000000e+00
    0.00000000e+00   0.00000000e+00]
 [  1.25000000e+02   1.05600000e+03  -4.60000000e+01 ...,  -4.37000000e+02
   -1.40000000e+02  -1.21500000e+03]
 ..., 
 [ -6.06000000e+02  -9.90400000e+03  -1.92500000e+03 ...,   4.08000000e+02
    3.37000000e+02   1.14000000e+03]
 [  8.50000000e+01   1.35000000e+02   3.48000000e+02 ...,   3.04000000e+02
    2.13000000e+02   3.76000000e+02]
 [  4.63000000e+02   5.41000000e+02   5.61000000e+02 ...,   4.37000000e+02
    4.08000000e+02   4.46000000e+02]]
```


























