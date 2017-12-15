
```python
zip = '/home/burak/Downloads/goog_voice_train.zip'
z = zipfile.ZipFile(zip, 'r')
z.close()
```







```python
from tensorflow.contrib.framework.python.ops import audio_ops as contrib_audio
import tensorflow as tf, re
import zipfile, pandas as pd, random
import pandas as pd, scipy.io.wavfile
import numpy as np, io, os

labels = ['down','go','left','no','off','on','right','stop','up','yes']

zip = '/home/burak/Downloads/goog_voice_train.zip'
import zipfile, pandas as pd, random
import scipy.io.wavfile, io
with zipfile.ZipFile(zip, 'r') as z: files = z.namelist()

noise_files = [x for x in files if 'noise.wav' in x]

files =  [x for x in files if '_background' not in x]

files = np.array([x for x in files if  '.wav' in x] )

z = zipfile.ZipFile(zip, 'r')
```

```python
sample_rate = 16000

def get_minibatch(batch_size, training=True):
    res = np.zeros((batch_size, 16000))
    y = np.zeros((batch_size,len(labels)+2 ))
    for i in range(batch_size):
      f = random.choice(files)          
      if random.choice(range(10)) != 0:
           label = re.findall(".*/(.*?)/.*?.wav",f)[0]
           if label in labels:
                y[i, labels.index(label)] = 1.0
           else:
                y[i, len(labels)] = 1.0 # unknown
           wav = io.BytesIO(z.open(f).read())
           v = scipy.io.wavfile.read(wav)
           #print f, v[1].shape
           res[i, 0:len(v[1])] = v[1]
      else: # silence, use generated data
           #print 'noise', f
           nf = random.choice(noise_files)
           wav = io.BytesIO(z.open(nf).read())
           v = scipy.io.wavfile.read(wav)
           chunks = int(len(v[1]) / sample_rate) - 1
           chosen_chunk = random.choice(range(chunks))
           fr = int(chosen_chunk * sample_rate)
           to = int((chosen_chunk+1)*sample_rate)
           chunk_byte = v[1][fr:to]
           res[i, :] = chunk_byte
           y[i, len(labels)+1] = 1.0 # silence
                                  
    return res,y

x,y = get_minibatch(10)

```

```python
print y
```

```text
[[ 0.  0.  0.  0.  0.  0.  0.  0.  0.  0.  1.  0.]
 [ 0.  0.  0.  0.  0.  0.  1.  0.  0.  0.  0.  0.]
 [ 0.  0.  0.  0.  0.  0.  0.  0.  0.  0.  1.  0.]
 [ 0.  0.  0.  0.  0.  0.  0.  0.  0.  0.  0.  1.]
 [ 0.  0.  0.  0.  0.  0.  0.  0.  0.  0.  1.  0.]
 [ 0.  1.  0.  0.  0.  0.  0.  0.  0.  0.  0.  0.]
 [ 0.  0.  0.  0.  0.  0.  0.  0.  0.  0.  1.  0.]
 [ 0.  0.  0.  0.  0.  1.  0.  0.  0.  0.  0.  0.]
 [ 0.  0.  0.  0.  0.  0.  1.  0.  0.  0.  0.  0.]
 [ 0.  0.  0.  0.  0.  0.  0.  0.  0.  0.  1.  0.]]
```










