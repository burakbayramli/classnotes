
```python
zip = '/home/burak/Downloads/goog_voice_train.zip'
import zipfile
with zipfile.ZipFile(zip, 'r') as z:
     res = z.namelist()
for x in res:
    if 'background' in x: print x
```

```text
train/audio/_background_noise_/
train/audio/_background_noise_/README.md
train/audio/_background_noise_/doing_the_dishes.wav
train/audio/_background_noise_/dude_miaowing.wav
train/audio/_background_noise_/exercise_bike.wav
train/audio/_background_noise_/pink_noise.wav
train/audio/_background_noise_/running_tap.wav
train/audio/_background_noise_/white_noise.wav
```



```python
print 1000 / 25
print 16000 / 40
```

```text
40
400
```


```python
from tensorflow.contrib.framework.python.ops import audio_ops as contrib_audio
import tensorflow as tf

init_op = tf.global_variables_initializer()

data = tf.placeholder(tf.float32, [None, 16000])

print data

stfts = tf.contrib.signal.stft(data, frame_length=400, frame_step=100, fft_length=512)

spec = tf.abs(stfts)

print spec

mfcc = contrib_audio.mfcc(spec,16000,dct_coefficient_count=26)

print mfcc

from tensorflow.python.ops import random_ops
s = np.random.rand(1,16000)
with tf.Session() as sess:
     sess.run(tf.global_variables_initializer())
     res = sess.run(spec, feed_dict={data: s })  
print res.shape

```

```text
Tensor("Placeholder_20:0", shape=(?, 16000), dtype=float32)
Tensor("Abs_9:0", shape=(?, 157, 257), dtype=float32)
Tensor("Mfcc_9:0", shape=(?, 157, 26), dtype=float32)
(1, 157, 257)
```


















