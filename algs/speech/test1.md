
```python
print 1000 / 25
```

```text
40
```


```python
from tensorflow.contrib.framework.python.ops import audio_ops as contrib_audio
import tensorflow as tf

init_op = tf.global_variables_initializer()

data = tf.placeholder(tf.float32, [None, 16000])

print data

stfts = tf.contrib.signal.stft(data, frame_length=40, frame_step=40, fft_length=512)

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
Tensor("Placeholder_18:0", shape=(?, 16000), dtype=float32)
Tensor("Abs_7:0", shape=(?, 400, 257), dtype=float32)
Tensor("Mfcc_7:0", shape=(?, 400, 26), dtype=float32)
(1, 400, 257)
```


















