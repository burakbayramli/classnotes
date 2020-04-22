
```python
import tensorflow as tf
import pandas as pd
import numpy as np

def sparse_tuple_from(sequences, dtype=np.int32):
    indices = []
    values = []
    for n, seq in enumerate(sequences):
        indices.extend(zip([n] * len(seq), range(len(seq))))
        values.extend(seq)
    indices = np.asarray(indices, dtype=np.int64)
    values = np.asarray(values, dtype=dtype)
    shape = np.asarray([len(sequences), np.asarray(indices).max(0)[1] + 1], dtype=np.int64)
    return indices, values, shape


train_targets_0 = [0, 1, 2, 1, 0]
# dimensions are time x depth
train_inputs_0 = np.asarray(
    [[0.633766, 0.221185, 0.0917319, 0.0129757, 0.0142857, 0.0260553],
     [0.111121, 0.588392, 0.278779, 0.0055756, 0.00569609, 0.010436],
     [0.0357786, 0.633813, 0.321418, 0.00249248, 0.00272882, 0.0037688],
     [0.0663296, 0.643849, 0.280111, 0.00283995, 0.0035545, 0.00331533],
     [0.458235, 0.396634, 0.123377, 0.00648837, 0.00903441, 0.00623107]],
    dtype=np.float32)

train_targets_1 = [0, 1, 1, 0]

train_inputs_1 = np.asarray(
    [[0.30176, 0.28562, 0.0831517, 0.0862751, 0.0816851, 0.161508],
     [0.24082, 0.397533, 0.0557226, 0.0546814, 0.0557528, 0.19549],
     [0.230246, 0.450868, 0.0389607, 0.038309, 0.0391602, 0.202456],
     [0.280884, 0.429522, 0.0326593, 0.0339046, 0.0326856, 0.190345],
     [0.423286, 0.315517, 0.0338439, 0.0393744, 0.0339315, 0.154046]],
    dtype=np.float32)

train_targets_2 = [0, 1, 1, 0]

train_inputs_2 = np.asarray(
    [[0.0, 0.0, 1.0, 0.0, 0.0, 0.0],
     [0.0, 0.0, 1.0, 0.0, 0.0, 0.0],
     [0.0, 0.0, 1.0, 0.0, 0.0, 0.0],
     [0.0, 0.0, 1.0, 0.0, 0.0, 0.0],
     [0.0, 0.0, 1.0, 0.0, 0.0, 0.0]],
    dtype=np.float32)
```

```python
import tensorflow as tf

train_seq_len = [5]
num_features = 6

tf.reset_default_graph()

targets = tf.sparse_placeholder(tf.int32)
logits1 = tf.placeholder(tf.float32, [None, num_features] )
logits2 = tf.reshape(logits1, [1, -1, num_features])
logits3 = tf.transpose(logits2, (1, 0, 2))
seq_len = tf.placeholder(tf.int32, [None])
loss = tf.nn.ctc_loss(targets, logits3, seq_len)
decoded, log_prob = tf.nn.ctc_greedy_decoder(logits3, seq_len)
          
with tf.Session() as sess:

     sess.run(tf.global_variables_initializer())

     train_targets = sparse_tuple_from([train_targets_2])
     
     feed = { logits1: train_inputs_2, targets: train_targets, seq_len: train_seq_len }
     res1 = sess.run(logits1, feed)     
     #print res1
     print 'logits1.shape', logits1.shape
     res2 = sess.run(logits2, feed)     
     #print res2
     print 'logits2.shape', logits2.shape
     res3 = sess.run(loss, feed)     
     #print res3
     
     feed_dec = { logits1: train_inputs_2, seq_len: train_seq_len }
     decoded_res = sess.run(decoded, feed_dec)     
     print decoded_res

```

```text
logits1.shape (?, 6)
logits2.shape (1, ?, 6)
[SparseTensorValue(indices=array([[0, 0]]), values=array([2]), dense_shape=array([1, 1]))]
```




































