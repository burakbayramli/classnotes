
```python
import tensorflow as tf
from tensorflow.python.framework import constant_op
from tensorflow.python.framework import dtypes
from tensorflow.python.framework import errors_impl
from tensorflow.python.framework import sparse_tensor

def SimpleSparseTensorFrom(x):
  x_ix = []
  x_val = []
  for batch_i, batch in enumerate(x):
    for time, val in enumerate(batch):
      x_ix.append([batch_i, time])
      x_val.append(val)
  x_shape = [len(x), np.asarray(x_ix).max(0)[1] + 1]
  x_ix = constant_op.constant(x_ix, dtypes.int64)
  x_val = constant_op.constant(x_val, dtypes.int32)
  x_shape = constant_op.constant(x_shape, dtypes.int64)
  res = sparse_tensor.SparseTensor(x_ix, x_val, x_shape)
  return res

train_targets = [[0, 1, 2, 1, 0]]
# dimensions are time x depth
train_inputs = np.asarray(
    [[0.633766, 0.221185, 0.0917319, 0.0129757, 0.0142857, 0.0260553],
     [0.111121, 0.588392, 0.278779, 0.0055756, 0.00569609, 0.010436],
     [0.0357786, 0.633813, 0.321418, 0.00249248, 0.00272882, 0.0037688],
     [0.0663296, 0.643849, 0.280111, 0.00283995, 0.0035545, 0.00331533],
     [0.458235, 0.396634, 0.123377, 0.00648837, 0.00903441, 0.00623107]],
    dtype=np.float32)

```

```text
SparseTensor(indices=Tensor("Const:0", shape=(5, 2), dtype=int64), values=Tensor("Const_1:0", shape=(5,), dtype=int32), dense_shape=Tensor("Const_2:0", shape=(2,), dtype=int64))
```

```python
import tensorflow as tf

train_seq_len = 5
num_features = 6

tf.reset_default_graph()

targets = SimpleSparseTensorFrom(train_targets)
print targets

targets = tf.sparse_placeholder(tf.int32)
logits1 = tf.placeholder(tf.float32, [None, num_features] )
logits2 = tf.reshape(logits1, [1, -1, num_features])
logits3 = tf.transpose(logits2, (1, 0, 2))
seq_len = tf.placeholder(tf.int32, [None])
loss = tf.nn.ctc_loss(targets, logits3, seq_len)
          
with tf.Session() as sess:

     sess.run(tf.global_variables_initializer())
     print sess.run(targets.eval())
     #feed = { logits1: train_inputs, targets: targets }
     #feed = { logits1: train_inputs}
     #res3 = sess.run(logits3, feed)
     #res4 = sess.run(loss, feed)

```

```text
SparseTensor(indices=Tensor("Const:0", shape=(5, 2), dtype=int64), values=Tensor("Const_1:0", shape=(5,), dtype=int32), dense_shape=Tensor("Const_2:0", shape=(2,), dtype=int64))
```






















