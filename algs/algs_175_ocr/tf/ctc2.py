from __future__ import absolute_import
from __future__ import division

import numpy as np
import tensorflow as tf
from tensorflow.python.framework import constant_op
from tensorflow.python.framework import dtypes
from tensorflow.python.framework import errors_impl
from tensorflow.python.framework import sparse_tensor
from tensorflow.python.ops import ctc_ops
from tensorflow.python.platform import test


def SimpleSparseTensorFrom(x):
  """Create a very simple SparseTensor with dimensions (batch, time).
  Args:
    x: a list of lists of type int
  Returns:
    x_ix and x_val, the indices and values of the SparseTensor<2>.
  """
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
  print 'res', res
  return res


# Input and ground truth from Alex Graves' implementation.

# max_time_steps == 7
depth = 6

# seq_len_0 == 5
targets_0 = [0, 1, 2, 1, 0]
loss_log_prob_0 = -3.34211
# dimensions are time x depth
input_prob_matrix_0 = np.asarray(
    [[0.633766, 0.221185, 0.0917319, 0.0129757, 0.0142857, 0.0260553],
     [0.111121, 0.588392, 0.278779, 0.0055756, 0.00569609, 0.010436],
     [0.0357786, 0.633813, 0.321418, 0.00249248, 0.00272882, 0.0037688],
     [0.0663296, 0.643849, 0.280111, 0.00283995, 0.0035545, 0.00331533],
     [0.458235, 0.396634, 0.123377, 0.00648837, 0.00903441, 0.00623107]],
    dtype=np.float32)

input_log_prob_matrix_0 = np.log(input_prob_matrix_0)

# seq_len_1 == 5
targets_1 = [0, 1, 1, 0]
loss_log_prob_1 = -5.42262
# dimensions are time x depth

input_prob_matrix_1 = np.asarray(
    [[0.30176, 0.28562, 0.0831517, 0.0862751, 0.0816851, 0.161508],
     [0.24082, 0.397533, 0.0557226, 0.0546814, 0.0557528, 0.19549],
     [0.230246, 0.450868, 0.0389607, 0.038309, 0.0391602, 0.202456],
     [0.280884, 0.429522, 0.0326593, 0.0339046, 0.0326856, 0.190345],
     [0.423286, 0.315517, 0.0338439, 0.0393744, 0.0339315, 0.154046]],
    dtype=np.float32)
input_log_prob_matrix_1 = np.log(input_prob_matrix_1)

print input_prob_matrix_1[0,:]

# len max_time_steps array of 2 x depth matrices
inputs = [
    np.vstack(
        [input_prob_matrix_0[t, :], input_prob_matrix_1[t, :]])
    for t in range(5)
] + 2 * [np.nan * np.ones((2, depth), np.float32)]

# convert inputs into [max_time x batch_size x depth tensor] Tensor
inputs = np.asarray(inputs, dtype=np.float32)
print inputs.shape

# len batch_size array of label vectors
print targets_0
labels = SimpleSparseTensorFrom([targets_0, targets_1])

# batch_size length vector of sequence_lengths
seq_lens = np.array([5, 5], dtype=np.int32)

#self._testCTCLoss(inputs, seq_lens, labels)
inputs_t = constant_op.constant(inputs)

with tf.Session() as sess:
  #loss = ctc_ops.ctc_loss(inputs=inputs_t, labels=labels, sequence_length=seq_lens)
  loss2 = tf.nn.ctc_loss( labels=labels, inputs=inputs_t, sequence_length=seq_lens, time_major=True )
  print 'labels', labels.eval()
  print (loss2.eval())
  

