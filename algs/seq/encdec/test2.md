
```python
import model, translate
import tensorflow as tf
FLAGS = translate.parameters()
FLAGS.en_vocab_size = 215
FLAGS.sp_vocab_size = 222
tf.reset_default_graph()
with tf.Session() as sess:
     tf_model = translate.restore_model(sess, FLAGS)
```

```text
WARNING:tensorflow:From model.py:137: all_variables (from tensorflow.python.ops.variables) is deprecated and will be removed after 2017-03-02.
Instructions for updating:
Please use tf.global_variables instead.
INFO:tensorflow:Restoring parameters from /tmp/checkpoints/model.ckpt
```
















