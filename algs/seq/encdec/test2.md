
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

















