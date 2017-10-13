
```python
import tensorflow as tf
print tf
print tf.__version__
```

```text
<module 'tensorflow' from '/usr/local/lib/python2.7/dist-packages/tensorflow/__init__.pyc'>
1.3.0
```

```python
import pickle
pkl1 = open('data/vocab_en.pkl', 'rb')
en_vocab_dict = pickle.load(pkl1)
pkl1.close()
pkl2 = open('data/vocab_sp.pkl', 'rb')
sp_vocab_dict = pickle.load(pkl2)
pkl2.close()

print len(en_vocab_dict.keys())
```

```text
395
```


```python
import translate
import tensorflow as tf
FLAGS = translate.parameters()
FLAGS.en_vocab_size = len(en_vocab_dict.keys())
FLAGS.sp_vocab_size = len(sp_vocab_dict.keys())
tf.reset_default_graph()
with tf.Session() as sess:
     tf_model = translate.restore_model(sess, FLAGS)
```

```python
test_s = "I can easily start learning the economy"
```


































