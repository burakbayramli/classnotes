
```python
import tensorflow as tf
a = tf.constant([[1.], [2.]])
b = tf.constant([1., 2.])
c = tf.reduce_sum(a + b)
print tf.Session().run(c)
```

```text
12.0
```
