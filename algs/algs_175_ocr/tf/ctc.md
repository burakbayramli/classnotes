

```python
import tensorflow as tf
out_charset="ABCDEFGHIJKLMNOPQRSTUVWXYZ"
pred = "HELO"
text = "HELLO"
indices = [[i] for i in range(0,len(text))]
values = [out_charset.index(c) for c in list(text)]
print values
shape = [len(text)]
with tf.Session() as sess:
     sess.run(tf.global_variables_initializer())
     label = tf.SparseTensorValue(indices,values,shape)
     label = tf.convert_to_tensor_or_sparse_tensor(label)
     print label.eval()
     label = tf.serialize_sparse(label) # needed for batching
```

```text
[7, 4, 11, 11, 14]
SparseTensorValue(indices=array([[0],
       [1],
       [2],
       [3],
       [4]]), values=array([ 7,  4, 11, 11, 14], dtype=int32), dense_shape=array([5]))
```


























