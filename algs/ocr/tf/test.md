
```python
from PIL import Image
import util

w = 512; h = 64

(t,s) = util.randomstring()
print s, t
res = util.paint_text(s,w,h,rotate=True,ud=True,multi_fonts=True)
res = res.reshape(h,w)
print res.shape
plt.imshow(res,cmap='gray',interpolation="none")
plt.savefig('out1.png')
#dataset = res.reshape(1,w,h,1)
#print dataset.shape
```

```text
NR60GpiRs [39, 43, 58, 52, 32, 15, 8, 43, 18]
(64, 512)
```
```python
import tensorflow as tf
import pandas as pd
import numpy as np

w = 512; h = 64

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

def get_minibatch(batch_size=1):
    vals = []; labels = []
    for i in range(batch_size):
    	(idxs,str) = util.randomstring()
    	res = util.paint_text(str,w,h,rotate=True,ud=True,multi_fonts=True)
    	vals.append(res)    
    	labels.append(idxs)
    return np.reshape(vals, (batch_size,w,h,1)), sparse_tuple_from(labels)

data, labels = get_minibatch(batch_size=1)
print data.shape
print labels

```

```text
(1, 512, 64, 1)
(array([[ 0,  0],
       [ 0,  1],
       [ 0,  2],
       [ 0,  3],
       [ 0,  4],
       [ 0,  5],
       [ 0,  6],
       [ 0,  7],
       [ 0,  8],
       [ 0,  9],
       [ 0, 10],
       [ 0, 11],
       [ 0, 12]]), array([58, 44, 40, 39, 18, 53, 37, 53, 43, 54,  3, 17,  0], dtype=int32), array([ 1, 13]))
```













