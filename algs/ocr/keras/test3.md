
```python
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import pickle

f = open("dict.pkl")
res= pickle.load(f)
f.close()
```

```python
print res[0].keys()
print 'source str',res[0]['source_str']
print 'label_length',res[0]['label_length']
print 'the_labels',res[0]['the_labels']
print 'the_labels',res[0]['the_labels'].shape
print 'input_length',res[0]['input_length']
print 'the_input',type(res[0]['the_input'])
print 'the_input',res[0]['the_input'].shape
```

```text
['the_input', 'input_length', 'the_labels', 'label_length', 'source_str']
source str ['', '']
label_length [[ 1.]
 [ 1.]]
the_labels [[ 27.   1.   1.   1.   1.   1.   1.   1.   1.   1.   1.   1.   1.   1.
    1.   1.]
 [ 27.   1.   1.   1.   1.   1.   1.   1.   1.   1.   1.   1.   1.   1.
    1.   1.]]
the_labels (2, 16)
input_length [[ 30.]
 [ 30.]]
the_input <type 'numpy.ndarray'>
the_input (2, 128, 64, 1)
```

```python
import util

res = util.get_minibatch()
print 'source str',res[0]['source_str']
#print 'label_length',res[0]['label_length']
print 'the_labels',res[0]['the_labels']
print 'the_labels',res[0]['the_labels'].shape
#print 'input_length',res[0]['input_length']
print 'the_input',res[0]['the_input'].shape

img = res[0]['the_input']
img = img.reshape((util.h,util.w))
plt.imshow(img,cmap='gray',interpolation="none")
plt.savefig('out1.png')
```

```text
source str [u'pemetmutnnwsi']
the_labels [[ 15.   4.  12.   4.  19.  12.  20.  19.  13.  13.  22.  18.   8.   1.
    1.   1.]]
the_labels (1, 16)
the_input (1, 128, 64, 1)
```


































