
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
print 'the_input',res[0]['the_input'].shape
print 'the_input',res[0]['the_input']
```

```text
['the_input', 'input_length', 'the_labels', 'label_length', 'source_str']
source str ['']
label_length [[ 1.]]
the_labels [[ 27.   1.   1.   1.   1.   1.   1.   1.   1.   1.   1.   1.   1.   1.
    1.   1.]]
the_labels (1, 16)
input_length [[ 30.]]
the_input (1, 128, 64, 1)
the_input [[[[ 1.        ]
   [ 1.        ]
   [ 1.        ]
   ..., 
   [ 0.89905836]
   [ 0.72517273]
   [ 0.58361949]]

  [[ 1.        ]
   [ 1.        ]
   [ 1.        ]
   ..., 
   [ 0.95834003]
   [ 0.82342244]
   [ 0.71497225]]

  [[ 1.        ]
   [ 1.        ]
   [ 1.        ]
   ..., 
   [ 1.        ]
   [ 0.91582534]
   [ 0.86362389]]

  ..., 
  [[ 0.62310733]
   [ 0.85694265]
   [ 1.        ]
   ..., 
   [ 0.91330392]
   [ 0.94402978]
   [ 0.93378349]]

  [[ 0.61633236]
   [ 0.73959987]
   [ 0.9106204 ]
   ..., 
   [ 0.97831418]
   [ 0.86011855]
   [ 0.81922948]]

  [[ 0.70092071]
   [ 0.71957706]
   [ 0.8120773 ]
   ..., 
   [ 1.        ]
   [ 0.9293999 ]
   [ 0.89497474]]]]
```














