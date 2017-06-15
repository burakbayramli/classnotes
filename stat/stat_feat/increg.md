

```python
from sklearn.feature_extraction.text import HashingVectorizer
import numpy as np
import pandas as pd, csv
import scipy.sparse as sps

HASH = 30
vect = HashingVectorizer(decode_error='ignore',n_features=HASH)

def get_row(cols):
    with open("nes2.csv", 'r') as csvfile:
        rd = csv.reader(csvfile)
        headers = {k: v for v, k in enumerate(next(rd))}
        for row in rd:
            label = float(row[headers['income']])
            rrow = [x + str(row[headers[x]]) for x in headers if x in cols]
            X_train = vect.transform(rrow)
            yield X_train.tocoo(), label            

def get_minibatch(row_getter,size=10):
    X_train = sps.lil_matrix((size,HASH))
    y_train = []
    for i in range(size):
        cx,y = row_getter.next()
        for dummy,j,val in zip(cx.row, cx.col, cx.data): X_train[i,j] = val
        y_train.append(y)
    return X_train, y_train
    
cols = ['gender','income','race','occup1']
row_getter = get_row(cols)
X,y = get_minibatch(row_getter,size=1)
print X.todense()
print y
```

```text
[[ 0.  0.  0. -1.  0.  0.  0.  0.  0.  0.  0.  0.  1.  0.  0.  0.  0.  0.
   0.  0.  0.  0.  0.  0.  0.  1.  0.  0.  1.  0.]]
[4.0]
```

```python
from sklearn.linear_model import SGDRegressor
clf = SGDRegressor(random_state=1, n_iter=1)
row_getter = get_row(cols)
for i in range(10):
    X_train, y_train = get_minibatch(row_getter,1000)
    clf.partial_fit(X_train, y_train)

X_test,y_test = get_minibatch(row_getter,100)
print clf.score(X_test, y_test)    
```

```text
0.439458612416
```



































