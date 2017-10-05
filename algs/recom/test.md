
```python
import matplotlib.pyplot as plt
from pathlib import Path
from zipfile import ZipFile
from keras.layers import Input, Embedding, Flatten, merge, Dense, Dropout, Lambda
from keras.models import Model
import keras.backend as K
import pandas as pd, zipfile
from sklearn.metrics import mean_squared_error, mean_absolute_error

ML_100K_FOLDER = Path('ml-100k')
icols = ["name", "date", "genre", "url"]
icols += ["f" + str(x) for x in range(19)]  # unused feature names
rcols  = ["user_id", "item_id", "rating", "timestamp"]
with zipfile.ZipFile('ml-100k.zip', 'r') as z:
    all_ratings = pd.read_csv(z.open('ml-100k/u.data'), sep='\t', names=rcols)
    items = pd.read_csv(z.open('ml-100k/u.item'), sep='|',names=icols, encoding='latin-1')
    
items.fillna(value="01-Jan-1997", inplace=True)

from sklearn.model_selection import train_test_split

ratings_train, ratings_test = train_test_split(
    all_ratings, test_size=0.2, random_state=0)
```

```python
print ratings_train.shape
```

```text
(80000, 4)
```

```python
input_x = tf.placeholder(tf.int32, [None, 2])
input_y = tf.placeholder(tf.float32, [None, 1])

```













