
```python
import tensorflow as tf
import numpy as np
import data_helpers
from tensorflow.contrib import learn
from tensorflow.python.framework import ops

ops.reset_default_graph()
sess = tf.Session()

x_text, y = data_helpers.load_data_and_labels("./data/rt-polaritydata/rt-polarity.pos", "./data/rt-polaritydata/rt-polarity.neg")

max_document_length = max([len(x.split(" ")) for x in x_text])
print max_document_length
print x_text[:4]
vocab_processor = learn.preprocessing.VocabularyProcessor(max_document_length)
x = np.array(list(vocab_processor.fit_transform(x_text)))

np.random.seed(10)
shuffle_indices = np.random.permutation(np.arange(len(y)))
x_shuffled = x[shuffle_indices]
y_shuffled = y[shuffle_indices]

dev_sample_index = -1 * int(0.80 * float(len(y)))
x_train, x_dev = x_shuffled[:dev_sample_index], x_shuffled[dev_sample_index:]
y_train, y_dev = y_shuffled[:dev_sample_index], y_shuffled[dev_sample_index:]
print("Vocabulary Size: {:d}".format(len(vocab_processor.vocabulary_)))
print("Train/Dev split: {:d}/{:d}".format(len(y_train), len(y_dev)))
```

```text
56
["the rock is destined to be the 21st century 's new conan and that he 's going to make a splash even greater than arnold schwarzenegger , jean claud van damme or steven segal", "the gorgeously elaborate continuation of the lord of the rings trilogy is so huge that a column of words cannot adequately describe co writer director peter jackson 's expanded vision of j r r tolkien 's middle earth", 'effective but too tepid biopic', 'if you sometimes like to go to the movies to have fun , wasabi is a good place to start']
Vocabulary Size: 18758
Train/Dev split: 2133/8529
```

```python
print x_train[19]
```

```text
[   84  2733   249    38   182     1   995    35     9 18287  6323    58
 16066     9     0     0     0     0     0     0     0     0     0     0
     0     0     0     0     0     0     0     0     0     0     0     0
     0     0     0     0     0     0     0     0     0     0     0     0
     0     0     0     0     0     0     0     0]
```







