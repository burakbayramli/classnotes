
```python
train_file = "work/data/adult.data.csv"
test_file = "work/data/adult.test.csv"
import pandas as pd
COLUMNS = ["age", "workclass", "fnlwgt", "education", "education_num",
           "marital_status", "occupation", "relationship", "race", "gender",
           "capital_gain", "capital_loss", "hours_per_week", "native_country",
           "income_bracket"]
df_train = pd.read_csv(train_file, names=COLUMNS, skipinitialspace=True)
df_test = pd.read_csv(test_file, names=COLUMNS, skipinitialspace=True, skiprows=1)
LABEL_COLUMN = "label"
df_train[LABEL_COLUMN] = (df_train["income_bracket"].apply(lambda x: ">50K" in x)).astype(int)
df_test[LABEL_COLUMN] = (df_test["income_bracket"].apply(lambda x: ">50K" in x)).astype(int)
```

```python
CATEGORICAL_COLUMNS = ["workclass", "education", "marital_status", "occupation",
                       "relationship", "race", "gender", "native_country"]
CONTINUOUS_COLUMNS = ["age", "education_num", "capital_gain", "capital_loss", "hours_per_week"]

import tensorflow as tf

def input_fn(df):
  # Creates a dictionary mapping from each continuous feature column name (k) to
  # the values of that column stored in a constant Tensor.
  continuous_cols = {k: tf.constant(df[k].values) for k in CONTINUOUS_COLUMNS}
  # Creates a dictionary mapping from each categorical feature column name (k)
  # to the values of that column stored in a tf.SparseTensor.
  categorical_cols = {k: tf.SparseTensor(
      indices=[[i, 0] for i in range(df[k].size)],
      values=df[k].values,
      dense_shape=[df[k].size, 1]) for k in CATEGORICAL_COLUMNS}
  # Merges the two dictionaries into one.
  feature_cols = dict(continuous_cols.items() + categorical_cols.items())
  # Converts the label column into a constant Tensor.
  label = tf.constant(df[LABEL_COLUMN].values)
  # Returns the feature columns and the label.
  return feature_cols, label

def train_input_fn():
  return input_fn(df_train)

def eval_input_fn():
  return input_fn(df_test)

gender = tf.contrib.layers.sparse_column_with_keys(
  column_name="gender", keys=["Female", "Male"])

education = tf.contrib.layers.sparse_column_with_hash_bucket("education", hash_bucket_size=1000)
race = tf.contrib.layers.sparse_column_with_hash_bucket("race", hash_bucket_size=100)
marital_status = tf.contrib.layers.sparse_column_with_hash_bucket("marital_status", hash_bucket_size=100)
relationship = tf.contrib.layers.sparse_column_with_hash_bucket("relationship", hash_bucket_size=100)
workclass = tf.contrib.layers.sparse_column_with_hash_bucket("workclass", hash_bucket_size=100)
occupation = tf.contrib.layers.sparse_column_with_hash_bucket("occupation", hash_bucket_size=1000)
native_country = tf.contrib.layers.sparse_column_with_hash_bucket("native_country", hash_bucket_size=1000)
age = tf.contrib.layers.real_valued_column("age")
education_num = tf.contrib.layers.real_valued_column("education_num")
capital_gain = tf.contrib.layers.real_valued_column("capital_gain")
capital_loss = tf.contrib.layers.real_valued_column("capital_loss")
hours_per_week = tf.contrib.layers.real_valued_column("hours_per_week")
age_buckets = tf.contrib.layers.bucketized_column(age, boundaries=[18, 25, 30, 35, 40, 45, 50, 55, 60, 65])
education_x_occupation = tf.contrib.layers.crossed_column([education, occupation], hash_bucket_size=int(1e4))
age_buckets_x_education_x_occupation = tf.contrib.layers.crossed_column(
  [age_buckets, education, occupation], hash_bucket_size=int(1e6))
```

```python
model_dir = "/tmp"
m = tf.contrib.learn.LinearClassifier(feature_columns=[
  gender, native_country, education, occupation, workclass, marital_status, race,
  age_buckets, education_x_occupation, age_buckets_x_education_x_occupation],
  model_dir=model_dir)
m.fit(input_fn=train_input_fn, steps=200)
```

```text
Out[1]: 
LinearClassifier(params={'gradient_clip_norm': None, 'head': <tensorflow.contrib.learn.python.learn.estimators.head._BinaryLogisticHead object at 0x7f923dc14250>, 'joint_weights': False, 'optimizer': None, 'feature_columns': [_SparseColumn(column_name='gender', is_integerized=False, bucket_size=None, lookup_config=_SparseIdLookupConfig(vocabulary_file=None, keys=('Female', 'Male'), num_oov_buckets=0, vocab_size=2, default_value=-1), combiner='sum', dtype=tf.string), _SparseColumn(column_name='native_country', is_integerized=False, bucket_size=1000, lookup_config=None, combiner='sum', dtype=tf.string), _SparseColumn(column_name='education', is_integerized=False, bucket_size=1000, lookup_config=None, combiner='sum', dtype=tf.string), _SparseColumn(column_name='occupation', is_integerized=False, bucket_size=1000, lookup_config=None, combiner='sum', dtype=tf.string), _SparseColumn(column_name='workclass', is_integerized=False, bucket_size=100, lookup_config=None, combiner='sum', dtype=tf.string), _SparseColumn(column_name='marital_status', is_integerized=False, bucket_size=100, lookup_config=None, combiner='sum', dtype=tf.string), _SparseColumn(column_name='race', is_integerized=False, bucket_size=100, lookup_config=None, combiner='sum', dtype=tf.string), _BucketizedColumn(source_column=_RealValuedColumn(column_name='age', dimension=1, default_value=None, dtype=tf.float32, normalizer=None), boundaries=(18, 25, 30, 35, 40, 45, 50, 55, 60, 65)), _CrossedColumn(columns=(_SparseColumn(column_name='education', is_integerized=False, bucket_size=1000, lookup_config=None, combiner='sum', dtype=tf.string), _SparseColumn(column_name='occupation', is_integerized=False, bucket_size=1000, lookup_config=None, combiner='sum', dtype=tf.string)), hash_bucket_size=10000, hash_key=None, combiner='sum', ckpt_to_load_from=None, tensor_name_in_ckpt=None), _CrossedColumn(columns=(_BucketizedColumn(source_column=_RealValuedColumn(column_name='age', dimension=1, default_value=None, dtype=tf.float32, normalizer=None), boundaries=(18, 25, 30, 35, 40, 45, 50, 55, 60, 65)), _SparseColumn(column_name='education', is_integerized=False, bucket_size=1000, lookup_config=None, combiner='sum', dtype=tf.string), _SparseColumn(column_name='occupation', is_integerized=False, bucket_size=1000, lookup_config=None, combiner='sum', dtype=tf.string)), hash_bucket_size=1000000, hash_key=None, combiner='sum', ckpt_to_load_from=None, tensor_name_in_ckpt=None)]})
```


```python
results = m.evaluate(input_fn=eval_input_fn, steps=1)
for key in sorted(results):
    print("%s: %s" % (key, results[key]))
```

```text
accuracy: 0.834675
accuracy/baseline_label_mean: 0.236223
accuracy/threshold_0.500000_mean: 0.834675
auc: 0.879206
auc_precision_recall: 0.684307
global_step: 200
labels/actual_label_mean: 0.236223
labels/prediction_mean: 0.241313
loss: 0.358859
precision/positive_threshold_0.500000_mean: 0.711046
recall/positive_threshold_0.500000_mean: 0.505592
```

```python
m = tf.contrib.learn.LinearClassifier(feature_columns=[
  gender, native_country, education, occupation, workclass, marital_status, race,
  age_buckets, education_x_occupation, age_buckets_x_education_x_occupation],
  optimizer=tf.train.FtrlOptimizer(
    learning_rate=0.1,
    l1_regularization_strength=1.0,
    l2_regularization_strength=1.0),
  model_dir=model_dir)
m.fit(input_fn=train_input_fn, steps=200)
  
```

```text
Out[1]: 
LinearClassifier(params={'gradient_clip_norm': None, 'head': <tensorflow.contrib.learn.python.learn.estimators.head._BinaryLogisticHead object at 0x7f9236f4c890>, 'joint_weights': False, 'optimizer': <tensorflow.python.training.ftrl.FtrlOptimizer object at 0x7f9236f40510>, 'feature_columns': [_SparseColumn(column_name='gender', is_integerized=False, bucket_size=None, lookup_config=_SparseIdLookupConfig(vocabulary_file=None, keys=('Female', 'Male'), num_oov_buckets=0, vocab_size=2, default_value=-1), combiner='sum', dtype=tf.string), _SparseColumn(column_name='native_country', is_integerized=False, bucket_size=1000, lookup_config=None, combiner='sum', dtype=tf.string), _SparseColumn(column_name='education', is_integerized=False, bucket_size=1000, lookup_config=None, combiner='sum', dtype=tf.string), _SparseColumn(column_name='occupation', is_integerized=False, bucket_size=1000, lookup_config=None, combiner='sum', dtype=tf.string), _SparseColumn(column_name='workclass', is_integerized=False, bucket_size=100, lookup_config=None, combiner='sum', dtype=tf.string), _SparseColumn(column_name='marital_status', is_integerized=False, bucket_size=100, lookup_config=None, combiner='sum', dtype=tf.string), _SparseColumn(column_name='race', is_integerized=False, bucket_size=100, lookup_config=None, combiner='sum', dtype=tf.string), _BucketizedColumn(source_column=_RealValuedColumn(column_name='age', dimension=1, default_value=None, dtype=tf.float32, normalizer=None), boundaries=(18, 25, 30, 35, 40, 45, 50, 55, 60, 65)), _CrossedColumn(columns=(_SparseColumn(column_name='education', is_integerized=False, bucket_size=1000, lookup_config=None, combiner='sum', dtype=tf.string), _SparseColumn(column_name='occupation', is_integerized=False, bucket_size=1000, lookup_config=None, combiner='sum', dtype=tf.string)), hash_bucket_size=10000, hash_key=None, combiner='sum', ckpt_to_load_from=None, tensor_name_in_ckpt=None), _CrossedColumn(columns=(_BucketizedColumn(source_column=_RealValuedColumn(column_name='age', dimension=1, default_value=None, dtype=tf.float32, normalizer=None), boundaries=(18, 25, 30, 35, 40, 45, 50, 55, 60, 65)), _SparseColumn(column_name='education', is_integerized=False, bucket_size=1000, lookup_config=None, combiner='sum', dtype=tf.string), _SparseColumn(column_name='occupation', is_integerized=False, bucket_size=1000, lookup_config=None, combiner='sum', dtype=tf.string)), hash_bucket_size=1000000, hash_key=None, combiner='sum', ckpt_to_load_from=None, tensor_name_in_ckpt=None)]})
```

```python
results = m.evaluate(input_fn=eval_input_fn, steps=1)
for key in sorted(results):
    print("%s: %s" % (key, results[key]))
```

```text
accuracy: 0.823493
accuracy/baseline_label_mean: 0.236223
accuracy/threshold_0.500000_mean: 0.823493
auc: 0.860434
auc_precision_recall: 0.635671
global_step: 400
labels/actual_label_mean: 0.236223
labels/prediction_mean: 0.246117
loss: 0.389911
precision/positive_threshold_0.500000_mean: 0.721918
recall/positive_threshold_0.500000_mean: 0.411183
```

```python
df2 = train_input_fn()
```

```text
({'hours_per_week': <tf.Tensor 'Const_4:0' shape=(32561,) dtype=int64>, 'workclass': <tensorflow.python.framework.sparse_tensor.SparseTensor object at 0x7f9225aa4c90>, 'relationship': <tensorflow.python.framework.sparse_tensor.SparseTensor object at 0x7f9225b04a50>, 'gender': <tensorflow.python.framework.sparse_tensor.SparseTensor object at 0x7f9225b040d0>, 'age': <tf.Tensor 'Const:0' shape=(32561,) dtype=int64>, 'marital_status': <tensorflow.python.framework.sparse_tensor.SparseTensor object at 0x7f9225aa4d10>, 'race': <tensorflow.python.framework.sparse_tensor.SparseTensor object at 0x7f9225b04110>, 'capital_gain': <tf.Tensor 'Const_2:0' shape=(32561,) dtype=int64>, 'native_country': <tensorflow.python.framework.sparse_tensor.SparseTensor object at 0x7f9225b04210>, 'capital_loss': <tf.Tensor 'Const_3:0' shape=(32561,) dtype=int64>, 'education': <tensorflow.python.framework.sparse_tensor.SparseTensor object at 0x7f9225aa4d50>, 'education_num': <tf.Tensor 'Const_1:0' shape=(32561,) dtype=int64>, 'occupation': <tensorflow.python.framework.sparse_tensor.SparseTensor object at 0x7f9225b04f10>}, <tf.Tensor 'Const_5:0' shape=(32561,) dtype=int64>)
```

```python
print df2[0]['hours_per_week']
```

```text
Tensor("Const_10:0", shape=(32561,), dtype=int64)
```













































