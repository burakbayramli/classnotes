
https://www.tensorflow.org/api_docs/python/tf/gradients

```python
import tensorflow as tf

a = tf.stop_gradient(tf.constant(0.))
b = tf.stop_gradient(2 * a)
g = tf.gradients(a + b, [a, b])

with tf.Session() as sess:
    print (sess.run(g))
```

```text
[1.0, 1.0]
```





```python
import tensorflow as tf
from sklearn.datasets import fetch_california_housing
from sklearn.preprocessing import StandardScaler

def reset_graph(seed=42):
    tf.reset_default_graph()
    tf.set_random_seed(seed)
    np.random.seed(seed)

housing = fetch_california_housing(data_home="/home/burak/Downloads/scikit-data")

m, n = housing.data.shape

housing_data_plus_bias = np.c_[np.ones((m, 1)), housing.data]
X = tf.constant(housing_data_plus_bias, dtype=tf.float32, name="X")
y = tf.constant(housing.target.reshape(-1, 1), dtype=tf.float32, name="y")
XT = tf.transpose(X)
theta = tf.matmul(tf.matmul(tf.matrix_inverse(tf.matmul(XT, X)), XT), y)

with tf.Session() as sess:
    theta_value = theta.eval()

print (theta_value)
```

```text
[[-3.6805901e+01]
 [ 4.3679604e-01]
 [ 9.4572417e-03]
 [-1.0734833e-01]
 [ 6.4441866e-01]
 [-3.9574115e-06]
 [-3.7890894e-03]
 [-4.2019320e-01]
 [-4.3307006e-01]]
```









