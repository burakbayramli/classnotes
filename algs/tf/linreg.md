
```python
import tensorflow as tf
from sklearn.datasets import fetch_california_housing
from sklearn.preprocessing import StandardScaler

housing = fetch_california_housing(data_home="/home/burak/Downloads/scikit-data")
scaler = StandardScaler()
m, n = housing.data.shape
housing_data_plus_bias = np.c_[np.ones((m, 1)), housing.data]
scaled_housing_data_plus_bias = scaler.fit_transform(housing_data_plus_bias.astype(np.float64))
```

```text
downloading Cal. housing from http://www.dcc.fc.up.pt/~ltorgo/Regression/cal_housing.tgz to /home/burak/Downloads
```

```python
n_epochs = 1000
learning_rate = 0.01

X = tf.constant(scaled_housing_data_plus_bias, dtype=tf.float32, name="X")
y = tf.constant(housing.target.reshape(-1, 1), dtype=tf.float32, name="y")
theta = tf.Variable(tf.random_uniform([n + 1, 1], -1.0, 1.0), name="theta")
y_pred = tf.matmul(X, theta, name="predictions")
error = y_pred - y
mse = tf.reduce_mean(tf.square(error), name="mse")
gradients = 2/m * tf.matmul(tf.transpose(X), error)
training_op = tf.assign(theta, theta - learning_rate * gradients)

init = tf.global_variables_initializer()

with tf.Session() as sess:
    sess.run(init)
    for epoch in range(n_epochs):
        if epoch % 100 == 0:print("Epoch", epoch, "MSE =", mse.eval())
        sess.run(training_op)

    best_theta = theta.eval()
    
print best_theta
```

```text
('Epoch', 0, 'MSE =', 6.0430689)
('Epoch', 100, 'MSE =', 6.0430689)
('Epoch', 200, 'MSE =', 6.0430689)
('Epoch', 300, 'MSE =', 6.0430689)
('Epoch', 400, 'MSE =', 6.0430689)
('Epoch', 500, 'MSE =', 6.0430689)
('Epoch', 600, 'MSE =', 6.0430689)
('Epoch', 700, 'MSE =', 6.0430689)
('Epoch', 800, 'MSE =', 6.0430689)
('Epoch', 900, 'MSE =', 6.0430689)
[[ 0.52967787]
 [ 0.56210446]
 [-0.05574155]
 [-0.46587348]
 [ 0.98646569]
 [-0.44030929]
 [-0.35371423]
 [-0.23383665]
 [ 0.33086038]]
```


























