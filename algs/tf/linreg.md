
```python
import tensorflow as tf
from sklearn.datasets import fetch_california_housing
from sklearn.preprocessing import StandardScaler

def reset_graph(seed=42):
    tf.reset_default_graph()
    tf.set_random_seed(seed)
    np.random.seed(seed)

housing = fetch_california_housing(data_home="/home/burak/Downloads/scikit-data")
scaler = StandardScaler()
m, n = housing.data.shape
scaled_housing_data = scaler.fit_transform(housing.data)
scaled_housing_data_plus_bias = np.c_[np.ones((m, 1)), scaled_housing_data]

print(scaled_housing_data_plus_bias.mean(axis=0))
print(scaled_housing_data_plus_bias.mean(axis=1))
print(scaled_housing_data_plus_bias.mean())
print(scaled_housing_data_plus_bias.shape)

```

```text
[  1.00000000e+00   6.60969987e-17   5.50808322e-18   6.60969987e-17
  -1.06030602e-16  -1.10161664e-17   3.44255201e-18  -1.07958431e-15
  -8.52651283e-15]
[ 0.38915536  0.36424355  0.5116157  ..., -0.06612179 -0.06360587
  0.01359031]
0.111111111111
(20640, 9)
```

```python
reset_graph()

n_epochs = 1000
learning_rate = 0.01

X = tf.constant(scaled_housing_data_plus_bias, dtype=tf.float32, name="X")
y = tf.constant(housing.target.reshape(-1, 1), dtype=tf.float32, name="y")
theta = tf.Variable(tf.random_uniform([n + 1, 1], -1.0, 1.0, seed=42), name="theta")
y_pred = tf.matmul(X, theta, name="predictions")
error = y_pred - y
mse = tf.reduce_mean(tf.square(error), name="mse")

# this block for manual gradient
#
#gradients = 2/np.float(m) * tf.matmul(tf.transpose(X), error)
gradients = tf.gradients(mse, [theta])[0]
training_op = tf.assign(theta, theta-(learning_rate*gradients))

init = tf.global_variables_initializer()

with tf.Session() as sess:
    sess.run(init)
    for epoch in range(n_epochs):
        if epoch % 100 == 0: print("Epoch", epoch, "MSE =", mse.eval())
	sess.run(training_op)
    
    best_theta = theta.eval()
    
print best_theta
```

```text
('Epoch', 0, 'MSE =', 9.1615429)
('Epoch', 100, 'MSE =', 0.71450061)
('Epoch', 200, 'MSE =', 0.56670463)
('Epoch', 300, 'MSE =', 0.55557162)
('Epoch', 400, 'MSE =', 0.54881167)
('Epoch', 500, 'MSE =', 0.5436362)
('Epoch', 600, 'MSE =', 0.53962916)
('Epoch', 700, 'MSE =', 0.53650916)
('Epoch', 800, 'MSE =', 0.53406781)
('Epoch', 900, 'MSE =', 0.53214717)
[[ 2.06855249]
 [ 0.88740271]
 [ 0.14401658]
 [-0.34770882]
 [ 0.36178368]
 [ 0.00393811]
 [-0.04269556]
 [-0.66145277]
 [-0.6375277 ]]
```

























