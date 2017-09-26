# Import packages
import tensorflow as tf
import pandas as pd
import numpy as np
from sklearn.model_selection import train_test_split
from sklearn import preprocessing

# Import data
data = pd.read_csv("iris.data", sep=",",names=["sepal_length", "sepal_width", "petal_length", "petal_width", "iris_class"])

# Shuffle data
data = data.sample(frac=1).reset_index(drop=True)

# Then split `x`, whose columns are normalized to 1, and `y`, one-hot encoded
all_x = data[["sepal_length", "sepal_width", "petal_length", "petal_width"]]
min_max_scaler = preprocessing.MinMaxScaler()
all_x = min_max_scaler.fit_transform(all_x)
all_y = pd.get_dummies(data.iris_class)

# ... and split training and test set
train_x, test_x, train_y, test_y = train_test_split(all_x, all_y, test_size=1./3)

print train_x
print train_y

# Check the dimensions
print(train_x.shape)
print(train_y.shape)
print(test_x.shape)
print(test_y.shape)

# and define number of features, n_x, and number of classes, n_y
n_x = np.shape(train_x)[1]
n_y = np.shape(train_y)[1]

# Reset graph
tf.reset_default_graph()

# Define learning rate
learning_rate = 0.01

# Start graph definition...
g = tf.Graph()
# ... and placeholders
# Define the number of neurons for each hidden layer:
h1 = 10
h2 = 20
h3 = 10

x = tf.placeholder(tf.float32, [None, n_x], name="x")
y = tf.placeholder(tf.float32, [None, n_y], name="y")

prediction = tf.contrib.layers.fully_connected(inputs=x,
                                               num_outputs=n_y, 
                                               activation_fn=tf.nn.softmax,
                                               scope="Out")

cost = tf.losses.softmax_cross_entropy(onehot_labels=y,
                                       logits=prediction,
                                       scope="Cost_Function")

correct_prediction = tf.equal(tf.argmax(prediction,1,name="Argmax_Pred"),
                              tf.argmax(y, 1, name="Y_Pred"),
                              name="Correct_Pred")
accuracy = tf.reduce_mean(tf.cast(correct_prediction,tf.float32,
                                  name="Cast_Corr_Pred"),name="Accuracy")
optimizer = tf.train.AdagradOptimizer(learning_rate, name="Optimizer").minimize(cost)

sess = tf.InteractiveSession()

# Initialize variables
init = tf.global_variables_initializer()
sess.run(init)

print correct_prediction.eval({x: test_x, y: test_y})

# Train for a number of epochs
training_epochs = 3000
for epoch in range(training_epochs):
    sess.run([optimizer, cost], feed_dict={x: train_x, y: train_y})

print("Accuracy:", accuracy.eval({x: test_x, y: test_y}))    

