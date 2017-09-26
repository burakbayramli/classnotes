# Import packages
import tensorflow as tf
import pandas as pd
import numpy as np
from sklearn.model_selection import train_test_split
from sklearn import preprocessing

np.random.seed(0)
tf.set_random_seed(0)
data = pd.read_csv("iris.data", sep=",",names=["sepal_length", "sepal_width", "petal_length", "petal_width", "iris_class"])
data = data.sample(frac=1,random_state=0).reset_index(drop=True)
all_x = data[["sepal_length", "sepal_width", "petal_length", "petal_width"]]
min_max_scaler = preprocessing.MinMaxScaler()
all_x = min_max_scaler.fit_transform(all_x)
all_y = pd.get_dummies(data.iris_class)
train_x, test_x, train_y, test_y = train_test_split(all_x, all_y, test_size=0.20)

print(train_x.shape)
print(train_y.shape)
print(test_x.shape)
print(test_y.shape)

# Reset graph
tf.reset_default_graph()

# Define learning rate
learning_rate = 0.01

x = tf.placeholder(tf.float32, [None, np.shape(train_x)[1]], name="x")
y = tf.placeholder(tf.float32, [None, np.shape(train_y)[1]], name="y")

prediction = tf.contrib.layers.fully_connected(inputs=x,
                                               num_outputs=np.shape(train_y)[1], 
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

optimizer = tf.train.AdagradOptimizer(learning_rate,
                                      name="Optimizer").minimize(cost)

sess = tf.InteractiveSession()

# Initialize variables
init = tf.global_variables_initializer()
sess.run(init)

# Train for a number of epochs
training_epochs = 2000
for epoch in range(training_epochs):
    sess.run([optimizer, cost], feed_dict={x: train_x, y: train_y})

print("Accuracy:", accuracy.eval({x: test_x, y: test_y}))    

