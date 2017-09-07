# The multiple neural network layer we will create will be composed of
# three fully connected hidden layers, with node sizes 25, 10, and 3
import numpy as np
from tensorflow.python.framework import ops
import matplotlib.pyplot as plt
import pandas as pd
import tensorflow as tf

cols_of_interest = ['AGE', 'LWT', 'RACE', 'SMOKE', 'PTL', 'HT', 'UI', 'FTV']
df = pd.read_csv('lowbwt.dat',sep='\s*',engine='python')
x_vals = np.array(df[cols_of_interest])
y_vals = np.array(df['BWT'])

ops.reset_default_graph()

# Set Seed
seed = 3
tf.set_random_seed(seed)
np.random.seed(seed)

batch_size = 10

# Create graph session 
sess = tf.Session()

# Split data into train/test = 80%/20%
tmp = np.random.choice(range(len(x_vals)), size=len(x_vals), replace=False)
first = int(len(x_vals)*0.80)
train_indices = tmp[:first]
test_indices = tmp[first:]

x_vals_train = x_vals[train_indices]
x_vals_test = x_vals[test_indices]
y_vals_train = y_vals[train_indices]
y_vals_test = y_vals[test_indices]


# Normalize by column (min-max norm to be between 0 and 1)
def normalize_cols(m):
    col_max = m.max(axis=0)
    col_min = m.min(axis=0)
    return (m-col_min) / (col_max - col_min)
    
x_vals_train = np.nan_to_num(normalize_cols(x_vals_train))
x_vals_test = np.nan_to_num(normalize_cols(x_vals_test))


# Define Variable Functions (weights and bias)
def init_weight(shape, st_dev):
    weight = tf.Variable(tf.random_normal(shape, stddev=st_dev))
    return(weight)
    

def init_bias(shape, st_dev):
    bias = tf.Variable(tf.random_normal(shape, stddev=st_dev))
    return(bias)
    
    
# Create Placeholders
x_data = tf.placeholder(shape=[None, 8], dtype=tf.float32)
y_target = tf.placeholder(shape=[None, 1], dtype=tf.float32)


# Create a fully connected layer:
def fully_connected(input_layer, weights, biases):
    layer = tf.add(tf.matmul(input_layer, weights), biases)
    return(tf.nn.relu(layer))


#--------Create the first layer (25 hidden nodes)--------
weight_1 = init_weight(shape=[8, 25], st_dev=10.0)
bias_1 = init_bias(shape=[25], st_dev=10.0)
layer_1 = fully_connected(x_data, weight_1, bias_1)

#--------Create second layer (10 hidden nodes)--------
weight_2 = init_weight(shape=[25, 10], st_dev=10.0)
bias_2 = init_bias(shape=[10], st_dev=10.0)
layer_2 = fully_connected(layer_1, weight_2, bias_2)


#--------Create third layer (3 hidden nodes)--------
weight_3 = init_weight(shape=[10, 3], st_dev=10.0)
bias_3 = init_bias(shape=[3], st_dev=10.0)
layer_3 = fully_connected(layer_2, weight_3, bias_3)


#--------Create output layer (1 output value)--------
weight_4 = init_weight(shape=[3, 1], st_dev=10.0)
bias_4 = init_bias(shape=[1], st_dev=10.0)
final_output = fully_connected(layer_3, weight_4, bias_4)

# Declare loss function (L1)
loss = tf.reduce_mean(tf.abs(y_target - final_output))

# Declare optimizer
my_opt = tf.train.AdamOptimizer(0.05)
train_step = my_opt.minimize(loss)

# Initialize Variables
init = tf.initialize_all_variables()
sess.run(init)

# Training loop
loss_vec = []
test_loss = []
for i in range(200):
    rand_index = np.random.choice(len(x_vals_train), size=batch_size)
    rand_x = x_vals_train[rand_index]
    rand_y = np.transpose([y_vals_train[rand_index]])
    sess.run(train_step, feed_dict={x_data: rand_x, y_target: rand_y})

    temp_loss = sess.run(loss, feed_dict={x_data: rand_x, y_target: rand_y})
    loss_vec.append(temp_loss)
    
    test_temp_loss = sess.run(loss, feed_dict={x_data: x_vals_test, y_target: np.transpose([y_vals_test])})
    test_loss.append(test_temp_loss)
    if (i+1)%25==0:
        print('Epoch: ' + str(i+1) + '. Kayip = ' + str(temp_loss))


# Plot loss over time
plt.plot(loss_vec, 'k-', label='Egitim Kaybi')
plt.plot(test_loss, 'r--', label='Test Kaybi')
plt.title('Her Epoch Icinde Kayip')
plt.xlabel('Epoch')
plt.ylabel('Kayip')
plt.legend(loc="upper right")
plt.savefig('tf_03.png')

