import tensorflow as tf
import numpy as np
from pprint import pprint
import datetime
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

np.random.seed(1)

LSTM_SIZE = 50
t_min, t_max = 0, 30
resolution = 0.1
sequence_length = 6
instruction_count = 1
epoch = 1000

def f(t):
    return t * np.sin(t) / 3 + 2 * np.sin(t*5)

def next_batch(batch_size, n_steps):
    t0 = np.random.rand(batch_size, 1) * (t_max - t_min - n_steps * resolution)
    Ts = t0 + np.arange(0., n_steps + 1) * resolution
    ys = f(Ts)
    X = ys[:, :-1].reshape(-1, n_steps, 1)
    y = ys[:, 1:].reshape(-1, n_steps, 1)
    y = y[:,-1,:]
    y = y.flatten()
    print y.shape    
    return list(X),y

reference_input_data,reference_output_data = next_batch(400, sequence_length)
   
NUM_EXAMPLES = len(reference_input_data) / 4 # we use 1/4 of the data for the training

test_input = reference_input_data[NUM_EXAMPLES:]
test_output = reference_output_data[NUM_EXAMPLES:] # everything beyond NUM_EXAMPLES

train_input = reference_input_data[:NUM_EXAMPLES]
train_output = reference_output_data[:NUM_EXAMPLES]

print train_input[1]
print train_output[1]

data = tf.placeholder(tf.float32, [None, sequence_length, instruction_count], name='data')
target = tf.transpose(tf.placeholder(tf.float32, [None], name='target'))

FEATURE_SIZE = 1 # Ace of Hearts, Ace of Clubs, King of Spades

def default_weights_and_bias():
    Weights = tf.Variable(tf.truncated_normal([LSTM_SIZE, LSTM_SIZE + FEATURE_SIZE], -0.2, 0.1))
    bias = tf.Variable(tf.constant(0.0, shape = [LSTM_SIZE, 1]))
    
    return Weights, bias

W_f, _ = default_weights_and_bias()

b_f = tf.Variable(tf.constant(1.0, shape = [LSTM_SIZE, 1]))

# The forget layer
#
# Shapes:
#   - W_f: 24x27
#   - ht_minus_1_and_xt: 27x?
#   - b_f: 24x1
#   - f_t: 24x?
def f_t(ht_minus_1_and_xt):
    return tf.sigmoid(tf.matmul(W_f, ht_minus_1_and_xt) + b_f)

W_i, b_i = default_weights_and_bias()

# Input Gate Layer
#
# Shapes:
#   - W_i: 24x27
#   - ht_minus_1_and_xt: 27x?
#   - b_i: 24x1
#   - i_t: 24x?
def i_t(ht_minus_1_and_xt):
    return tf.sigmoid(tf.matmul(W_i, ht_minus_1_and_xt) + b_i)

W_C, b_c = default_weights_and_bias()

# New Candidates for the Conveyor
#
# Shapes:
#   - W_C: 24x27
#   - ht_minus_1_and_xt: 27x?
#   - b_c: 24x1
#   - candidate_C_t: 24x?
def candidate_C_t(ht_minus_1_and_xt):
    return tf.tanh(tf.matmul(W_C, ht_minus_1_and_xt) + b_c)

# Updated Conveyor
#
# Shapes:
#   - f_t: 24x?
#   - Conveyor: 24x?
#   - i_t: 24x?
#   - CandidateConveyor: 24x?
def C_t(ht_minus_1_and_xt, Conveyor, CandidateConveyor):
    return f_t(ht_minus_1_and_xt) * Conveyor + i_t(ht_minus_1_and_xt) * CandidateConveyor

W_o, b_o = default_weights_and_bias()

# Updated Conveyor
#
# Shapes:
#   - W_o: 24x27
#   - b_o: 24x1
#   - ht_minus_1_and_xt: 27x?
#   - FinalConveyor: 24x?
#   - o_t: 24x?
#   - h_t: 24x?
def h_t(ht_minus_1_and_xt, FinalConveyor):
    o_t = tf.sigmoid(tf.matmul(W_o, ht_minus_1_and_xt) + b_o)
    
    return o_t * tf.tanh(FinalConveyor)

def lstm_cell(ht_minus_1_and_Conveyor, xt):
    ht_minus_1, Conveyor = ht_minus_1_and_Conveyor
    
    ht_minus_1_and_xt = tf.transpose(tf.concat([ht_minus_1, xt], 1))
    
    CandidateConveyor = candidate_C_t(ht_minus_1_and_xt)
    
    FinalConveyor = C_t(ht_minus_1_and_xt, Conveyor, CandidateConveyor)
    
    lstm_prediction = tf.transpose(h_t(ht_minus_1_and_xt, FinalConveyor))
    
    return(lstm_prediction, FinalConveyor)

data_length = tf.shape(data)[0]

def lstm_loop(last_lstm_prediction, last_state, step):
    lstm_prediction, state = lstm_cell((last_lstm_prediction, last_state),
                                       data[:, step, :])
    return lstm_prediction, state, tf.add(step, 1)

initial_Conveyor = tf.zeros([LSTM_SIZE, data_length])

initial_prediction = tf.zeros([data_length, LSTM_SIZE])

timesteps = sequence_length

for_each_time_step = lambda a, b, step: tf.less(step, timesteps)

lstm_prediction, lstm_state, _ = tf.while_loop(for_each_time_step,
                                               lstm_loop,
                                               (initial_prediction, initial_Conveyor, 0),
                                               parallel_iterations=32)

weight = tf.Variable(tf.truncated_normal([LSTM_SIZE, 1]))

bias = tf.Variable(tf.constant(0.0, shape=[1]))

prediction = tf.matmul(lstm_prediction, weight) + bias

with tf.name_scope('mean_square_error'):
    mean_square_error = tf.reduce_sum(tf.square(tf.subtract(target, tf.unstack(prediction, axis = 1))))
    
tf.summary.scalar('mean_square_error', mean_square_error)

optimizer = tf.train.AdamOptimizer()

minimize = optimizer.minimize(mean_square_error)

with tf.name_scope('error'):
    with tf.name_scope('mistakes'):
        mistakes = tf.not_equal(target, tf.round(tf.unstack(prediction, axis = 1)))

sess = tf.InteractiveSession()

merged = tf.summary.merge_all()

date = str(datetime.datetime.now())

init_op = tf.global_variables_initializer()

saver = tf.train.Saver() 

sess.run(init_op)

for i in range(epoch):
    if (i + 1) % 20 == 0:
        summary, mean_squ_err = sess.run([merged, mean_square_error], {data: test_input, target: test_output})        
        print('Epoch {:4d} | mean squ error {: 3.1f}'.format(i + 1, mean_squ_err))
    
    sess.run(minimize,{data: train_input, target: train_output})

saver.save(sess, "/tmp/lstm-time-")
    
sess.close()

# saver = tf.train.Saver() 

# sess = tf.InteractiveSession()

# tst_input = [[-3.22914761],
#              [-2.55665759],
#              [-1.44953796],
#              [-0.0840293 ],
#              [ 1.30288372],
#              [ 2.47053056]]

# tst_input = [ np.array(tst_input) ]
# print tst_input    
# with tf.Session() as sess:
#     saver.restore(sess, "/tmp/lstm-time-")
#     print sess.run(prediction, { data: tst_input  } )

t_start = 30. # tahmine burada basla
n_more = 40 # bu kadar daha uret
newx = []
newy = []
with tf.Session() as sess:
    saver.restore(sess, "/tmp/lstm-time-")
    t_curr = t_start
    for i in range(n_more):
        newp = np.array([  t_curr + (j*resolution) for j in range(sequence_length)  ] )
        newp = f(newp)
        t_curr += resolution
        newp = [ newp.reshape(sequence_length,1) ]
        newp = f(newp)
        outp = sess.run(prediction, { data: newp  } )[0][0]
        newx.append(t_curr)
        newy.append(outp)

newx = np.array(newx)
newy = np.array(newy)
print newx, newy

t = np.linspace(t_min, t_max, int((t_max - t_min) / resolution))
y = f(t)
plt.plot(t,y)
plt.plot(newx,newy,'g')
plt.savefig('lstm_01.png')
