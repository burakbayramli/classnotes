import tensorflow as tf, util

num_layers = 3
num_cell = 100

def init_model():

    tf.reset_default_graph()

    dropout_prob = tf.placeholder(tf.float32)

    data = tf.placeholder(tf.float32, [None, util.fs])

    stfts = tf.contrib.signal.stft(data, frame_length=256, frame_step=128, fft_length=256)

    fingerprint = tf.abs(stfts)

    print fingerprint

    y = tf.placeholder(tf.float32, shape=[None, len(util.labels)])

    cells = []
    for _ in range(num_layers):
        cell = tf.contrib.rnn.LSTMCell(num_cell)
        cell = tf.contrib.rnn.DropoutWrapper(cell, output_keep_prob=1-dropout_prob)
        cells.append(cell)
    cell = tf.contrib.rnn.MultiRNNCell(cells)
    output, states = tf.nn.dynamic_rnn(cell, fingerprint, dtype=tf.float32)
    last = states[-1][0]

    print last

    logits = tf.contrib.layers.fully_connected(inputs=last,
                                               num_outputs=len(util.labels),
                                               activation_fn=None)

    softmax = tf.nn.softmax_cross_entropy_with_logits(logits=logits,labels=y) 

    cross_entropy = tf.reduce_mean(softmax)

    train_step = tf.train.AdamOptimizer(0.001).minimize(cross_entropy)

    correct_prediction = tf.equal(tf.argmax(y,1), tf.argmax(logits,1))

    evaluation_step = tf.reduce_mean(tf.cast(correct_prediction, tf.float32))

    return evaluation_step, train_step, data, y, dropout_prob
