import tensorflow as tf, util, os

num_layers = 3
num_cell = 100

class Model:
    def __init__(self):

        self.mfile = "/tmp/speech1.ckpt"

        tf.reset_default_graph()

        self.dop = tf.placeholder(tf.float32) # dropout olasiligi (probability)
        
        self.data = tf.placeholder(tf.float32, [None, util.fs])
        
        self.stfts = tf.contrib.signal.stft(self.data, frame_length=256,
                                            frame_step=128, fft_length=256)
        self.fingerprint = tf.abs(self.stfts)
        
        print self.fingerprint

        self.y = tf.placeholder(tf.float32, shape=[None, len(util.labels)])
        cells = []
        for _ in range(num_layers):
            cell = tf.contrib.rnn.LSTMCell(num_cell)
            cell = tf.contrib.rnn.DropoutWrapper(cell, output_keep_prob=1-self.dop)
            cells.append(cell)
        cell = tf.contrib.rnn.MultiRNNCell(cells)
        output, states = tf.nn.dynamic_rnn(cell, self.fingerprint, dtype=tf.float32)
        self.last = states[-1][0]

        print self.last

        self.logits = tf.contrib.layers.fully_connected(inputs=self.last,
                                                        num_outputs=len(util.labels),
                                                        activation_fn=None)

        self.softmax = tf.nn.softmax_cross_entropy_with_logits(logits=self.logits,
                                                               labels=self.y) 

        self.cross_entropy = tf.reduce_mean(self.softmax)

        self.train_step = tf.train.AdamOptimizer(0.001).minimize(self.cross_entropy)
        
        self.correct_prediction = tf.equal(tf.argmax(self.y,1),
                                           tf.argmax(self.logits,1))

        self.evaluation_step = tf.reduce_mean(tf.cast(self.correct_prediction,
                                                      tf.float32))


        self.saver = tf.train.Saver()
                
        
