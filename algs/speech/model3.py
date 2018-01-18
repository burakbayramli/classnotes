# model1.py
import tensorflow as tf, util, os

class Model:
    def __init__(self):
                
        self.file = os.path.basename(__file__).replace(".pyc","").replace(".py","")
        
        self.mfile = "/tmp/" + self.file + ".ckpt"

        self.batch_size = 100
        
        self.num_epochs = 400

        self.dop_param = 0.0 # dropout olasiligi
        
        self.num_cell = 200

        self.num_layers = 2

        tf.reset_default_graph()

        self.dop = tf.placeholder(tf.float32) # dropout olasiligi (probability)
        
        self.data = tf.placeholder(tf.float32, [None, util.fs])

        print self.data 
        
        self.stfts = tf.contrib.signal.stft(self.data, frame_length=256,
                                            frame_step=128, fft_length=256)

        print self.stfts
        
        self.fingerprint = tf.abs(self.stfts)
        
        print self.fingerprint

        self.y = tf.placeholder(tf.float32, shape=[None, len(util.labels)])
        
        j_cell_fw = tf.contrib.rnn.LSTMBlockCell(self.num_cell,forget_bias=1.0)
        j_cell_fw = tf.contrib.rnn.DropoutWrapper(j_cell_fw, input_keep_prob=1-self.dop)
        j_cell_bw = tf.contrib.rnn.LSTMBlockCell(self.num_cell,forget_bias=1.0)
        j_cell_bw = tf.contrib.rnn.DropoutWrapper(j_cell_bw, input_keep_prob=1-self.dop)

        j_cell_fw = tf.contrib.rnn.MultiRNNCell([j_cell_fw]*self.num_layers)
        j_cell_bw = tf.contrib.rnn.MultiRNNCell([j_cell_bw]*self.num_layers)

        (j_fw, j_bw) , _ = tf.nn.bidirectional_dynamic_rnn(
            j_cell_fw,
            j_cell_bw,
            inputs=self.fingerprint,
            dtype=tf.float32)

        outputs  = tf.concat([j_fw, j_bw],2)

        print outputs

        self.last = outputs[:,-1]

        print self.last

        self.logits = tf.contrib.layers.fully_connected(inputs=self.last,
                                                        num_outputs=len(util.labels),
                                                        activation_fn=None)

        print self.logits
        
        self.softmax = tf.nn.softmax_cross_entropy_with_logits(logits=self.logits,
                                                               labels=self.y) 

        self.cross_entropy = tf.reduce_mean(self.softmax)

        self.train_step = tf.train.AdamOptimizer(0.001).minimize(self.cross_entropy)
        
        self.correct_prediction = tf.equal(tf.argmax(self.y,1),
                                           tf.argmax(self.logits,1))

        self.evaluation_step = tf.reduce_mean(tf.cast(self.correct_prediction,
                                                      tf.float32))


        self.saver = tf.train.Saver()
                
        
# training 0.91 validation 0.926238
