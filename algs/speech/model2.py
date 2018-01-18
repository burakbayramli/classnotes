# model1.py
import tensorflow as tf, util, os

class Model:
    def __init__(self):
                
        self.file = os.path.basename(__file__).replace(".pyc","").replace(".py","")
        
        self.mfile = "/tmp/" + self.file + ".ckpt"

        self.batch_size = 400
        
        self.num_epochs = 200

        self.dop_param = 0.0 # dropout olasiligi
        
        self.num_cell = 200

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
        
        gru_fw_cell	=	tf.contrib.rnn.GRUCell(self.num_cell)
        gru_fw_cell	=	tf.contrib.rnn.DropoutWrapper(gru_fw_cell, output_keep_prob=1-self.dop)

        gru_bw_cell	=	tf.contrib.rnn.GRUCell(self.num_cell)
        gru_bw_cell	=	tf.contrib.rnn.DropoutWrapper(gru_bw_cell, output_keep_prob=1-self.dop)

        outputs, states	=  tf.nn.bidirectional_dynamic_rnn(cell_fw=gru_fw_cell,
                                                           cell_bw=gru_bw_cell,
                                                           inputs=self.fingerprint,
                                                           dtype=tf.float32)
        print outputs
        print states

        self.states = tf.concat(values=states, axis=1)

        print self.states

        self.logits = tf.contrib.layers.fully_connected(inputs=self.states,
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
