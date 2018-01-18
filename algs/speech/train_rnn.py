import pandas as pd
import numpy as np, util
import tensorflow as tf
import scipy.io.wavfile, zipfile
import io, time, os, random, re
import model1 # farkli modeller burada import edilir

# bu dosya her model icin farkli isimde secilebilir
mfile = "/tmp/speech1.ckpt"

batch_size = 40
num_epochs = 100
random.seed(0)
np.random.seed(0)

#evaluation_step, train_step, data, y, dropout_prob = model1.init_model()
m = model1.Model()

sess = tf.Session()

sess.run(tf.global_variables_initializer())

saver = tf.train.Saver()

if os.path.isfile(mfile + ".index"):
     print 'restoring'
     saver.restore(sess, mfile)

for i in range(num_epochs):
    x_batch, y_batch = util.get_minibatch(batch_size)
    d = { m.data:x_batch, m.y:y_batch, m.dropout_prob:0.5}
    acc, _ = sess.run([m.evaluation_step, m.train_step], feed_dict=d)
    print i, 'accuracy', acc 
    if i % 5 == 0:
         d = { m.data:x_batch, m.y:y_batch, m.dropout_prob:0.0 }
         tacc = sess.run(m.evaluation_step, feed_dict=d)
	 val_x, val_y = util.get_minibatch(batch_size,validation=True)
         d = { m.data:val_x, m.y:val_y, m.dropout_prob:0}
         vacc = sess.run(m.evaluation_step, feed_dict=d)
         print i, 'training', tacc, 'validation', vacc
    
saver.save(sess, mfile)
