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

evaluation_step, train_step, data, y, dropout_prob = model1.init_model()

sess = tf.Session()

sess.run(tf.global_variables_initializer())

saver = tf.train.Saver()

if os.path.isfile(mfile + ".index"):
     print 'restoring'
     saver.restore(sess, mfile)

for i in range(num_epochs):
    x_batch, y_batch = util.get_minibatch(batch_size)
    d = { data:x_batch, y:y_batch, dropout_prob:0.0}
    acc, _ = sess.run([evaluation_step, train_step], feed_dict=d)
    print i, 'accuracy', acc 
    if i % 5 == 0:
	val_x, val_y = util.get_minibatch(batch_size,validation=True)
        d = { data:val_x, y:val_y, dropout_prob:0}
        acc = sess.run(evaluation_step, feed_dict=d)
        print i, 'validation accuracy', acc        
    
saver.save(sess, mfile)
