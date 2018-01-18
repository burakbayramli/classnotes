import pandas as pd, sys
import numpy as np, util
import tensorflow as tf
import scipy.io.wavfile, zipfile
import io, time, os, random, re
import model1 # farkli modeller burada import edilir

# bu dosya her model icin farkli isimde secilebilir

seed = 0
random.seed(seed)
np.random.seed(seed)

cmd = 'm = ' + sys.argv[1] + '.Model()'
print cmd, 'isletiliyor'
exec cmd

sess = tf.Session()

sess.run(tf.global_variables_initializer())

tf.set_random_seed(seed)

saver = tf.train.Saver()

if os.path.isfile(m.mfile + ".index"):
     print 'restoring'
     saver.restore(sess, m.mfile)

for i in range(m.num_epochs):
    train_x, train_y = util.get_minibatch(m.batch_size)
    d = { m.data:train_x, m.y:train_y, m.dop:m.dop_param}
    acc, _ = sess.run([m.evaluation_step, m.train_step], feed_dict=d)
    print i, 'accuracy', acc 
    if i % 5 == 0:
         d = { m.data:train_x, m.y:train_y, m.dop:m.dop_param }
         tacc = sess.run(m.evaluation_step, feed_dict=d)
	 val_x, val_y = util.get_minibatch(m.batch_size,validation=True)
         d = { m.data:val_x, m.y:val_y, m.dop:0}
         vacc = sess.run(m.evaluation_step, feed_dict=d)
         print i, 'training', tacc, 'validation', vacc
    
saver.save(sess, m.mfile)
