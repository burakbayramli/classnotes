import pandas as pd, sys
import numpy as np, util
import tensorflow as tf
import scipy.io.wavfile, zipfile
import io, time, os, random, re


# komut satirindan verilen parametreye gore dogru
# modeli import et ve aktif hale getir
cmd = 'import ' + sys.argv[1] 
exec cmd
cmd = 'm = ' + sys.argv[1] + '.Model()'
print 'model', sys.argv[1]
exec cmd

sess = tf.Session()

sess.run(tf.global_variables_initializer())

saver = tf.train.Saver()

# eger model diskte varsa yukle
print m.mfile
print 'model file exists', os.path.isfile(m.mfile + ".index")
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

# modeli diske yaz
saver.save(sess, m.mfile)
