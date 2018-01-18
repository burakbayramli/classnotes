import pandas as pd, sys
import numpy as np, util
import tensorflow as tf
import scipy.io.wavfile, zipfile
import io, time, os, random, re
from array import array
import pyaudio
import wave
import model1 

THRESHOLD = 500
CHUNK_SIZE = 16000
FORMAT = pyaudio.paInt16
RATE = 16000

p = pyaudio.PyAudio()
stream = p.open(format=FORMAT, channels=1, rate=RATE,
    input=True, output=True,
    frames_per_buffer=CHUNK_SIZE)

num_silent = 0
snd_started = False

import model1
m = model1.Model()

sess = tf.Session()

sess.run(tf.global_variables_initializer())

saver = tf.train.Saver()

print m.mfile
print 'model file exists', os.path.isfile(m.mfile + ".index")
if os.path.isfile(m.mfile + ".index"):
     print 'restoring'
     saver.restore(sess, m.mfile)
     
while 1:
    snd_data = array('h', stream.read(CHUNK_SIZE))
    arr = np.array(snd_data).reshape(1,16000)
    arr = util.adj_volume(arr)
    print arr.shape    
    d = { m.data:arr, m.dop:0}
    l = sess.run(m.logits, feed_dict=d)
    if np.any(np.abs(l)>3.0):
         print util.labels[np.argmax(l)], l

