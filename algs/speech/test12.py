# best so far - >80 on validation
from tensorflow.contrib.framework.python.ops import audio_ops as contrib_audio
import tensorflow as tf, re
import zipfile, pandas as pd, random
import pandas as pd, scipy.io.wavfile
import numpy as np, io, os, numpy.linalg as lin

labels = ['down','go','left','no','off','on','right','stop','up','yes']

import zipfile, pandas as pd, random
import scipy.io.wavfile, io

trainzip = '/home/burak/Downloads/goog_voice_train.zip'
with zipfile.ZipFile(trainzip, 'r') as z: tfiles = z.namelist()
noise_files = [x for x in tfiles if '_background' in x and '.wav' in x]
tfiles =  [x for x in tfiles if '_background' not in x]
tfiles = np.array([x for x in tfiles if  '.wav' in x] )

valzip = '/home/burak/Downloads/test.zip'
with zipfile.ZipFile(valzip, 'r') as z: vfiles = z.namelist()
vfiles = np.array([x for x in vfiles if  '.wav' in x] )

zt = zipfile.ZipFile(trainzip, 'r')
zv = zipfile.ZipFile(valzip, 'r')

sample_rate = 16000
batch_size = 100
num_epochs = 10000
fs=16000
numcep = 26
numcontext = 9
mfile = "/tmp/speech12.ckpt"
time_dim = 50
feature_dim = 494
num_cell = 256

def normalize(v):
    if np.std(v)==0: return v
    return (v-np.mean(v)) / np.std(v)


def get_minibatch_val(batch_size):
    res = np.zeros((batch_size, time_dim, feature_dim))
    y = np.zeros((batch_size,len(labels)+2 ))
    for i in range(batch_size):
        f = random.choice(vfiles)
        label = re.findall(".*/(.*?)/.*?.wav",f)[0]
        labels2 = labels + ['unknown','silence']
        wav = io.BytesIO(zv.open(f).read())
        v = scipy.io.wavfile.read(wav)
        data = normalize(v[1])
        res[i, :] = data
        y[i, labels2.index(label)] = 1.0
               
    return res.reshape((batch_size,time_dim,feature_dim,1)),y


def get_minibatch(batch_size):

    def noise_snippet():
       nf = random.choice(noise_files)
       wav = io.BytesIO(zt.open(nf).read())
       v = scipy.io.wavfile.read(wav)
       chunks = int(len(v[1]) / sample_rate) - 1
       chosen_chunk = random.choice(range(chunks))
       fr = int(chosen_chunk * sample_rate)
       to = int((chosen_chunk+1)*sample_rate)
       chunk_byte = v[1][fr:to]
       return normalize(chunk_byte)
       
    res = np.zeros((batch_size, time_dim, feature_dim))
    y = np.zeros((batch_size,len(labels)+2 ))
    for i in range(batch_size):
      f = random.choice(tfiles)
      # pick silence (noise) randomly as training
      if random.choice(range(10)) == 0: 
           res[i, :] = audiofile_to_input_vector(noise_snippet(), sample_rate, numcep, numcontext)
           y[i, len(labels)+1] = 1.0 # silence
      else: # otherwise regular file is used
          label = re.findall(".*/(.*?)/.*?.wav",f)[0]
          # label is in the file name
          if label in labels:
              y[i, labels.index(label)] = 1.0
          else: # if not unknown
              y[i, len(labels)] = 1.0 # unknown
          wav = io.BytesIO(zt.open(f).read())
          v = scipy.io.wavfile.read(wav)

          data = normalize(v[1])

          if random.choice(range(3))==0:
              shift = np.random.randint(0,200)
              pad = data[0]
              data[shift:-1] = data[0:len(data)-shift-1] 
              data[0:shift] = pad
                    
          # sometimes add noise to training
          if random.choice(range(3))==0:
              data[0:len(data)] = normalize(data + noise_snippet()[0:len(data)])
              
          res[i, :] = data
          
    return res.reshape((batch_size,time_dim,feature_dim,1)),y

tf.reset_default_graph()

dropout_prob = tf.placeholder(tf.float32)

y = tf.placeholder(tf.float32, shape=[None, 12])

pcm = tf.placeholder(tf.float32, [16000, None], name = 'inputs')

spectrogram = contrib_audio.audio_spectrogram(
    pcm,
    window_size=480,
    stride=160,
    magnitude_squared=True)


print spectrogram

mfcc_ = contrib_audio.mfcc(spectrogram, sample_rate, 40)

print mfcc_

exit()

pos_weight = tf.constant([0.045, 0.045, 0.045, 0.045, 0.045, 0.045, 0.045, 0.045, 0.045, 0.045, 0.045, 0.505])

softmax = tf.nn.weighted_cross_entropy_with_logits(logits=logits,
                                                   targets=y,
                                                   pos_weight=pos_weight) 

cross_entropy = tf.reduce_mean(softmax)

train_step = tf.train.AdamOptimizer(0.001).minimize(cross_entropy)

correct_prediction = tf.equal(tf.argmax(y,1), tf.argmax(logits,1))

accuracy = (tf.reduce_mean(tf.cast(correct_prediction, tf.float32)))*100.

sess = tf.Session()

sess.run(tf.global_variables_initializer())

saver = tf.train.Saver()

if os.path.isfile(mfile + ".index"):
     print 'restoring'
     saver.restore(sess, mfile)
        
for i in range(num_epochs):
    x_batch, y_batch = get_minibatch(batch_size)
    if i % 5 == 0:
        acc = sess.run(accuracy,feed_dict={ fingerprint:x_batch, y:y_batch, dropout_prob: 0.2 })
        print i, 'accuracy', acc
    sess.run(train_step,feed_dict={ fingerprint:x_batch, y:y_batch })
    if i % 30 == 0: 
        saver.save(sess, mfile)
        x_batch, y_batch = get_minibatch_val(batch_size)
        acc = sess.run(accuracy,feed_dict={fingerprint:x_batch, y:y_batch, dropout_prob: 0.0})
        print i, 'validation accuracy', acc
