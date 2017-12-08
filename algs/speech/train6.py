import tensorflow as tf
from python_speech_features import mfcc
import numpy as np
import tensorflow as tf
import scipy.io.wavfile as wav
from glob import glob
import time, re, os, random
import numpy as np

num_units = 200
num_layers = 3
batch_size = 5
num_epochs = 1000
num_batches_per_epoch = 10
sample_rate=16000
num_features = 26
steps_before_after = 9
SPACE_TOKEN = '<space>'
SPACE_INDEX = 0
FIRST_INDEX = ord('a') - 1  # 0 is reserved to space
mfile = "/tmp/speech.ckpt"

def load_wavfile(wavfile):
    """
    Read a wav file using scipy.io.wavfile
    """
    rate, sig = wav.read(wavfile)
    data_name = os.path.splitext(os.path.basename(wavfile))[0]
    return rate, sig, data_name

def audiofile_to_input_vector(audio_filename, numcep, numcontext):
    '''
    Turn an audio file into feature representation.

    This function has been modified from Mozilla DeepSpeech:
    https://github.com/mozilla/DeepSpeech/blob/master/util/audio.py

    # This Source Code Form is subject to the terms of the Mozilla Public
    # License, v. 2.0. If a copy of the MPL was not distributed with this
    # file, You can obtain one at http://mozilla.org/MPL/2.0/.
    '''

    # Load wav files
    fs, audio = wav.read(audio_filename)

    # Get mfcc coefficients
    orig_inputs = mfcc(audio, samplerate=fs, numcep=numcep)

    # We only keep every second feature (BiRNN stride = 2)
    orig_inputs = orig_inputs[::2]

    # For each time slice of the training set, we need to copy the context this makes
    # the numcep dimensions vector into a numcep + 2*numcep*numcontext dimensions
    # because of:
    #  - numcep dimensions for the current mfcc feature set
    #  - numcontext*numcep dimensions for each of the past and future (x2) mfcc feature set
    # => so numcep + 2*numcontext*numcep
    train_inputs = np.array([], np.float32)
    train_inputs.resize((orig_inputs.shape[0], numcep + 2 * numcep * numcontext))

    # Prepare pre-fix post fix context
    empty_mfcc = np.array([])
    empty_mfcc.resize((numcep))

    # Prepare train_inputs with past and future contexts
    time_slices = range(train_inputs.shape[0])
    context_past_min = time_slices[0] + numcontext
    context_future_max = time_slices[-1] - numcontext
    for time_slice in time_slices:
        # Reminder: array[start:stop:step]
        # slices from indice |start| up to |stop| (not included), every |step|

        # Add empty context data of the correct size to the start and end
        # of the MFCC feature matrix

        # Pick up to numcontext time slices in the past, and complete with empty
        # mfcc features
        need_empty_past = max(0, (context_past_min - time_slice))
        empty_source_past = list(empty_mfcc for empty_slots in range(need_empty_past))
        data_source_past = orig_inputs[max(0, time_slice - numcontext):time_slice]
        assert(len(empty_source_past) + len(data_source_past) == numcontext)

        # Pick up to numcontext time slices in the future, and complete with empty
        # mfcc features
        need_empty_future = max(0, (time_slice - context_future_max))
        empty_source_future = list(empty_mfcc for empty_slots in range(need_empty_future))
        data_source_future = orig_inputs[time_slice + 1:time_slice + numcontext + 1]
        assert(len(empty_source_future) + len(data_source_future) == numcontext)

        if need_empty_past:
            past = np.concatenate((empty_source_past, data_source_past))
        else:
            past = data_source_past

        if need_empty_future:
            future = np.concatenate((data_source_future, empty_source_future))
        else:
            future = data_source_future

        past = np.reshape(past, numcontext * numcep)
        now = orig_inputs[time_slice]
        future = np.reshape(future, numcontext * numcep)

        train_inputs[time_slice] = np.concatenate((past, now, future))
        assert(len(train_inputs[time_slice]) == numcep + 2 * numcep * numcontext)

    # Scale/standardize the inputs
    # This can be done more efficiently in the TensorFlow graph
    train_inputs = (train_inputs - np.mean(train_inputs)) / np.std(train_inputs)
    return train_inputs
    
fdir = "/home/burak/Downloads/train/audio"

def find_files(directory, pattern='.wav'):
    """Recursively finds all files matching the pattern."""
    files = []
    for root, directories, filenames in os.walk(directory):
        for filename in filenames: 
            path = os.path.join(root,filename)
            if pattern in path: files.append(path)    
    res = sorted(files)
    return res

audio_files = find_files("/home/burak/Downloads/goog_tf_speech/audio")
print len(audio_files)

f = '/home/burak/Downloads/goog_tf_speech/audio/down/00176480_nohash_0.wav'
res = audiofile_to_input_vector(f, num_features, steps_before_after)
res = res.astype('float32')
print res.shape

f = '/home/burak/Downloads/goog_tf_speech/audio/down/00176480_nohash_0.wav'
res = audiofile_to_input_vector(f, num_features, steps_before_after)
res = res.astype('float32')
print res.shape

labels = ['down','go','left','no','off','on','right','stop','up','yes']

random.seed(0)
def get_minibatch(batch_size):
    res = []
    y = np.zeros((batch_size,len(labels)))
    for i in range(batch_size):
    	f = random.choice(audio_files)
	a = audiofile_to_input_vector(f, num_features, steps_before_after)
	a = a.astype('float32')
    	res.append(a)
	label = re.findall(".*/(.*?)/.*?.wav",f)[0]
	y[i, labels.index(label)] = 1.0
    res = np.array(res)
    y = np.array(y)
    return res, y

data,y = get_minibatch(1)
print data.shape, y.shape
print y[0]

tf.reset_default_graph()

# e.g: log filter bank or MFCC features
# Has size [batch_size, max_step_size, num_features], but the
# batch_size and max_step_size can vary along each step
X = tf.placeholder(tf.float32, [batch_size, 50, 494])
y = tf.placeholder(tf.float32, shape=[batch_size, len(labels)])

basic_cell = tf.contrib.rnn.GRUCell(num_units=num_units)
multi_layer_cell = tf.contrib.rnn.MultiRNNCell([basic_cell] * num_units)
cells = []
for _ in range(num_layers):
    cell = tf.contrib.rnn.LSTMCell(num_units) 
    cells.append(cell)
cell = tf.contrib.rnn.MultiRNNCell(cells)
output, states = tf.nn.dynamic_rnn(cell, X, dtype=tf.float32)
last = states[-1][0]
logits = tf.contrib.layers.fully_connected(inputs=last,
                                           num_outputs=len(labels),
					   activation_fn=None)

print logits

softmax = tf.nn.softmax_cross_entropy_with_logits(logits=logits,labels=y)                         

cross_entropy = tf.reduce_mean(softmax)
train_step = tf.train.AdamOptimizer(0.001).minimize(cross_entropy)

correct_prediction = tf.equal(tf.argmax(y,1), tf.argmax(logits,1))
accuracy = (tf.reduce_mean(tf.cast(correct_prediction, tf.float32)))*100

with tf.Session() as sess:
    sess.run(tf.global_variables_initializer())
    for i in range(num_epochs):
        print i
        try:
            x_batch, y_batch = get_minibatch(batch_size)
            sess.run(train_step,feed_dict={X:x_batch, y:y_batch})
            if i % 10 == 0: 
                acc = sess.run(accuracy,feed_dict={X:x_batch, y:y_batch})
                print 'accuracy', acc
        except:
            #print 'error'
            continue
               
