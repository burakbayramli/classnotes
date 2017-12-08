
```python
from python_speech_features import mfcc
import numpy as np
import tensorflow as tf
import scipy.io.wavfile as wav
from glob import glob
import time, re, os, random
import numpy as np
import librosa

num_epochs = 1000
num_hidden = 100
num_layers = 4
num_batches_per_epoch = 10
sample_rate=16000
num_features = 26

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
```

```python
f = '/home/burak/Downloads/goog_tf_speech/audio/down/00176480_nohash_0.wav'
res = audiofile_to_input_vector(f, 26, 9)
res = res.astype('float32')
print res.shape

f = '/home/burak/Downloads/goog_tf_speech/audio/down/00176480_nohash_0.wav'
res = audiofile_to_input_vector(f, 26, 9)
res = res.astype('float32')
print res.shape
```

```text
(50, 494)
(50, 494)
```


```python
num_units = 200
num_layers = 3
     
graph = tf.Graph()
with graph.as_default():

    dropout = tf.placeholder(tf.float32)

    # e.g: log filter bank or MFCC features
    # Has size [batch_size, max_step_size, num_features], but the
    # batch_size and max_step_size can vary along each step
    inputs = tf.placeholder(tf.float32, [None, None, 494])

    cells = []
    for _ in range(num_layers):
    	cell = tf.contrib.rnn.GRUCell(num_units)  # Or LSTMCell(num_units)
    	cell = tf.contrib.rnn.DropoutWrapper(cell, output_keep_prob=1.0 - dropout)
    	cells.append(cell)
    cell = tf.contrib.rnn.MultiRNNCell(cells)

    data = tf.placeholder(tf.float32, [None, None, 28])
    output, state = tf.nn.dynamic_rnn(cell, data, dtype=tf.float32)
```



















































