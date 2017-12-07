from python_speech_features import mfcc
import numpy as np
import tensorflow as tf
import scipy.io.wavfile as wav
from glob import glob
import time, re, os, random
import numpy as np
import librosa

num_epochs = 1000
num_hidden = 50
num_layers = 4
num_batches_per_epoch = 10
sample_rate=16000
num_features = 26
# Accounting the 0th index +  space + blank label = 28 characters
num_classes = ord('z') - ord('a') + 1 + 1 + 1
print ('num_classes %d' % num_classes)

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

def convert_inputs_to_ctc_format(inputs, target_text):
    #print('convert_inputs_to_ctc_format target_text:' + target_text)
    #inputs = mfcc(audio, samplerate=fs, numcep=num_features)
    # Transform in 3D array
    train_inputs = np.asarray(inputs[np.newaxis, :])
    train_inputs = (train_inputs - np.mean(train_inputs)) / np.std(train_inputs)
    train_seq_len = [train_inputs.shape[1]]

    # Get only the words between [a-z] and replace period for none
    original = ' '.join(target_text.strip().lower().split(' ')).replace('.', '').replace('?', '').replace(',', '').replace("'", '').replace('!', '').replace('-', '')
    #print('original:' + original)
    targets = original.replace(' ', '  ')
    targets = targets.split(' ')

    # Adding blank label
    targets = np.hstack([SPACE_TOKEN if x == '' else list(x) for x in targets])

    # Transform char into index
    targets = np.asarray([SPACE_INDEX if x == SPACE_TOKEN else ord(x) - FIRST_INDEX
                          for x in targets])

    # Creating sparse representation to feed the placeholder
    train_targets = sparse_tuple_from([targets])

    return train_inputs, train_targets, train_seq_len, original


def sparse_tuple_from(sequences, dtype=np.int32):
    indices = []
    values = []
    for n, seq in enumerate(sequences):
        indices.extend(zip([n] * len(seq), range(len(seq))))
        values.extend(seq)

    indices = np.asarray(indices, dtype=np.int64)
    values = np.asarray(values, dtype=dtype)
    shape = np.asarray([len(sequences), np.asarray(indices).max(0)[1] + 1], dtype=np.int64)

    return indices, values, shape

def read_audio_from_filename(filename, sample_rate):
    audio, _ = librosa.load(filename, sr=sample_rate, mono=True)
    audio = audio.reshape(-1, 1)
    return audio

def find_files(directory, pattern='.wav'):
    """Recursively finds all files matching the pattern."""
    files = []
    for root, directories, filenames in os.walk(directory):
        for filename in filenames: 
            path = os.path.join(root,filename)
            if pattern in path: files.append(path)    
    res = sorted(files)
    return res

def run_ctc():
    graph = tf.Graph()
    with graph.as_default():
        # e.g: log filter bank or MFCC features
        # Has size [batch_size, max_step_size, num_features], but the
        # batch_size and max_step_size can vary along each step
        inputs = tf.placeholder(tf.float32, [None, None, 494])

        # Here we use sparse_placeholder that will generate a
        # SparseTensor required by ctc_loss op.
        targets = tf.sparse_placeholder(tf.int32)

        # 1d array of size [batch_size]
        seq_len = tf.placeholder(tf.int32, [None])

        network = tf.contrib.rnn.MultiRNNCell([tf.contrib.rnn.BasicRNNCell(num_hidden)
                                               for _ in range(num_layers)], state_is_tuple=True)

        
        # The second output is the last state and we will no use that
        outputs, _ = tf.nn.dynamic_rnn(network, inputs, seq_len, dtype=tf.float32)

        shape = tf.shape(inputs)
        batch_s, max_time_steps = shape[0], shape[1]

        # Reshaping to apply the same weights over the timesteps
        outputs = tf.reshape(outputs, [-1, num_hidden])

        # Truncated normal with mean 0 and stdev=0.1
        # Tip: Try another initialization
        W = tf.Variable(tf.truncated_normal([num_hidden, num_classes], stddev=0.1))
        # Zero initialization
        # Tip: Is tf.zeros_initializer the same?
        b = tf.Variable(tf.constant(0., shape=[num_classes]))

        # Doing the affine projection
        logits = tf.matmul(outputs, W) + b

        # Reshaping back to the original shape
        logits = tf.reshape(logits, [batch_s, -1, num_classes])

        # Time major
        logits = tf.transpose(logits, (1, 0, 2))

        loss = tf.nn.ctc_loss(targets, logits, seq_len)
        cost = tf.reduce_mean(loss)

        optimizer = tf.train.MomentumOptimizer(learning_rate=0.005,
                                               momentum=0.9).minimize(cost)

        # Option 2: tf.contrib.ctc.ctc_beam_search_decoder
        # (it's slower but you'll get better results)
        #decoded, log_prob = tf.nn.ctc_greedy_decoder(logits, seq_len)
        decoded, log_prob = tf.nn.ctc_beam_search_decoder(logits, seq_len)

        # Inaccuracy: label error rate
        ler = tf.reduce_mean(tf.edit_distance(tf.cast(decoded[0], tf.int32),
                                              targets))

    files = find_files("/home/burak/Downloads/goog_tf_speech")
        
    with tf.Session(graph=graph) as session:

        tf.global_variables_initializer().run()

        saver = tf.train.Saver()
        #saver.restore(session, mfile)

        for curr_epoch in range(num_epochs):
            train_cost = train_ler = 0
            for batch in range(num_batches_per_epoch):
                filename = random.choice(files)
                txt = re.findall(".*/(.*?)/.*?.wav",filename)[0]
                #audio = read_audio_from_filename(filename, sample_rate)
                audio = audiofile_to_input_vector(filename, 26, 9)
                out = convert_inputs_to_ctc_format(audio,txt)
                train_inputs, train_targets, train_seq_len, original = out

                feed = {inputs: train_inputs,
                        targets: train_targets,
                        seq_len: train_seq_len}

                batch_cost, _ = session.run([cost, optimizer], feed)
                train_ler = session.run(ler, feed_dict=feed)
                
                print 'batch_cost', batch_cost, 'train_ler', train_ler

            # Decoding
            d = session.run(decoded[0], feed_dict=feed)
            str_decoded = ''.join([chr(x) for x in np.asarray(d[1]) + FIRST_INDEX])
            # Replacing blank label to none
            str_decoded = str_decoded.replace(chr(ord('z') + 1), '')
            # Replacing space label to space
            str_decoded = str_decoded.replace(chr(ord('a') - 1), ' ')

            print('Original: %s' % original)
            print('Decoded: %s' % str_decoded)
                
            if curr_epoch % 10 == 0: saver.save(session, mfile)
            

if __name__ == '__main__':
    run_ctc()
