from glob import glob
import librosa
import time, re, os, random
import numpy as np
import tensorflow as tf
from utils import convert_inputs_to_ctc_format

# Some configs
num_features = 13
# Accounting the 0th index +  space + blank label = 28 characters
num_classes = ord('z') - ord('a') + 1 + 1 + 1
print ('num_classes %d' % num_classes)

# Hyper-parameters
num_epochs = 100
num_hidden = 100
num_layers = 1
batch_size = 10
num_batches_per_epoch = 10

# TODO: test also with import scipy.io.wavfile as wav; fs, audio = wav.read(audio_filename)
def read_audio_from_filename(filename, sample_rate):
    # import scipy.io.wavfile as wav
    # fs, audio = wav.read(filename)
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
        inputs = tf.placeholder(tf.float32, [None, None, num_features])

        # Here we use sparse_placeholder that will generate a
        # SparseTensor required by ctc_loss op.
        targets = tf.sparse_placeholder(tf.int32)

        # 1d array of size [batch_size]
        seq_len = tf.placeholder(tf.int32, [None])

        # Defining the cell
        # Can be:
        cell = tf.contrib.rnn.LSTMCell(num_hidden, state_is_tuple=True)

        # Stacking rnn cells
        stack = tf.contrib.rnn.MultiRNNCell([cell] * num_layers,
                                            state_is_tuple=True)

        # The second output is the last state and we will no use that
        outputs, _ = tf.nn.dynamic_rnn(stack, inputs, seq_len, dtype=tf.float32)

        shape = tf.shape(inputs)
        batch_s, max_time_steps = shape[0], shape[1]

        # Reshaping to apply the same weights over the timesteps
        outputs = tf.reshape(outputs, [-1, num_hidden])

        # Truncated normal with mean 0 and stdev=0.1
        # Tip: Try another initialization
        W = tf.Variable(tf.truncated_normal([num_hidden,
                                             num_classes],
                                            stddev=0.1))
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
        decoded, log_prob = tf.nn.ctc_greedy_decoder(logits, seq_len)

        # Inaccuracy: label error rate
        ler = tf.reduce_mean(tf.edit_distance(tf.cast(decoded[0], tf.int32),
                                              targets))

    files = find_files("/home/burak/Downloads/train/audio")
        
    with tf.Session(graph=graph) as session:

        tf.global_variables_initializer().run()

        for curr_epoch in range(num_epochs):
            train_cost = train_ler = 0
            for batch in range(num_batches_per_epoch):
                filename = random.choice(files)
                txt = re.findall(".*/(.*?)/.*?.wav",filename)[0]
                audio = read_audio_from_filename(filename, 16000)
                out = convert_inputs_to_ctc_format(audio,16000,txt)
                train_inputs, train_targets, train_seq_len, original = out

                feed = {inputs: train_inputs,
                        targets: train_targets,
                        seq_len: train_seq_len}

                batch_cost, _ = session.run([cost, optimizer], feed)
                train_cost += batch_cost * batch_size
                train_ler += session.run(ler, feed_dict=feed) * batch_size
                
                #print 'train_ler in for', train_ler
                
            print 'train_cost', train_cost, 'train_ler', train_ler / num_batches_per_epoch
            


if __name__ == '__main__':
    run_ctc()
