
```python
from python_speech_features import mfcc
import numpy as np
import tensorflow as tf
import scipy.io.wavfile as wav
from glob import glob
import time, re, os, random
import numpy as np

num_epochs = 1000
num_hidden = 100
num_layers = 4
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
```

```text
23682
```

```python
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
	#y.append(labels.index(label))
	y[i, labels.index(label)] = 1.0
    res = np.array(res)
    y = np.array(y)
    return res, y

data,y = get_minibatch(1)
print data.shape, y.shape
print y[0]
#print y[1]
```

```text
(50, 494)
(50, 494)
(1, 50, 494) (1, 10)
[ 0.  0.  0.  0.  0.  0.  0.  0.  1.  0.]
```


```python
import tensorflow as tf

num_units = 200
num_layers = 3
batch_size = 2

tf.reset_default_graph()

# e.g: log filter bank or MFCC features
# Has size [batch_size, max_step_size, num_features], but the
# batch_size and max_step_size can vary along each step
X = tf.placeholder(tf.float32, [batch_size, 50, 494])
#y = tf.placeholder(tf.int32, [None])
y = tf.placeholder(tf.float32, shape=[batch_size, len(labels)])

basic_cell = tf.contrib.rnn.BasicRNNCell(num_units=num_units)
multi_layer_cell = tf.contrib.rnn.MultiRNNCell([basic_cell] * num_units)
cells = []
for _ in range(num_layers):
    cell = tf.contrib.rnn.LSTMCell(num_units) 
    cells.append(cell)
cell = tf.contrib.rnn.MultiRNNCell(cells)
output, states = tf.nn.dynamic_rnn(cell, X, dtype=tf.float32)
last = states[-1][0]
print last
print 'len(labels)',len(labels)
logits = tf.contrib.layers.fully_connected(inputs=last,
                                           num_outputs=len(labels),
					   activation_fn=tf.nn.relu)
print logits
xentropy = tf.losses.softmax_cross_entropy(y, logits)
loss = tf.reduce_mean(xentropy)
optimizer = tf.train.AdamOptimizer(learning_rate=0.001)
training_op = optimizer.minimize(loss)
init = tf.global_variables_initializer()
with tf.Session() as sess:
    batch_x, batch_y = get_minibatch(2)
    sess.run(training_op, feed_dict={X: batch_x, y: batch_y})
    

```

```text
Tensor("rnn/while/Exit_6:0", shape=(2, 200), dtype=float32)
len(labels) 10
Tensor("fully_connected/Relu:0", shape=(2, 10), dtype=float32)

FailedPreconditionErrorTraceback (most recent call last)
<ipython-input-1-4643af20cfa7> in <module>()
     38 with tf.Session() as sess:
     39     batch_x, batch_y = get_minibatch(2)
---> 40     sess.run(training_op, feed_dict={X: batch_x, y: batch_y})
     41     
     42 

/usr/local/lib/python2.7/dist-packages/tensorflow/python/client/session.pyc in run(self, fetches, feed_dict, options, run_metadata)
    887     try:
    888       result = self._run(None, fetches, feed_dict, options_ptr,
--> 889                          run_metadata_ptr)
    890       if run_metadata:
    891         proto_data = tf_session.TF_GetBuffer(run_metadata_ptr)

/usr/local/lib/python2.7/dist-packages/tensorflow/python/client/session.pyc in _run(self, handle, fetches, feed_dict, options, run_metadata)
   1118     if final_fetches or final_targets or (handle and feed_dict_tensor):
   1119       results = self._do_run(handle, final_targets, final_fetches,
-> 1120                              feed_dict_tensor, options, run_metadata)
   1121     else:
   1122       results = []

/usr/local/lib/python2.7/dist-packages/tensorflow/python/client/session.pyc in _do_run(self, handle, target_list, fetch_list, feed_dict, options, run_metadata)
   1315     if handle is None:
   1316       return self._do_call(_run_fn, self._session, feeds, fetches, targets,
-> 1317                            options, run_metadata)
   1318     else:
   1319       return self._do_call(_prun_fn, self._session, handle, feeds, fetches)

/usr/local/lib/python2.7/dist-packages/tensorflow/python/client/session.pyc in _do_call(self, fn, *args)
   1334         except KeyError:
   1335           pass
-> 1336       raise type(e)(node_def, op, message)
   1337 
   1338   def _extend_graph(self):

FailedPreconditionError: Attempting to use uninitialized value fully_connected/biases
	 [[Node: fully_connected/biases/read = Identity[T=DT_FLOAT, _class=["loc:@fully_connected/biases"], _device="/job:localhost/replica:0/task:0/device:CPU:0"](fully_connected/biases)]]

Caused by op u'fully_connected/biases/read', defined at:
  File "<string>", line 1, in <module>
  File "/usr/local/lib/python2.7/dist-packages/Pymacs.py", line 132, in main
    lisp._protocol.loop()
  File "/usr/local/lib/python2.7/dist-packages/Pymacs.py", line 268, in loop
    value = eval(text)
  File "<string>", line 1, in <module>
  File "/home/burak/Documents/kod/site-lisp/ipython-md.py", line 81, in run_py_code
    res_code = get_ip().run_cell(content)
  File "/usr/local/lib/python2.7/dist-packages/IPython/core/interactiveshell.py", line 2718, in run_cell
    interactivity=interactivity, compiler=compiler, result=result)
  File "/usr/local/lib/python2.7/dist-packages/IPython/core/interactiveshell.py", line 2822, in run_ast_nodes
    if self.run_code(code, result):
  File "/usr/local/lib/python2.7/dist-packages/IPython/core/interactiveshell.py", line 2882, in run_code
    exec(code_obj, self.user_global_ns, self.user_ns)
  File "<ipython-input-1-4643af20cfa7>", line 31, in <module>
    activation_fn=tf.nn.relu)
  File "/usr/local/lib/python2.7/dist-packages/tensorflow/contrib/framework/python/ops/arg_scope.py", line 181, in func_with_args
    return func(*args, **current_args)
  File "/usr/local/lib/python2.7/dist-packages/tensorflow/contrib/layers/python/layers/layers.py", line 1639, in fully_connected
    outputs = layer.apply(inputs)
  File "/usr/local/lib/python2.7/dist-packages/tensorflow/python/layers/base.py", line 671, in apply
    return self.__call__(inputs, *args, **kwargs)
  File "/usr/local/lib/python2.7/dist-packages/tensorflow/python/layers/base.py", line 559, in __call__
    self.build(input_shapes[0])
  File "/usr/local/lib/python2.7/dist-packages/tensorflow/python/layers/core.py", line 145, in build
    trainable=True)
  File "/usr/local/lib/python2.7/dist-packages/tensorflow/python/layers/base.py", line 458, in add_variable
    trainable=trainable and self.trainable)
  File "/usr/local/lib/python2.7/dist-packages/tensorflow/python/ops/variable_scope.py", line 1203, in get_variable
    constraint=constraint)
  File "/usr/local/lib/python2.7/dist-packages/tensorflow/python/ops/variable_scope.py", line 1092, in get_variable
    constraint=constraint)
  File "/usr/local/lib/python2.7/dist-packages/tensorflow/python/ops/variable_scope.py", line 417, in get_variable
    return custom_getter(**custom_getter_kwargs)
  File "/usr/local/lib/python2.7/dist-packages/tensorflow/contrib/layers/python/layers/layers.py", line 1539, in layer_variable_getter
    return _model_variable_getter(getter, *args, **kwargs)
  File "/usr/local/lib/python2.7/dist-packages/tensorflow/contrib/layers/python/layers/layers.py", line 1531, in _model_variable_getter
    custom_getter=getter, use_resource=use_resource)
  File "/usr/local/lib/python2.7/dist-packages/tensorflow/contrib/framework/python/ops/arg_scope.py", line 181, in func_with_args
    return func(*args, **current_args)
  File "/usr/local/lib/python2.7/dist-packages/tensorflow/contrib/framework/python/ops/variables.py", line 262, in model_variable
    use_resource=use_resource)
  File "/usr/local/lib/python2.7/dist-packages/tensorflow/contrib/framework/python/ops/arg_scope.py", line 181, in func_with_args
    return func(*args, **current_args)
  File "/usr/local/lib/python2.7/dist-packages/tensorflow/contrib/framework/python/ops/variables.py", line 217, in variable
    use_resource=use_resource)
  File "/usr/local/lib/python2.7/dist-packages/tensorflow/python/ops/variable_scope.py", line 394, in _true_getter
    use_resource=use_resource, constraint=constraint)
  File "/usr/local/lib/python2.7/dist-packages/tensorflow/python/ops/variable_scope.py", line 805, in _get_single_variable
    constraint=constraint)
  File "/usr/local/lib/python2.7/dist-packages/tensorflow/python/ops/variables.py", line 213, in __init__
    constraint=constraint)
  File "/usr/local/lib/python2.7/dist-packages/tensorflow/python/ops/variables.py", line 356, in _init_from_args
    self._snapshot = array_ops.identity(self._variable, name="read")
  File "/usr/local/lib/python2.7/dist-packages/tensorflow/python/ops/array_ops.py", line 125, in identity
    return gen_array_ops.identity(input, name=name)
  File "/usr/local/lib/python2.7/dist-packages/tensorflow/python/ops/gen_array_ops.py", line 2071, in identity
    "Identity", input=input, name=name)
  File "/usr/local/lib/python2.7/dist-packages/tensorflow/python/framework/op_def_library.py", line 787, in _apply_op_helper
    op_def=op_def)
  File "/usr/local/lib/python2.7/dist-packages/tensorflow/python/framework/ops.py", line 2956, in create_op
    op_def=op_def)
  File "/usr/local/lib/python2.7/dist-packages/tensorflow/python/framework/ops.py", line 1470, in __init__
    self._traceback = self._graph._extract_stack()  # pylint: disable=protected-access

FailedPreconditionError (see above for traceback): Attempting to use uninitialized value fully_connected/biases
	 [[Node: fully_connected/biases/read = Identity[T=DT_FLOAT, _class=["loc:@fully_connected/biases"], _device="/job:localhost/replica:0/task:0/device:CPU:0"](fully_connected/biases)]]

```







































































