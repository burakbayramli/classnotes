
```python
from keras import backend as K
from keras.layers.convolutional import Conv2D, MaxPooling2D
from keras.layers import Input, Dense, Activation
from keras.layers import Reshape, Lambda
from keras.layers.merge import add, concatenate
from keras.models import Model
from keras.layers.recurrent import GRU
from keras.optimizers import SGD
from keras.utils.data_utils import get_file
from keras.preprocessing import image
import keras.callbacks
import util

def ctc_lambda_func(args):
    y_pred, labels, input_length, label_length = args
    # the 2 is critical here since the first couple outputs of the RNN
    # tend to be garbage:
    y_pred = y_pred[:, 2:, :]
    return K.ctc_batch_cost(labels, y_pred, input_length, label_length)

def train():
    # Input Parameters
    img_w = 128
    img_h = 64
    words_per_epoch = 16000
    val_split = 0.2
    val_words = int(words_per_epoch * (val_split))

    # Network parameters
    conv_filters = 16
    kernel_size = (3, 3)
    pool_size = 2
    time_dense_size = 32
    rnn_size = 512
    minibatch_size = 32

    if K.image_data_format() == 'channels_first':
        input_shape = (1, img_w, img_h)
    else:
        input_shape = (img_w, img_h, 1)
    
    act = 'relu'
    input_data = Input(name='the_input', shape=input_shape, dtype='float32')
    print input_data.shape
    inner = Conv2D(conv_filters, kernel_size, padding='same',
                   activation=act, kernel_initializer='he_normal',
                   name='conv1')(input_data)
    print kernel_size
    print inner
    inner = MaxPooling2D(pool_size=(pool_size, pool_size), name='max1')(inner)
    print pool_size
    print inner
    inner = Conv2D(conv_filters, kernel_size, padding='same',
                   activation=act, kernel_initializer='he_normal',
                   name='conv2')(inner)
    print kernel_size
    print act
    print inner
    inner = MaxPooling2D(pool_size=(pool_size, pool_size), name='max2')(inner)
    print inner
    conv_to_rnn_dims = (img_w // (pool_size ** 2), (img_h // (pool_size ** 2)) * conv_filters)
    inner = Reshape(target_shape=conv_to_rnn_dims, name='reshape')(inner)
    print inner
    # cuts down input size going into RNN:
    print time_dense_size
    print inner
    inner = Dense(time_dense_size, activation=act, name='dense1')(inner)
    print inner

    # Two layers of bidirectional GRUs
    # GRU seems to work as well, if not better than LSTM:
    print 'rnn_size', rnn_size
    gru_1 = GRU(rnn_size, return_sequences=True, kernel_initializer='he_normal', name='gru1')(inner)
    print gru_1
    gru_1b = GRU(rnn_size, return_sequences=True, go_backwards=True, kernel_initializer='he_normal', name='gru1_b')(inner)
    print gru_1b
    gru1_merged = add([gru_1, gru_1b])
    print gru1_merged
    gru_2 = GRU(rnn_size, return_sequences=True, kernel_initializer='he_normal', name='gru2')(gru1_merged)
    print gru_2
    gru_2b = GRU(rnn_size, return_sequences=True, go_backwards=True, kernel_initializer='he_normal', name='gru2_b')(gru1_merged)
    print gru_2b

    inner = Dense(len(util.all_chars)+1, kernel_initializer='he_normal',
                  name='dense2')(concatenate([gru_2, gru_2b]))
    print inner
    y_pred = Activation('softmax', name='softmax')(inner)
    Model(inputs=input_data, outputs=y_pred).summary()    

    labels = Input(name='the_labels', shape=[util.absolute_max_string_len], dtype='float32')
    print 'labels', labels
    input_length = Input(name='input_length', shape=[1], dtype='int64')
    print input_length
    label_length = Input(name='label_length', shape=[1], dtype='int64')
    print 'label_length', label_length

    loss_out = Lambda(ctc_lambda_func, output_shape=(1,), name='ctc')([y_pred, labels, input_length, label_length])

    # clipnorm seems to speeds up convergence
    sgd = SGD(lr=0.02, decay=1e-6, momentum=0.9, nesterov=True, clipnorm=5)

    model = Model(inputs=[input_data, labels, input_length, label_length], outputs=loss_out)

    # the loss calc occurs elsewhere, so use a dummy lambda func for the loss
    model.compile(loss={'ctc': lambda y_true, y_pred: y_pred}, optimizer=sgd)
    return model    

m = train()
```

```text
(?, 128, 64, 1)
(3, 3)
Tensor("conv1_3/Relu:0", shape=(?, 128, 64, 16), dtype=float32)
2
Tensor("max1_3/MaxPool:0", shape=(?, 64, 32, 16), dtype=float32)
(3, 3)
relu
Tensor("conv2_3/Relu:0", shape=(?, 64, 32, 16), dtype=float32)
Tensor("max2_3/MaxPool:0", shape=(?, 32, 16, 16), dtype=float32)
Tensor("reshape_3/Reshape:0", shape=(?, 32, 256), dtype=float32)
32
Tensor("reshape_3/Reshape:0", shape=(?, 32, 256), dtype=float32)
Tensor("dense1_3/Relu:0", shape=(?, 32, 32), dtype=float32)
rnn_size 512
Tensor("gru1_3/transpose_1:0", shape=(?, ?, 512), dtype=float32)
Tensor("gru1_b_3/transpose_1:0", shape=(?, ?, 512), dtype=float32)
Tensor("add_2/add:0", shape=(?, ?, 512), dtype=float32)
Tensor("gru2_3/transpose_1:0", shape=(?, ?, 512), dtype=float32)
Tensor("gru2_b_3/transpose_1:0", shape=(?, ?, 512), dtype=float32)
Tensor("dense2_3/add:0", shape=(?, 32, 28), dtype=float32)
____________________________________________________________________________________________________
Layer (type)                     Output Shape          Param #     Connected to                     
====================================================================================================
the_input (InputLayer)           (None, 128, 64, 1)    0                                            
____________________________________________________________________________________________________
conv1 (Conv2D)                   (None, 128, 64, 16)   160         the_input[0][0]                  
____________________________________________________________________________________________________
max1 (MaxPooling2D)              (None, 64, 32, 16)    0           conv1[0][0]                      
____________________________________________________________________________________________________
conv2 (Conv2D)                   (None, 64, 32, 16)    2320        max1[0][0]                       
____________________________________________________________________________________________________
max2 (MaxPooling2D)              (None, 32, 16, 16)    0           conv2[0][0]                      
____________________________________________________________________________________________________
reshape (Reshape)                (None, 32, 256)       0           max2[0][0]                       
____________________________________________________________________________________________________
dense1 (Dense)                   (None, 32, 32)        8224        reshape[0][0]                    
____________________________________________________________________________________________________
gru1 (GRU)                       (None, 32, 512)       837120      dense1[0][0]                     
____________________________________________________________________________________________________
gru1_b (GRU)                     (None, 32, 512)       837120      dense1[0][0]                     
____________________________________________________________________________________________________
add_2 (Add)                      (None, 32, 512)       0           gru1[0][0]                       
                                                                   gru1_b[0][0]                     
____________________________________________________________________________________________________
gru2 (GRU)                       (None, 32, 512)       1574400     add_2[0][0]                      
____________________________________________________________________________________________________
gru2_b (GRU)                     (None, 32, 512)       1574400     add_2[0][0]                      
____________________________________________________________________________________________________
concatenate_2 (Concatenate)      (None, 32, 1024)      0           gru2[0][0]                       
                                                                   gru2_b[0][0]                     
____________________________________________________________________________________________________
dense2 (Dense)                   (None, 32, 28)        28700       concatenate_2[0][0]              
____________________________________________________________________________________________________
softmax (Activation)             (None, 32, 28)        0           dense2[0][0]                     
====================================================================================================
Total params: 4,862,444
Trainable params: 4,862,444
Non-trainable params: 0
____________________________________________________________________________________________________
labels Tensor("the_labels_3:0", shape=(?, 16), dtype=float32)
Tensor("input_length_3:0", shape=(?, 1), dtype=int64)
label_length Tensor("label_length_3:0", shape=(?, 1), dtype=int64)
```

```python
outputs = {'ctc': np.ones((1,1)) * 1 }
print outputs
import util
data = util.get_minibatch()
```

```text
{'ctc': array([[ 1.]])}
```

```python
m.fit(x=data[0], y=outputs, batch_size=1, epochs=1)
```














