
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
import pandas as pd
import numpy as np
import util, random

def ctc_lambda_func(args):
    y_pred, labels, input_length, label_length = args
    # the 2 is critical here since the first couple outputs of the RNN
    # tend to be garbage:
    y_pred = y_pred[:, 2:, :]
    return K.ctc_batch_cost(labels, y_pred, input_length, label_length)

def get_model(img_w,img_h):
    # Input Parameters
    val_split = 0.2

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
    #print input_data.shape
    inner = Conv2D(conv_filters, kernel_size, padding='same',
                   activation=act, kernel_initializer='he_normal',
                   name='conv1')(input_data)
    #print kernel_size
    #print inner
    inner = MaxPooling2D(pool_size=(pool_size, pool_size), name='max1')(inner)
    #print pool_size
    #print inner
    inner = Conv2D(conv_filters, kernel_size, padding='same',
                   activation=act, kernel_initializer='he_normal',
                   name='conv2')(inner)
    print kernel_size
    #print act
    #print inner
    inner = MaxPooling2D(pool_size=(pool_size, pool_size), name='max2')(inner)
    #print inner
    conv_to_rnn_dims = (img_w // (pool_size ** 2), (img_h // (pool_size ** 2)) * conv_filters)
    inner = Reshape(target_shape=conv_to_rnn_dims, name='reshape')(inner)
    #print inner
    # cuts down input size going into RNN:
    #print time_dense_size
    #print inner
    inner = Dense(time_dense_size, activation=act, name='dense1')(inner)
    #print inner

    # Two layers of bidirectional GRUs
    # GRU seems to work as well, if not better than LSTM:
    #print 'rnn_size', rnn_size
    gru_1 = GRU(rnn_size, return_sequences=True, kernel_initializer='he_normal', name='gru1')(inner)
    #print gru_1
    gru_1b = GRU(rnn_size, return_sequences=True, go_backwards=True, kernel_initializer='he_normal', name='gru1_b')(inner)
    #print gru_1b
    gru1_merged = add([gru_1, gru_1b])
    #print gru1_merged
    gru_2 = GRU(rnn_size, return_sequences=True, kernel_initializer='he_normal', name='gru2')(gru1_merged)
    #print gru_2
    gru_2b = GRU(rnn_size, return_sequences=True, go_backwards=True, kernel_initializer='he_normal', name='gru2_b')(gru1_merged)
    #print gru_2b

    inner = Dense(len(util.all_chars)+1, kernel_initializer='he_normal',
                  name='dense2')(concatenate([gru_2, gru_2b]))
    #print inner
    y_pred = Activation('softmax', name='softmax')(inner)
    Model(inputs=input_data, outputs=y_pred).summary()    

    labels = Input(name='the_labels', shape=[util.absolute_max_string_len], dtype='float32')
    #print 'labels', labels
    input_length = Input(name='input_length', shape=[1], dtype='int64')
    #print input_length
    label_length = Input(name='label_length', shape=[1], dtype='int64')
    #print 'label_length', label_length

    loss_out = Lambda(ctc_lambda_func, output_shape=(1,), name='ctc')([y_pred, labels, input_length, label_length])

    # clipnorm seems to speeds up convergence
    sgd = SGD(lr=0.02, decay=1e-3, momentum=0.9, nesterov=True, clipnorm=5)

    model = Model(inputs=[input_data, labels, input_length, label_length], outputs=loss_out)

    # the loss calc occurs elsewhere, so use a dummy lambda func for the loss
    model.compile(loss={'ctc': lambda y_true, y_pred: y_pred}, optimizer=sgd)
    return model    

img_w = 128
img_h = 64
m = get_model(img_w,img_h)
np.random.seed(0)
random.seed(0)

batch_size = 5
outputs = {'ctc': np.ones((batch_size,1)) * batch_size }
#print outputs
import util
for i in range(1000):
    data = util.get_minibatch(img_w,img_h,batch_size)
    m.fit(x=data[0], y=outputs, batch_size=batch_size, epochs=1)
