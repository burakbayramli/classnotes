# -*- coding: utf-8 -*-
import os, util
import datetime
import numpy as np
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

def ctc_lambda_func(args):
    y_pred, labels, input_length, label_length = args
    # the 2 is critical here since the first couple outputs of the RNN
    # tend to be garbage:
    y_pred = y_pred[:, 2:, :]
    return K.ctc_batch_cost(labels, y_pred, input_length, label_length)

def get_model(img_w,img_h,minibatch_size,pool_size):
    conv_filters = 20
    kernel_size = (2, 2)
    time_dense_size = 32
    rnn_size = 256

    input_shape = (img_w, img_h, 1)
    
    img_gen = util.TextImageGenerator(minibatch_size=minibatch_size,
                                      img_w=img_w,
                                      img_h=img_h,
                                      downsample_factor=(pool_size ** 2),
                                      absolute_max_string_len=12
    )
        
    act = 'relu'
    
    input_data = Input(name='the_input', shape=input_shape, dtype='float32')
    
    inner = Conv2D(conv_filters, kernel_size, padding='same',
                   activation=act, kernel_initializer='he_normal',
                   name='conv1')(input_data)
    
    inner = MaxPooling2D(pool_size=(pool_size, pool_size), name='max1')(inner)
    
    inner = Conv2D(conv_filters, kernel_size, padding='same',
                   activation=act, kernel_initializer='he_normal',
                   name='conv2')(inner)
    
    inner = MaxPooling2D(pool_size=(pool_size, pool_size), name='max2')(inner)
    
    conv_to_rnn_dims = (img_w // (pool_size ** 2), (img_h // (pool_size ** 2))
                        * conv_filters)

    inner = Reshape(target_shape=conv_to_rnn_dims, name='reshape')(inner)
    inner = Dense(time_dense_size, activation=act, name='dense1')(inner)
    
    gru_1 = GRU(rnn_size, return_sequences=True,
                kernel_initializer='he_normal', name='gru1')(inner)
    
    gru_1b = GRU(rnn_size, return_sequences=True,
                 go_backwards=True, kernel_initializer='he_normal',
                 name='gru1_b')(inner)

    gru1_merged = add([gru_1, gru_1b])

    gru_2 = GRU(rnn_size, return_sequences=True,
                kernel_initializer='he_normal', name='gru2')(gru1_merged)
    
    gru_2b = GRU(rnn_size, return_sequences=True,
                 go_backwards=True, kernel_initializer='he_normal',
                 name='gru2_b')(gru1_merged)

    inner = Dense(img_gen.get_output_size(),
                  kernel_initializer='he_normal',
                  name='dense2')(concatenate([gru_2, gru_2b]))
    
    y_pred = Activation('softmax', name='softmax')(inner)
    
    Model(inputs=input_data, outputs=y_pred).summary()

    labels = Input(name='the_labels',
                   shape=[img_gen.absolute_max_string_len],
                   dtype='float32')

    input_length = Input(name='input_length', shape=[1], dtype='int64')

    label_length = Input(name='label_length', shape=[1], dtype='int64')

    loss_out = Lambda(ctc_lambda_func, output_shape=(1,), name='ctc')([y_pred,
                                                                       labels,
                                                                       input_length,
                                                                       label_length])

    sgd = SGD(lr=0.02, decay=1e-6, momentum=0.9, nesterov=True, clipnorm=5)

    model = Model(inputs=[input_data,
                          labels,
                          input_length,
                          label_length], outputs=loss_out)

    model.compile(loss={'ctc': lambda y_true, y_pred: y_pred}, optimizer=sgd)

    test_func = K.function([input_data], [y_pred])

    return model, test_func
        

if __name__ == '__main__':
    pool_size = 3
    img_w = 256
    img_h = 64
    minibatch_size = 10
    img_gen = util.TextImageGenerator(minibatch_size=minibatch_size,
                                      img_w=img_w,
                                      img_h=img_h,
                                      downsample_factor=(pool_size ** 2),
                                      absolute_max_string_len=12
    )
    model, dummy = get_model(img_w,img_h,minibatch_size,pool_size)
    mfile = "/tmp/ocr.h5"
    if os.path.isfile(mfile):
        print 'Loaded', mfile
        model.load_weights(mfile)

    model.fit_generator(generator=img_gen.next_train(),
                        steps_per_epoch=1000,
                        epochs=1,
                        validation_steps=0,
                        callbacks=[img_gen],
                        initial_epoch=0)

    model.save(mfile)
    
