from tensorflow.contrib.keras import regularizers as R
from tensorflow.contrib.keras import models as M
from tensorflow.contrib.keras import layers as L
from tensorflow.contrib.keras import backend as K
from util import flatten_idx, random_transform, idx_transformations
import numpy as np, util, random

mfile = "/tmp/alphago-zero.h5"

class PolicyValue:
    def __init__(self, model):
        self.model = model

    def save(self):
        self.model.save_weights(mfile)
        
    def load(self):
        self.model.load_weights(mfile)
        
    def eval_policy_state(self, state):
        x = util.get_board(state).reshape(1, 17, 9, 9)
        probs1 = self.model.predict(x)[0][0]
        probs2 = probs1[1:].reshape(9,9)
        res_probs = [(action,probs2[action]) \
                     for action in state.get_legal_moves() if action]
        res_probs.append((None, probs1[0]))
        return res_probs

    def eval_value_state(self, state):
        x = util.get_board(state).reshape(1, 17, 9, 9)
        return self.model.predict(x)[1][0][0]
            
    @staticmethod
    def create_network(**kwargs):
        model_input = L.Input(shape=(17, 9, 9))
        print model_input
        
        convolution_path = L.Convolution2D(
            input_shape=(),
            filters=64,
            kernel_size=3,
            activation='linear',
            padding='same',
            kernel_regularizer=R.l2(.0001),
            bias_regularizer=R.l2(.0001))(model_input)
        print convolution_path
        convolution_path = L.BatchNormalization(
            beta_regularizer=R.l2(.0001),
            gamma_regularizer=R.l2(.0001))(convolution_path)
        print convolution_path
        convolution_path = L.Activation('relu')(convolution_path)

        convolution_path = L.Convolution2D(
            input_shape=(),
            filters=128,
            kernel_size=3,
            activation='linear',
            padding='same',
            kernel_regularizer=R.l2(.0001),
            bias_regularizer=R.l2(.0001))(convolution_path)
        print convolution_path
        convolution_path = L.BatchNormalization(
            beta_regularizer=R.l2(.0001),
            gamma_regularizer=R.l2(.0001))(convolution_path)
        print convolution_path
        convolution_path = L.Activation('relu')(convolution_path)


        print '------------- value -------------------'            
        # policy head
        policy_path = L.Convolution2D(
            input_shape=(),
            filters=2,
            kernel_size=1,
            activation='linear',
            padding='same',
            kernel_regularizer=R.l2(.0001),
            bias_regularizer=R.l2(.0001))(convolution_path)
        print policy_path
        policy_path = L.BatchNormalization(
                beta_regularizer=R.l2(.0001),
                gamma_regularizer=R.l2(.0001))(policy_path)
        policy_path = L.Activation('relu')(policy_path)
        print policy_path
        policy_path = L.Flatten()(policy_path)
        print policy_path
        policy_path = L.Dense(
                (9*9)+1,
                kernel_regularizer=R.l2(.0001),
                bias_regularizer=R.l2(.0001))(policy_path)
        policy_output = L.Activation('softmax')(policy_path)
        print 'policy_output', policy_output

        print '------------- policy -------------------'
        
        # value head
        value_path = L.Convolution2D(
            input_shape=(),
            filters=1,
            kernel_size=1,
            activation='linear',
            padding='same',
            kernel_regularizer=R.l2(.0001),
            bias_regularizer=R.l2(.0001))(convolution_path)
        print value_path
        value_path = L.BatchNormalization(
                beta_regularizer=R.l2(.0001),
                gamma_regularizer=R.l2(.0001))(value_path)
        value_path = L.Activation('relu')(value_path)
        print value_path
        value_path = L.Flatten()(value_path)
        print value_path
        value_path = L.Dense(
                256,
                kernel_regularizer=R.l2(.0001),
                bias_regularizer=R.l2(.0001))(value_path)
        print value_path
        value_path = L.Activation('relu')(value_path)
        print value_path
        value_path = L.Dense(
                1,
                kernel_regularizer=R.l2(.0001),
                bias_regularizer=R.l2(.0001))(value_path)
        print value_path
        value_output = L.Activation('tanh')(value_path)
        print value_path

        return M.Model(inputs=[model_input], outputs=[policy_output, value_output])
