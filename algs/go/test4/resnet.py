from tensorflow.contrib.keras import regularizers as R
from tensorflow.contrib.keras import models as M
from tensorflow.contrib.keras import layers as L
from tensorflow.contrib.keras import backend as K
from util import flatten_idx, random_transform, idx_transformations
import numpy as np, util, random

mfile = "/tmp/alphago-zero.h5"

class PolicyValue:
    """uses a convolutional neural network to evaluate the state of the game
    and compute a probability distribution over the next action
    and value of the current state.
    """

    def __init__(self, model):
        self.model = model

    def save(self):
        self.model.save_weights(mfile)
        
    def load(self):
        self.model.load_weights(mfile)
        
    def eval_policy_state(self, state):
        #return [(action, random.random()) for action in state.get_legal_moves()]
        x = util.get_board(state).reshape(1, 17, 9, 9)
        probs1 = self.model.predict(x)[0][0]
        probs2 = probs1[1:].reshape(9,9)
        res_probs = [(action,probs2[action]) for action in state.get_legal_moves() if action]
        res_probs.append((None, probs1[0]))
        return res_probs

    def eval_value_state(self, state):
        x = util.get_board(state).reshape(1, 17, 9, 9)
        return self.model.predict(x)[1][0][0]
            
    @staticmethod
    def create_network(**kwargs):
        """construct a convolutional neural network with Residual blocks.
        Arguments are the same as with the default CNNPolicy network, except the default
        number of layers is 20 plus a new n_skip parameter

        Keword Arguments:
        - input_dim:             depth of features to be processed by first layer (default 17)
        - board:                 width of the go board to be processed (default 19)
        - filters_per_layer:     number of filters used on every layer (default 256)
        - layers:                number of residual blocks (default 19)
        - filter_width:          width of filter
                                 Must be odd.
        """
        defaults = {
            "input_dim": 17,
            "board": 9,
            "filters_per_layer": 64,
            "layers": 9,
            "filter_width": 3
        }

        # copy defaults, but override with anything in kwargs
        params = defaults
        params.update(kwargs)

        # create the network using Keras' functional API,
        model_input = L.Input(shape=(params["input_dim"], params["board"], params["board"]))
        print model_input
        # create first layer
        convolution_path = L.Convolution2D(
            input_shape=(),
            filters=params["filters_per_layer"],
            kernel_size=params["filter_width"],
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
        def add_resnet_unit(path, **params):
            block_input = path
            # add Conv2D
            path = L.Convolution2D(
                filters=params["filters_per_layer"],
                kernel_size=params["filter_width"],
                activation='linear',
                padding='same',
                kernel_regularizer=R.l2(.0001),
                bias_regularizer=R.l2(.0001))(path)
            print path
            path = L.BatchNormalization(
                    beta_regularizer=R.l2(.0001),
                    gamma_regularizer=R.l2(.0001))(path)
            print path
            path = L.Activation('relu')(path)
            print path
            path = L.Convolution2D(
                filters=params["filters_per_layer"],
                kernel_size=params["filter_width"],
                activation='linear',
                padding='same',
                kernel_regularizer=R.l2(.0001),
                bias_regularizer=R.l2(.0001))(path)
            print path
            path = L.BatchNormalization(
                    beta_regularizer=R.l2(.0001),
                    gamma_regularizer=R.l2(.0001))(path)
            print path
            path = L.Add()([block_input, path])
            print path
            path = L.Activation('relu')(path)
            print path
            return path

        # create all other layers
        for _ in range(params['layers']):
            convolution_path = add_resnet_unit(convolution_path, **params)

        print '------------- policy -------------------'            
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
                params["board"]*params["board"]+1,
                kernel_regularizer=R.l2(.0001),
                bias_regularizer=R.l2(.0001))(policy_path)
        policy_output = L.Activation('softmax')(policy_path)
        print 'policy_output', policy_output

        print '-------------value -------------------'
        
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
