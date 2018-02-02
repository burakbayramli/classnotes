from tensorflow.contrib.keras import models as M
from tensorflow.contrib.keras import layers as L
from tensorflow.contrib.keras import regularizers as R
from util import flatten_idx, random_transform, idx_transformations
from nn_util import NeuralNetBase, neuralnet
import numpy as np
import random


@neuralnet
class PolicyValue(NeuralNetBase):
    """uses a convolutional neural network to evaluate the state of the game
    and compute a probability distribution over the next action
    and value of the current state.
    """

    def _select_moves_and_normalize(self, nn_output, moves, size, transform="noop"):
        """helper function to normalize a distribution over the given list of moves
        and return a list of (move, prob) tuples
        """
        if len(moves) == 0:
            return []
        move_indices = [flatten_idx(idx_transformations(m, size, transform), size) for m in moves]
        # get network activations at legal move locations
        distribution = nn_output[move_indices]
        distribution = distribution / distribution.sum()
        return zip(moves, distribution)

    def batch_eval_policy_state(self, states, moves_lists=None):
        """Given a list of states, evaluates them all at once to make best use of GPU
        batching capabilities.

        Analogous to [eval_policy_state(s) for s in states]

        Returns: a parallel list of move distributions as in eval_policy_state
        """

        #print 'batch_eval_policy_state', states
        exit()
        n_states = len(states)
        if n_states == 0:
            return []
        state_size = states[0].size
        if not all([st.size == state_size for st in states]):
            raise ValueError("all states must have the same size")
        # concatenate together all one-hot encoded states along the 'batch' dimension
        nn_input = np.concatenate([self.preprocessor.state_to_tensor(s) for s in states], axis=0)
        # pass all input through the network at once (backend makes use of
        # batches if len(states) is large)
        network_policy, network_value = self.forward(nn_input)
        # default move lists to all legal moves
        moves_lists = moves_lists or [st.get_legal_moves() for st in states]
        results = [None] * n_states
        for i in range(n_states):
            results[i] = self._select_moves_and_normalize(network_policy[i], moves_lists[i],
                                                          state_size)
        return results

    def batch_eval_value_state(self, states):
        """Given a list of states, evaluates them all at once to make best use
        of GPU batching capabilities.

        Analogous to [eval_value_state(s) for s in states]

        Returns: a parallel list of values as in eval_value_state

        """

        #print 'batch_eval_value_state', states
        return np.array([random.random() for x in states])
        
        n_states = len(states)
        if n_states == 0:
            return []
        state_size = states[0].size
        if not all([st.size == state_size for st in states]):
            raise ValueError("all states must have the same size")
        # concatenate together all one-hot encoded states along the 'batch' dimension
        nn_input = np.concatenate([self.preprocessor.state_to_tensor(s) for s in states], axis=0)
        # pass all input through the network at once (backend makes use of
        # batches if len(states) is large)
        network_policy, network_value = self.forward(nn_input)
        return network_value


    def eval_policy_state(self, state, moves=None):
        #print 'eval_policy_state', state
        return [(action, random.random()) for action in state.get_legal_moves()]
        #exit()
        """Given a GameState object, returns a list of (action, probability) pairs
        according to the network outputs

        If a list of moves is specified, only those moves are kept in the distribution
        """
        transform = random_transform()
        tensor = self.preprocessor.state_to_tensor(state, transform)
        # run the tensor through the network
        network_policy, network_value = self.forward(tensor)
        moves = moves or state.get_legal_moves()
        return self._select_moves_and_normalize(network_policy[0], moves, state.size, transform)

    def eval_value_state(self, state):
        """Given a GameState object, returns value
        according to the network outputs
        """
        #transform = random_transform()
        #tensor = self.preprocessor.state_to_tensor(state, transform)
        # run the tensor through the network
        #network_policy, network_value = self.forward(tensor)
        #return network_value[0]
        return random.random()

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
            "board": 19,
            "filters_per_layer": 256,
            "layers": 19,
            "filter_width": 3
        }
        # copy defaults, but override with anything in kwargs
        params = defaults
        params.update(kwargs)

        # create the network using Keras' functional API,
        model_input = L.Input(shape=(params["input_dim"], params["board"], params["board"]))

        # create first layer
        convolution_path = L.Convolution2D(
            input_shape=(),
            filters=params["filters_per_layer"],
            kernel_size=params["filter_width"],
            activation='linear',
            padding='same',
            kernel_regularizer=R.l2(.0001),
            bias_regularizer=R.l2(.0001))(model_input)

        convolution_path = L.BatchNormalization(
                beta_regularizer=R.l2(.0001),
                gamma_regularizer=R.l2(.0001))(convolution_path)

        convolution_path = L.Activation('relu')(convolution_path)

        def add_resnet_unit(path, **params):
            """Add a resnet unit to path
            Returns new path
            """

            block_input = path
            # add Conv2D
            path = L.Convolution2D(
                filters=params["filters_per_layer"],
                kernel_size=params["filter_width"],
                activation='linear',
                padding='same',
                kernel_regularizer=R.l2(.0001),
                bias_regularizer=R.l2(.0001))(path)
            # add BatchNorm
            path = L.BatchNormalization(
                    beta_regularizer=R.l2(.0001),
                    gamma_regularizer=R.l2(.0001))(path)
            # add ReLU
            path = L.Activation('relu')(path)
            # add Conv2D
            path = L.Convolution2D(
                filters=params["filters_per_layer"],
                kernel_size=params["filter_width"],
                activation='linear',
                padding='same',
                kernel_regularizer=R.l2(.0001),
                bias_regularizer=R.l2(.0001))(path)
            # add BatchNorm
            path = L.BatchNormalization(
                    beta_regularizer=R.l2(.0001),
                    gamma_regularizer=R.l2(.0001))(path)
            # Merge 'input layer' with the path
            path = L.Add()([block_input, path])
            # add ReLU
            path = L.Activation('relu')(path)
            return path

        # create all other layers
        for _ in range(params['layers']):
            convolution_path = add_resnet_unit(convolution_path, **params)

        # policy head
        policy_path = L.Convolution2D(
            input_shape=(),
            filters=2,
            kernel_size=1,
            activation='linear',
            padding='same',
            kernel_regularizer=R.l2(.0001),
            bias_regularizer=R.l2(.0001))(convolution_path)
        policy_path = L.BatchNormalization(
                beta_regularizer=R.l2(.0001),
                gamma_regularizer=R.l2(.0001))(policy_path)
        policy_path = L.Activation('relu')(policy_path)
        policy_path = L.Flatten()(policy_path)
        policy_path = L.Dense(
                params["board"]*params["board"]+1,
                kernel_regularizer=R.l2(.0001),
                bias_regularizer=R.l2(.0001))(policy_path)
        policy_output = L.Activation('softmax')(policy_path)

        # value head
        value_path = L.Convolution2D(
            input_shape=(),
            filters=1,
            kernel_size=1,
            activation='linear',
            padding='same',
            kernel_regularizer=R.l2(.0001),
            bias_regularizer=R.l2(.0001))(convolution_path)
        value_path = L.BatchNormalization(
                beta_regularizer=R.l2(.0001),
                gamma_regularizer=R.l2(.0001))(value_path)
        value_path = L.Activation('relu')(value_path)
        value_path = L.Flatten()(value_path)
        value_path = L.Dense(
                256,
                kernel_regularizer=R.l2(.0001),
                bias_regularizer=R.l2(.0001))(value_path)
        value_path = L.Activation('relu')(value_path)
        value_path = L.Dense(
                1,
                kernel_regularizer=R.l2(.0001),
                bias_regularizer=R.l2(.0001))(value_path)
        value_output = L.Activation('tanh')(value_path)

        return M.Model(inputs=[model_input], outputs=[policy_output, value_output])
