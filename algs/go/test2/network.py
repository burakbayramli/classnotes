from tensorflow.contrib.keras import models as M
from tensorflow.contrib.keras import layers as L
from tensorflow.contrib.keras import regularizers as R
from util import flatten_idx, random_transform, idx_transformations
from tensorflow.contrib.keras import backend as K
from tensorflow.contrib.keras import models as M
import numpy as np

class PolicyValue:
    """uses a convolutional neural network to evaluate the state of the game
    and compute a probability distribution over the next action
    and value of the current state.
    """

    def __init__(self, feature_list, **kwargs):
        """create a neural net object that preprocesses according to feature_list and uses
        a neural network specified by keyword arguments (using subclass' create_network())

        optional argument: init_network (boolean). If set to False, skips initializing
        self.model and self.forward and the calling function should set them.
        """
        self.preprocessor = Preprocess(feature_list)
        kwargs["input_dim"] = self.preprocessor.output_dim

        if kwargs.get('init_network', True):
            # self.__class__ refers to the subclass so that subclasses only
            # need to override create_network()
            self.model = self.__class__.create_network(**kwargs)
            # self.forward is a lambda function wrapping a Keras function
            self.forward = self._model_forward()

    def _model_forward(self):
        """Construct a function using the current keras backend that, when given a batch
        of inputs, simply processes them forward and returns the output

        This is as opposed to model.compile(), which takes a loss function
        and training method.

        c.f. https://github.com/fchollet/keras/issues/1426
        """
        # The uses_learning_phase property is True if the model contains layers that behave
        # differently during training and testing, e.g. Dropout or BatchNormalization.
        # In these cases, K.learning_phase() is a reference to a backend variable that should
        # be set to 0 when using the network in prediction mode and is automatically set to 1
        # during training.
        if self.model.uses_learning_phase:
            forward_function = K.function(self.model.inputs + [K.learning_phase()], self.model.outputs)

            # the forward_function returns a list of tensors
            # the first [0] gets the front tensor.
            return lambda inpt: forward_function([inpt, 0])
        else:
            # identical but without a second input argument for the learning phase
            forward_function = K.function(self.model.inputs, self.model.outputs)
            return lambda inpt: forward_function([inpt])
    

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
        """Given a list of states, evaluates them all at once to make best use of GPU
        batching capabilities.

        Analogous to [eval_value_state(s) for s in states]

        Returns: a parallel list of values as in eval_value_state
        """
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
        transform = random_transform()
        tensor = self.preprocessor.state_to_tensor(state, transform)
        # run the tensor through the network
        network_policy, network_value = self.forward(tensor)
        return network_value[0]

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
            "filters_per_layer": 16,
            "layers": 4,
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
