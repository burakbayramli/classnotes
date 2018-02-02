from tensorflow.contrib.keras import backend as K
from tensorflow.contrib.keras import models as M
import json


class NeuralNetBase(object):
    """Base class for neural network classes handling feature processing, construction
    of a 'forward' function, etc.
    """

    # keep track of subclasses to make generic saving/loading cleaner.
    # subclasses can be 'registered' with the @neuralnet decorator
    subclasses = {}

    def __init__(self):
        """create a neural net object that preprocesses according to feature_list and uses
        a neural network specified by keyword arguments (using subclass' create_network())

        optional argument: init_network (boolean). If set to False, skips initializing
        self.model and self.forward and the calling function should set them.
        """

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

    @staticmethod
    def load_model(class_name):
        if class_name == "PolicyValue": return PolicyValue()        
    
    def save_model(self, json_file, weights_file=None):
        pass

def neuralnet(cls):
    """Class decorator for registering subclasses of NeuralNetBase
    """
    NeuralNetBase.subclasses[cls.__name__] = cls
    return cls

