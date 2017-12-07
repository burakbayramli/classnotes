import os
import tensorflow as tf

from configparser import ConfigParser
from set_dirs import get_conf_dir

conf_dir = "."
parser = ConfigParser(os.environ)
parser.read(os.path.join(conf_dir, 'neural_network.ini'))

# AdamOptimizer
beta1 = parser.getfloat('optimizer', 'beta1')
beta2 = parser.getfloat('optimizer', 'beta2')
epsilon = parser.getfloat('optimizer', 'epsilon')
learning_rate = parser.getfloat('optimizer', 'learning_rate')


def variable_on_cpu(name, shape, initializer):
    """
    Next we concern ourselves with graph creation.
    However, before we do so we must introduce a utility function ``variable_on_cpu()``
    used to create a variable in CPU memory.
    """
    # Use the /cpu:0 device for scoped operations
    with tf.device('/cpu:0'):
        # Create or get apropos variable
        var = tf.get_variable(name=name, shape=shape, initializer=initializer)
    return var


def create_optimizer():
    optimizer = tf.train.AdamOptimizer(learning_rate=learning_rate,
                                       beta1=beta1,
                                       beta2=beta2,
                                       epsilon=epsilon)
    return optimizer
