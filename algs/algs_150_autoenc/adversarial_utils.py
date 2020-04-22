import keras.backend as K
import numpy as np
from keras.layers import Activation, Lambda
from keras.models import Model
from six import iteritems

import tensorflow as tf
from six import iterkeys
from tensorflow.contrib.graph_editor import select
from tensorflow.contrib.graph_editor import util
from tensorflow.python.framework import ops as tf_ops

def unpack_assignment(a):
    if isinstance(a, (list, tuple)):
        assert (len(a) == 2)
        return a
    elif isinstance(a, tf.Tensor):
        assert (a.op.type in ['Assign', 'AssignAdd', 'AssignSub'])
        if a.op.type == 'Assign':
            return a.op.inputs[0], a.op.inputs[1]
        if a.op.type == 'AssignAdd':
            return a.op.inputs[0], a.op.inputs[0] + a.op.inputs[1]
        elif a.op.type == 'AssignSub':
            return a.op.inputs[0], a.op.inputs[0] - a.op.inputs[1]
        else:
            raise ValueError("Unsupported operation: {}".format(a.op.type))
    else:
        raise ValueError("Unsupported assignment object type: {}".format(type(a)))


def map_params(params):
    return [x.op.outputs[0] for x in params]


def clone_replace(f, replace):
    flatten_target_ts = util.flatten_tree(f)
    graph = util.get_unique_graph(flatten_target_ts, check_types=(tf_ops.Tensor))
    control_ios = util.ControlOutputs(graph)
    ops = select.get_walks_intersection_ops(list(iterkeys(replace)),
                                            flatten_target_ts,
                                            control_ios=control_ios)
    if not ops:
        # this happens with disconnected inputs
        return f
    else:
        return tf.contrib.graph_editor.graph_replace(f, replace)


def variable_key(a):
    if hasattr(a, "op"):
        return a.op
    else:
        return a

def build_gan(generator, discriminator, name="gan"):
    """
    Build GAN from generator and discriminator
    Model is (z, x) -> (yfake, yreal)
    :param generator: Model (z -> x)
    :param discriminator: Model (x -> y)
    :return: GAN model
    """
    yfake = Activation("linear", name="yfake")(discriminator(generator(generator.inputs)))
    yreal = Activation("linear", name="yreal")(discriminator(discriminator.inputs))
    model = Model(generator.inputs + discriminator.inputs, [yfake, yreal], name=name)
    return model


def eliminate_z(gan, latent_sampling):
    """
    Eliminate z from GAN using latent_sampling
    :param gan: model with 2 inputs: z, x
    :param latent_sampling: layer that samples z with same batch size as x
    :return: Model x -> gan(latent_sampling(x), x)
    """
    x = gan.inputs[1]
    z = latent_sampling(x)
    model = Model(x, fix_names(gan([z, x]), gan.output_names), name=gan.name)
    return model


def simple_gan(generator, discriminator, latent_sampling):
    # build basic gan
    gan = build_gan(generator, discriminator)
    # generate z on gpu, eliminate one input
    if latent_sampling is None:
        return gan
    else:
        return eliminate_z(gan, latent_sampling)


def fix_names(outputs, names):
    if not isinstance(outputs, list):
        outputs = [outputs]
    if not isinstance(names, list):
        names = [names]
    return [Activation('linear', name=name)(output) for output, name in zip(outputs, names)]


def gan_targets(n):
    """
    Standard training targets
    [generator_fake, generator_real, discriminator_fake, discriminator_real] = [1, 0, 0, 1]
    :param n: number of samples
    :return: array of targets
    """
    generator_fake = np.ones((n, 1))
    generator_real = np.zeros((n, 1))
    discriminator_fake = np.zeros((n, 1))
    discriminator_real = np.ones((n, 1))
    return [generator_fake, generator_real, discriminator_fake, discriminator_real]


def gan_targets_hinge(n):
    """
    Standard training targets for hinge loss
    [generator_fake, generator_real, discriminator_fake, discriminator_real] = [1, -1, -1, 1]
    :param n: number of samples
    :return: array of targets
    """
    generator_fake = np.ones((n, 1))
    generator_real = np.ones((n, 1)) * -1
    discriminator_fake = np.ones((n, 1)) * -1
    discriminator_real = np.ones((n, 1))
    return [generator_fake, generator_real, discriminator_fake, discriminator_real]


def normal_latent_sampling(latent_shape):
    """
    Sample from normal distribution
    :param latent_shape: batch shape
    :return: normal samples, shape=(n,)+latent_shape
    """
    return Lambda(lambda x: K.random_normal((K.shape(x)[0],) + latent_shape),
                  output_shape=lambda x: ((x[0],) + latent_shape))


def uniform_latent_sampling(latent_shape, low=0.0, high=1.0):
    """
    Sample from uniform distribution
    :param latent_shape: batch shape
    :return: normal samples, shape=(n,)+latent_shape
    """
    return Lambda(lambda x: K.random_uniform((K.shape(x)[0],) + latent_shape, low, high),
                  output_shape=lambda x: ((x[0],) + latent_shape))


def n_choice(x, n):
    return x[np.random.choice(x.shape[0], size=n, replace=False)]


def merge_updates(updates):
    """Average repeated updates of the same variable"""
    merged_updates = {}
    for update in updates:
        variable, value = unpack_assignment(update)
        key = variable_key(variable)
        if key not in merged_updates:
            merged_updates[key] = [variable, []]
        merged_updates[key][1].append(value)
    ret = []
    for k, v in iteritems(merged_updates):
        variable = v[0]
        values = v[1]
        n = len(values)
        if n == 1:
            ret.append(K.update(variable, value[0]))
        else:
            ret.append(K.update(variable, sum(values) / n))
    return ret
