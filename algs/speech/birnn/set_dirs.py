#!/usr/bin/env python3

import os


def get_relevant_directories(
        home_dir=None,
        data_dir=None,
        conf_dir=None,
        debug=False):

    home_dir = get_home_dir(home_dir=home_dir)

    data_dir = get_data_dir(data_dir=data_dir, home_dir=home_dir)

    conf_dir = get_conf_dir(conf_dir=conf_dir, home_dir=home_dir, debug=debug)

    return home_dir, data_dir, conf_dir


def get_home_dir(home_dir=None):
    if home_dir is None:
        home_dir = "/home/burak/Documents/general/RNN-Tutorial-master/"
    return home_dir


def get_data_dir(data_dir=None, home_dir=None):
    if data_dir is None:
        data_dir = os.path.join(get_home_dir(home_dir=home_dir), 'data', 'raw')
    # if the beginning of the data_dir is not '/' then prepend home_dir behind it
    elif not os.path.isabs(data_dir):
        data_dir = os.path.join(get_home_dir(home_dir=home_dir), data_dir)
    return data_dir


def get_conf_dir(conf_dir=None, home_dir=None, debug=False):
    if conf_dir is None:
        conf_dir = os.path.join(get_home_dir(home_dir=home_dir), 'configs')
        # Descend to the testing folder if debug==True
        if debug:
            conf_dir = os.path.join(conf_dir, 'testing')
    return conf_dir


def get_model_dir(model_dir=None, home_dir=None):
    if model_dir is None:
        model_dir = os.path.join(get_home_dir(home_dir=home_dir), 'models')
    return model_dir
