import os
from math import ceil
from random import random
from glob import glob
from configparser import ConfigParser
import logging
from collections import namedtuple

from load_audio_to_mem import get_audio_and_transcript, pad_sequences
from text import sparse_tuple_from
from set_dirs import get_data_dir

DataSets = namedtuple('DataSets', 'train dev test')


def read_datasets(conf_path, sets, numcep, numcontext,
                  thread_count=8):
    '''Main function to create DataSet objects.

    This function calls an internal function _get_data_set_dict that
    reads the configuration file. Then it calls the internal function _read_data_set
    which collects the text files in the data directories, returning a DataSet object.
    This function returns a DataSets object containing the requested datasets.

    Args:
        sets (list):        List of datasets to create. Options are: 'train', 'dev', 'test'
        numcep (int):       Number of mel-frequency cepstral coefficients to compute.
        numcontext (int):   For each time point, number of contextual samples to include.
        thread_count (int): Number of threads

    Returns:
        DataSets:   A single `DataSets` instance containing each of the requested datasets

        E.g., when sets=['train'], datasets.train exists, with methods to retrieve examples.

    This function has been modified from Mozilla DeepSpeech:
    https://github.com/mozilla/DeepSpeech/blob/master/util/importers/librivox.py

    # This Source Code Form is subject to the terms of the Mozilla Public
    # License, v. 2.0. If a copy of the MPL was not distributed with this
    # file, You can obtain one at http://mozilla.org/MPL/2.0/.
    '''
    data_dir, dataset_config = _get_data_set_dict(conf_path, sets)

    def _read_data_set(config):
        path = os.path.join(data_dir, config['dir_pattern'])
        return DataSet.from_directory(path,
                                      thread_count=thread_count,
                                      batch_size=config['batch_size'],
                                      numcep=numcep,
                                      numcontext=numcontext,
                                      start_idx=config['start_idx'],
                                      limit=config['limit'],
                                      sort=config['sort']
                                      )
    datasets = {name: _read_data_set(dataset_config[name])
                      if name in sets else None
                for name in ('train', 'dev', 'test')}
    return DataSets(**datasets)


class DataSet:
    '''
    Train/test/dev dataset API for loading via threads and delivering batches.

    This class has been modified from Mozilla DeepSpeech:
    https://github.com/mozilla/DeepSpeech/blob/master/util/importers/librivox.py

    # This Source Code Form is subject to the terms of the Mozilla Public
    # License, v. 2.0. If a copy of the MPL was not distributed with this
    # file, You can obtain one at http://mozilla.org/MPL/2.0/.
    '''

    def __init__(self, txt_files, thread_count, batch_size, numcep, numcontext):
        self._coord = None
        self._numcep = numcep
        self._txt_files = txt_files
        self._batch_size = batch_size
        self._numcontext = numcontext
        self._start_idx = 0

    @classmethod
    def from_directory(cls, dirpath, thread_count, batch_size, numcep, numcontext, start_idx=0, limit=0, sort=None):
        if not os.path.exists(dirpath):
            raise IOError("'%s' does not exist" % dirpath)
        txt_files = txt_filenames(dirpath, start_idx=start_idx, limit=limit, sort=sort)
        if len(txt_files) == 0:
            raise RuntimeError('start_idx=%d and limit=%d arguments result in zero files' % (start_idx, limit))
        return cls(txt_files, thread_count, batch_size, numcep, numcontext)

    def next_batch(self, batch_size=None):
        if batch_size is None:
            batch_size = self._batch_size

        end_idx = min(len(self._txt_files), self._start_idx + batch_size)
        idx_list = range(self._start_idx, end_idx)
        txt_files = [self._txt_files[i] for i in idx_list]
        wav_files = [x.replace('.txt', '.wav') for x in txt_files]
        (source, _, target, _) = get_audio_and_transcript(txt_files,
                                                          wav_files,
                                                          self._numcep,
                                                          self._numcontext)
        self._start_idx += batch_size
        # Verify that the start_idx is not larger than total available sample size
        if self._start_idx >= self.size:
            self._start_idx = 0

        # Pad input to max_time_step of this batch
        source, source_lengths = pad_sequences(source)
        sparse_labels = sparse_tuple_from(target)
        return source, source_lengths, sparse_labels

    @property
    def files(self):
        return self._txt_files

    @property
    def size(self):
        return len(self.files)

    @property
    def total_batches(self):
        # Note: If len(_txt_files) % _batch_size != 0, this re-uses initial _txt_files
        return int(ceil(float(len(self._txt_files)) / float(self._batch_size)))
# END DataSet

SORTS = ['filesize_low_high', 'filesize_high_low', 'alpha', 'random']

def txt_filenames(dataset_path, start_idx=0, limit=None, sort='alpha'):
        # Obtain list of txt files
        txt_files = glob(os.path.join(dataset_path, "*.txt"))
        limit = limit or len(txt_files)

        # Optional: sort files to improve padding performance
        if sort not in SORTS:
            raise ValueError('sort must be one of [%s]', SORTS)
        reverse = False
        key = None
        if 'filesize' in sort:
            key = os.path.getsize
        if sort == 'filesize_high_low':
            reverse = True
        elif sort == 'random':
            key = lambda *args: random()
        txt_files = sorted(txt_files, key=key, reverse=reverse)

        return txt_files[start_idx:limit + start_idx]


def _get_data_set_dict(conf_path, sets):
    parser = ConfigParser(os.environ)
    parser.read(conf_path)
    config_header = 'data'
    data_dir = get_data_dir(parser.get(config_header, 'data_dir'))
    data_dict = {}

    if 'train' in sets:
        d = {}
        d['dir_pattern'] = parser.get(config_header, 'dir_pattern_train')
        d['limit'] = parser.getint(config_header, 'n_train_limit')
        d['sort'] = parser.get(config_header, 'sort_train')
        d['batch_size'] = parser.getint(config_header, 'batch_size_train')
        d['start_idx'] = parser.getint(config_header, 'start_idx_init_train')
        data_dict['train'] = d
        logging.debug('Training configuration: %s', str(d))

    if 'dev' in sets:
        d = {}
        d['dir_pattern'] = parser.get(config_header, 'dir_pattern_dev')
        d['limit'] = parser.getint(config_header, 'n_dev_limit')
        d['sort'] = parser.get(config_header, 'sort_dev')
        d['batch_size'] = parser.getint(config_header, 'batch_size_dev')
        d['start_idx'] = parser.getint(config_header, 'start_idx_init_dev')
        data_dict['dev'] = d
        logging.debug('Dev configuration: %s', str(d))

    if 'test' in sets:
        d = {}
        d['dir_pattern'] = parser.get(config_header, 'dir_pattern_test')
        d['limit'] = parser.getint(config_header, 'n_test_limit')
        d['sort'] = parser.get(config_header, 'sort_test')
        d['batch_size'] = parser.getint(config_header, 'batch_size_test')
        d['start_idx'] = parser.getint(config_header, 'start_idx_init_test')
        data_dict['test'] = d
        logging.debug('Test configuration: %s', str(d))

    return data_dir, data_dict
