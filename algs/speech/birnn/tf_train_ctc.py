#!/usr/bin/env python

import os
import numpy as np
import time
import warnings
from configparser import ConfigParser
import logging

import tensorflow as tf
from tensorflow.python.ops import ctc_ops

# Custom modules
from text import ndarray_to_text, sparse_tuple_to_texts

# in future different than utils class
from utils import create_optimizer
from datasets import read_datasets
from set_dirs import get_conf_dir, get_model_dir

# Import the setup scripts for different types of model
from rnn import BiRNN as BiRNN_model
from rnn import SimpleLSTM as SimpleLSTM_model

logger = logging.getLogger(__name__)


class Tf_train_ctc(object):
    '''
    Class to train a speech recognition model with TensorFlow.

    Requirements:
    - TensorFlow 1.0.1
    - Python 3.5
    - Configuration: $RNN_TUTORIAL/configs/neural_network.ini

    Features:
    - Batch loading of input data
    - Checkpoints model
    - Label error rate is the edit (Levenshtein) distance of the top path vs true sentence
    - Logs summary stats for TensorBoard
    - Epoch 1: Train starting with shortest transcriptions, then shuffle

    # Note: All calls to tf.name_scope or tf.summary.* support TensorBoard visualization.

    This class was initially based on open source code from Mozilla DeepSpeech:
    https://github.com/mozilla/DeepSpeech/blob/master/DeepSpeech.py

    # This Source Code Form is subject to the terms of the Mozilla Public
    # License, v. 2.0. If a copy of the MPL was not distributed with this
    # file, You can obtain one at http://mozilla.org/MPL/2.0/.
    '''

    def __init__(self,
                 config_file='neural_network.ini',
                 model_name=None,
                 debug=False):
        # set TF logging verbosity
        tf.logging.set_verbosity(tf.logging.INFO)

        # Load the configuration file depending on debug True/False
        self.conf_path = "neural_network.ini"
        self.load_configs()

        # Verify that the GPU is operational, if not use CPU
        self.tf_device = '/cpu:0'
        logging.info('Using this device for main computations: %s', self.tf_device)

        # set the directories
        self.set_up_directories(model_name)

        # set up the model
        self.set_up_model()

    def load_configs(self):
        parser = ConfigParser(os.environ)
        if not os.path.exists(self.conf_path):
            raise IOError("Configuration file '%s' does not exist" % self.conf_path)
        logging.info('Loading config from %s', self.conf_path)
        parser.read(self.conf_path)

        # set which set of configs to import
        config_header = 'nn'

        logger.info('config header: %s', config_header)

        self.epochs = parser.getint(config_header, 'epochs')
        logger.debug('self.epochs = %d', self.epochs)

        self.network_type = parser.get(config_header, 'network_type')

        # Number of mfcc features, 13 or 26
        self.n_input = parser.getint(config_header, 'n_input')

        # Number of contextual samples to include
        self.n_context = parser.getint(config_header, 'n_context')

        # self.decode_train = parser.getboolean(config_header, 'decode_train')
        # self.random_seed = parser.getint(config_header, 'random_seed')
        self.model_dir = parser.get(config_header, 'model_dir')

        # set the session name
        self.session_name = '{}_{}'.format(
            self.network_type, time.strftime("%Y%m%d-%H%M%S"))
        sess_prefix_str = 'develop'
        if len(sess_prefix_str) > 0:
            self.session_name = '{}_{}'.format(
                sess_prefix_str, self.session_name)

        # How often to save the model
        self.SAVE_MODEL_EPOCH_NUM = parser.getint(
            config_header, 'SAVE_MODEL_EPOCH_NUM')

        # decode dev set after N epochs
        self.VALIDATION_EPOCH_NUM = parser.getint(
            config_header, 'VALIDATION_EPOCH_NUM')

        # decide when to stop training prematurely
        self.CURR_VALIDATION_LER_DIFF = parser.getfloat(
            config_header, 'CURR_VALIDATION_LER_DIFF')

        self.AVG_VALIDATION_LER_EPOCHS = parser.getint(
            config_header, 'AVG_VALIDATION_LER_EPOCHS')
        # initialize list to hold average validation at end of each epoch
        self.AVG_VALIDATION_LERS = [
            1.0 for _ in range(self.AVG_VALIDATION_LER_EPOCHS)]

        # setup type of decoder
        self.beam_search_decoder = parser.get(
            config_header, 'beam_search_decoder')

        # determine if the data input order should be shuffled after every epic
        self.shuffle_data_after_epoch = parser.getboolean(
            config_header, 'shuffle_data_after_epoch')

        # initialize to store the minimum validation set label error rate
        self.min_dev_ler = parser.getfloat(config_header, 'min_dev_ler')

        # set up GPU if available
        self.tf_device = str(parser.get(config_header, 'tf_device'))

        # set up the max amount of simultaneous users
        # this restricts GPU usage to the inverse of self.simultaneous_users_count
        self.simultaneous_users_count = parser.getint(config_header, 'simultaneous_users_count')

    def set_up_directories(self, model_name):
        # Set up model directory
        self.model_dir = os.path.join(get_model_dir(), self.model_dir)
        # summary will contain logs
        self.SUMMARY_DIR = os.path.join(
            self.model_dir, "summary", self.session_name)
        # session will contain models
        self.SESSION_DIR = os.path.join(
            self.model_dir, "session", self.session_name)

        if not os.path.exists(self.SESSION_DIR):
            os.makedirs(self.SESSION_DIR)
        if not os.path.exists(self.SUMMARY_DIR):
            os.makedirs(self.SUMMARY_DIR)

        # set the model name and restore if not None
        if model_name is not None:
            self.model_path = os.path.join(self.SESSION_DIR, model_name)
        else:
            self.model_path = None

    def set_up_model(self):
        self.sets = ['train', 'dev', 'test']

        # read data set, inherits configuration path
        # to parse the config file for where data lives
        self.data_sets = read_datasets(self.conf_path,
                                       self.sets,
                                       self.n_input,
                                       self.n_context
                                       )

        self.n_examples_train = len(self.data_sets.train._txt_files)
        self.n_examples_dev = len(self.data_sets.dev._txt_files)
        self.n_examples_test = len(self.data_sets.test._txt_files)
        self.batch_size = self.data_sets.train._batch_size
        self.n_batches_per_epoch = int(np.ceil(
            self.n_examples_train / self.batch_size))

        logger.info('''Training model: {}
        Train examples: {:,}
        Dev examples: {:,}
        Test examples: {:,}
        Epochs: {}
        Training batch size: {}
        Batches per epoch: {}'''.format(
            self.session_name,
            self.n_examples_train,
            self.n_examples_dev,
            self.n_examples_test,
            self.epochs,
            self.batch_size,
            self.n_batches_per_epoch))

    def run_model(self):
        self.graph = tf.Graph()
        with self.graph.as_default(), tf.device('/cpu:0'):

            with tf.device(self.tf_device):
                # Run multiple functions on the specificed tf_device
                # tf_device GPU set in configs, but is overridden if not available
                self.setup_network_and_graph()
                self.load_placeholder_into_network()
                self.setup_loss_function()
                self.setup_optimizer()
                self.setup_decoder()

            self.setup_summary_statistics()

            # create the configuration for the session
            tf_config = tf.ConfigProto()
            tf_config.allow_soft_placement = True
            tf_config.gpu_options.per_process_gpu_memory_fraction = \
                (1.0 / self.simultaneous_users_count)

            # create the session
            self.sess = tf.Session(config=tf_config)

            # initialize the summary writer
            self.writer = tf.summary.FileWriter(
                self.SUMMARY_DIR, graph=self.sess.graph)

            # Add ops to save and restore all the variables
            self.saver = tf.train.Saver()

            # For printing out section headers
            section = '\n{0:=^40}\n'

            # If there is a model_path declared, then restore the model
            if self.model_path is not None:
                self.saver.restore(self.sess, self.model_path)
            # If there is NOT a model_path declared, build the model from scratch
            else:
                # Op to initialize the variables
                init_op = tf.global_variables_initializer()

                # Initializate the weights and biases
                self.sess.run(init_op)

                # MAIN LOGIC for running the training epochs
                logger.info(section.format('Run training epoch'))
                self.run_training_epochs()

            logger.info(section.format('Decoding test data'))
            # make the assumption for working on the test data, that the epoch here is the last epoch
            _, self.test_ler = self.run_batches(self.data_sets.test, is_training=False,
                                                decode=True, write_to_file=False, epoch=self.epochs)

            # Add the final test data to the summary writer
            # (single point on the graph for end of training run)
            summary_line = self.sess.run(
                self.test_ler_op, {self.ler_placeholder: self.test_ler})
            self.writer.add_summary(summary_line, self.epochs)

            logger.info('Test Label Error Rate: {}'.format(self.test_ler))

            # save train summaries to disk
            self.writer.flush()

            self.sess.close()

    def setup_network_and_graph(self):
        # e.g: log filter bank or MFCC features
        # shape = [batch_size, max_stepsize, n_input + (2 * n_input * n_context)]
        # the batch_size and max_stepsize can vary along each step
        self.input_tensor = tf.placeholder(
            tf.float32, [None, None, self.n_input + (2 * self.n_input * self.n_context)], name='input')

        # Use sparse_placeholder; will generate a SparseTensor, required by ctc_loss op.
        self.targets = tf.sparse_placeholder(tf.int32, name='targets')
        # 1d array of size [batch_size]
        self.seq_length = tf.placeholder(tf.int32, [None], name='seq_length')

    def load_placeholder_into_network(self):
        # logits is the non-normalized output/activations from the last layer.
        # logits will be input for the loss function.
        # nn_model is from the import statement in the load_model function
        # summary_op variables are for tensorboard
        if self.network_type == 'SimpleLSTM':
            self.logits, summary_op = SimpleLSTM_model(
                self.conf_path,
                self.input_tensor,
                tf.to_int64(self.seq_length)
            )
        elif self.network_type == 'BiRNN':
            self.logits, summary_op = BiRNN_model(
                self.conf_path,
                self.input_tensor,
                tf.to_int64(self.seq_length),
                self.n_input,
                self.n_context
            )
        else:
            raise ValueError('network_type must be SimpleLSTM or BiRNN')
        self.summary_op = tf.summary.merge([summary_op])

    def setup_loss_function(self):
        with tf.name_scope("loss"):
            self.total_loss = ctc_ops.ctc_loss(
                self.targets, self.logits, self.seq_length)
            self.avg_loss = tf.reduce_mean(self.total_loss)
            self.loss_summary = tf.summary.scalar("avg_loss", self.avg_loss)

            self.cost_placeholder = tf.placeholder(dtype=tf.float32, shape=[])

            self.train_cost_op = tf.summary.scalar(
                "train_avg_loss", self.cost_placeholder)

    def setup_optimizer(self):
        # Note: The optimizer is created in models/RNN/utils.py
        with tf.name_scope("train"):
            self.optimizer = create_optimizer()
            self.optimizer = self.optimizer.minimize(self.avg_loss)

    def setup_decoder(self):
        with tf.name_scope("decode"):
            if self.beam_search_decoder == 'default':
                self.decoded, self.log_prob = ctc_ops.ctc_beam_search_decoder(
                    self.logits, self.seq_length, merge_repeated=False)
            elif self.beam_search_decoder == 'greedy':
                self.decoded, self.log_prob = ctc_ops.ctc_greedy_decoder(
                    self.logits, self.seq_length, merge_repeated=False)
            else:
                logging.warning("Invalid beam search decoder option selected!")

    def setup_summary_statistics(self):
        # Create a placholder for the summary statistics
        with tf.name_scope("accuracy"):
            # Compute the edit (Levenshtein) distance of the top path
            distance = tf.edit_distance(
                tf.cast(self.decoded[0], tf.int32), self.targets)

            # Compute the label error rate (accuracy)
            self.ler = tf.reduce_mean(distance, name='label_error_rate')
            self.ler_placeholder = tf.placeholder(dtype=tf.float32, shape=[])
            self.train_ler_op = tf.summary.scalar(
                "train_label_error_rate", self.ler_placeholder)
            self.dev_ler_op = tf.summary.scalar(
                "validation_label_error_rate", self.ler_placeholder)
            self.test_ler_op = tf.summary.scalar(
                "test_label_error_rate", self.ler_placeholder)

    def run_training_epochs(self):
        train_start = time.time()
        for epoch in range(self.epochs):
            # Initialize variables that can be updated
            save_dev_model = False
            stop_training = False
            is_checkpoint_step, is_validation_step = \
                self.validation_and_checkpoint_check(epoch)

            epoch_start = time.time()

            self.train_cost, self.train_ler = self.run_batches(
                self.data_sets.train,
                is_training=True,
                decode=False,
                write_to_file=False,
                epoch=epoch)

            epoch_duration = time.time() - epoch_start

            log = 'Epoch {}/{}, train_cost: {:.3f}, \
                   train_ler: {:.3f}, time: {:.2f} sec'
            logger.info(log.format(
                epoch + 1,
                self.epochs,
                self.train_cost,
                self.train_ler,
                epoch_duration))

            summary_line = self.sess.run(
                self.train_ler_op, {self.ler_placeholder: self.train_ler})
            self.writer.add_summary(summary_line, epoch)

            summary_line = self.sess.run(
                self.train_cost_op, {self.cost_placeholder: self.train_cost})
            self.writer.add_summary(summary_line, epoch)

            # Shuffle the data for the next epoch
            if self.shuffle_data_after_epoch:
                np.random.shuffle(self.data_sets.train._txt_files)

            # Run validation if it was determined to run a validation step
            if is_validation_step:
                self.run_validation_step(epoch)

            if (epoch + 1) == self.epochs or is_checkpoint_step:
                # save the final model
                save_path = self.saver.save(self.sess, os.path.join(
                    self.SESSION_DIR, 'model.ckpt'), epoch)
                logger.info("Model saved: {}".format(save_path))

            if save_dev_model:
                # If the dev set is not improving,
                # the training is killed to prevent overfitting
                # And then save the best validation performance model
                save_path = self.saver.save(self.sess, os.path.join(
                    self.SESSION_DIR, 'model-best.ckpt'))
                logger.info(
                    "Model with best validation label error rate saved: {}".
                    format(save_path))

            if stop_training:
                break

        train_duration = time.time() - train_start
        logger.info('Training complete, total duration: {:.2f} min'.format(
            train_duration / 60))

    def run_validation_step(self, epoch):
        dev_ler = 0

        _, dev_ler = self.run_batches(self.data_sets.dev,
                                      is_training=False,
                                      decode=True,
                                      write_to_file=False,
                                      epoch=epoch)

        logger.info('Validation Label Error Rate: {}'.format(dev_ler))

        summary_line = self.sess.run(
            self.dev_ler_op, {self.ler_placeholder: dev_ler})
        self.writer.add_summary(summary_line, epoch)

        if dev_ler < self.min_dev_ler:
            self.min_dev_ler = dev_ler

        # average historical LER
        history_avg_ler = np.mean(self.AVG_VALIDATION_LERS)

        # if this LER is not better than average of previous epochs, exit
        if history_avg_ler - dev_ler <= self.CURR_VALIDATION_LER_DIFF:
            log = "Validation label error rate not improved by more than {:.2%} \
                  after {} epochs. Exit"
            warnings.warn(log.format(self.CURR_VALIDATION_LER_DIFF,
                                     self.AVG_VALIDATION_LER_EPOCHS))

        # save avg validation accuracy in the next slot
        self.AVG_VALIDATION_LERS[
            epoch % self.AVG_VALIDATION_LER_EPOCHS] = dev_ler

    def validation_and_checkpoint_check(self, epoch):
        # initially set at False unless indicated to change
        is_checkpoint_step = False
        is_validation_step = False

        # Check if the current epoch is a validation or checkpoint step
        if (epoch > 0) and ((epoch + 1) != self.epochs):
            if (epoch + 1) % self.SAVE_MODEL_EPOCH_NUM == 0:
                is_checkpoint_step = True
            if (epoch + 1) % self.VALIDATION_EPOCH_NUM == 0:
                is_validation_step = True

        return is_checkpoint_step, is_validation_step

    def run_batches(self, dataset, is_training, decode, write_to_file, epoch):
        n_examples = len(dataset._txt_files)

        n_batches_per_epoch = int(np.ceil(n_examples / dataset._batch_size))

        self.train_cost = 0
        self.train_ler = 0

        for batch in range(n_batches_per_epoch):
            # Get next batch of training data (audio features) and transcripts
            source, source_lengths, sparse_labels = dataset.next_batch()

            feed = {self.input_tensor: source,
                    self.targets: sparse_labels,
                    self.seq_length: source_lengths}

            # If the is_training is false, this means straight decoding without computing loss
            if is_training:
                # avg_loss is the loss_op, optimizer is the train_op;
                # running these pushes tensors (data) through graph
                batch_cost, _ = self.sess.run(
                    [self.avg_loss, self.optimizer], feed)
                self.train_cost += batch_cost * dataset._batch_size
                logger.debug('Batch cost: %.2f | Train cost: %.2f',
                             batch_cost, self.train_cost)

            self.train_ler += self.sess.run(self.ler, feed_dict=feed) * dataset._batch_size
            logger.debug('Label error rate: %.2f', self.train_ler)

            # Turn on decode only 1 batch per epoch
            if decode and batch == 0:
                d = self.sess.run(self.decoded[0], feed_dict={
                    self.input_tensor: source,
                    self.targets: sparse_labels,
                    self.seq_length: source_lengths}
                )
                dense_decoded = tf.sparse_tensor_to_dense(
                    d, default_value=-1).eval(session=self.sess)
                dense_labels = sparse_tuple_to_texts(sparse_labels)

                # only print a set number of example translations
                counter = 0
                counter_max = 4
                if counter < counter_max:
                    for orig, decoded_arr in zip(dense_labels, dense_decoded):
                        # convert to strings
                        decoded_str = ndarray_to_text(decoded_arr)
                        logger.info('Batch {}, file {}'.format(batch, counter))
                        logger.info('Original: {}'.format(orig))
                        logger.info('Decoded:  {}'.format(decoded_str))
                        counter += 1

                # save out variables for testing
                self.dense_decoded = dense_decoded
                self.dense_labels = dense_labels

        # Metrics mean
        if is_training:
            self.train_cost /= n_examples
        self.train_ler /= n_examples

        # Populate summary for histograms and distributions in tensorboard
        self.accuracy, summary_line = self.sess.run(
            [self.avg_loss, self.summary_op], feed)
        self.writer.add_summary(summary_line, epoch)

        return self.train_cost, self.train_ler


# to run in console
if __name__ == '__main__':
    import click

    # Use click to parse command line arguments
    @click.command()
    @click.option('--config', default='neural_network.ini', help='Configuration file name')
    @click.option('--name', default=None, help='Model name for logging')
    @click.option('--debug', type=bool, default=False,
                  help='Use debug settings in config file')
    # Train RNN model using a given configuration file
    def main(config='neural_network.ini', name=None, debug=False):
        logging.basicConfig(level=logging.DEBUG,
                            format='%(asctime)s [%(levelname)s] %(name)s: %(message)s')
        global logger
        logger = logging.getLogger(os.path.basename(__file__))

        # create the Tf_train_ctc class
        tf_train_ctc = Tf_train_ctc(
            config_file=config, model_name=name, debug=debug)

        # run the training
        tf_train_ctc.run_model()

    main()
