
```python
import os
import itertools
import codecs
import re
import datetime
import cairocffi as cairo
import editdistance
import numpy as np
from scipy import ndimage

class TextImageGenerator(keras.callbacks.Callback):

    def __init__(self, monogram_file, bigram_file, minibatch_size,
                 img_w, img_h, downsample_factor, val_split,
                 absolute_max_string_len=16):

        self.minibatch_size = minibatch_size
        self.img_w = img_w
        self.img_h = img_h
        self.monogram_file = monogram_file
        self.bigram_file = bigram_file
        self.downsample_factor = downsample_factor
        self.val_split = val_split
        self.blank_label = self.get_output_size() - 1
        self.absolute_max_string_len = absolute_max_string_len

    def get_output_size(self):
        return len(alphabet) + 1

    # num_words can be independent of the epoch size due to the use of generators
    # as max_string_len grows, num_words can grow
    def build_word_list(self, num_words, max_string_len=None, mono_fraction=0.5):
        assert max_string_len <= self.absolute_max_string_len
        assert num_words % self.minibatch_size == 0
        assert (self.val_split * num_words) % self.minibatch_size == 0
        self.num_words = num_words
        self.string_list = [''] * self.num_words
        tmp_string_list = []
        self.max_string_len = max_string_len
        self.Y_data = np.ones([self.num_words, self.absolute_max_string_len]) * -1
        self.X_text = []
        self.Y_len = [0] * self.num_words

        # monogram file is sorted by frequency in english speech
        with codecs.open(self.monogram_file, mode='rt', encoding='utf-8') as f:
            for line in f:
                if len(tmp_string_list) == int(self.num_words * mono_fraction):
                    break
                word = line.rstrip()
                if max_string_len == -1 or max_string_len is None or len(word) <= max_string_len:
                    tmp_string_list.append(word)

        # bigram file contains common word pairings in english speech
        with codecs.open(self.bigram_file, mode='rt', encoding='utf-8') as f:
            lines = f.readlines()
            for line in lines:
                if len(tmp_string_list) == self.num_words:
                    break
                columns = line.lower().split()
                word = columns[0] + ' ' + columns[1]
                if is_valid_str(word) and \
                        (max_string_len == -1 or max_string_len is None or len(word) <= max_string_len):
                    tmp_string_list.append(word)
        if len(tmp_string_list) != self.num_words:
            raise IOError('Could not pull enough words from supplied monogram and bigram files. ')
        # interlace to mix up the easy and hard words
        self.string_list[::2] = tmp_string_list[:self.num_words // 2]
        self.string_list[1::2] = tmp_string_list[self.num_words // 2:]

        for i, word in enumerate(self.string_list):
            self.Y_len[i] = len(word)
            self.Y_data[i, 0:len(word)] = text_to_labels(word)
            self.X_text.append(word)
        self.Y_len = np.expand_dims(np.array(self.Y_len), 1)

        self.cur_val_index = self.val_split
        self.cur_train_index = 0

    # each time an image is requested from train/val/test, a new random
    # painting of the text is performed
    def get_batch(self, index, size, train):
        # width and height are backwards from typical Keras convention
        # because width is the time dimension when it gets fed into the RNN
        if K.image_data_format() == 'channels_first':
            X_data = np.ones([size, 1, self.img_w, self.img_h])
        else:
            X_data = np.ones([size, self.img_w, self.img_h, 1])

        labels = np.ones([size, self.absolute_max_string_len])
        input_length = np.zeros([size, 1])
        label_length = np.zeros([size, 1])
        source_str = []
        for i in range(size):
            # Mix in some blank inputs.  This seems to be important for
            # achieving translational invariance
            if train and i > size - 4:
                if K.image_data_format() == 'channels_first':
                    X_data[i, 0, 0:self.img_w, :] = self.paint_func('')[0, :, :].T
                else:
                    X_data[i, 0:self.img_w, :, 0] = self.paint_func('',)[0, :, :].T
                labels[i, 0] = self.blank_label
                input_length[i] = self.img_w // self.downsample_factor - 2
                label_length[i] = 1
                source_str.append('')
            else:
                if K.image_data_format() == 'channels_first':
                    X_data[i, 0, 0:self.img_w, :] = self.paint_func(self.X_text[index + i])[0, :, :].T
                else:
                    X_data[i, 0:self.img_w, :, 0] = self.paint_func(self.X_text[index + i])[0, :, :].T
                labels[i, :] = self.Y_data[index + i]
                input_length[i] = self.img_w // self.downsample_factor - 2
                label_length[i] = self.Y_len[index + i]
                source_str.append(self.X_text[index + i])
        inputs = {'the_input': X_data,
                  'the_labels': labels,
                  'input_length': input_length,
                  'label_length': label_length,
                  'source_str': source_str  # used for visualization only
                  }
        outputs = {'ctc': np.zeros([size])}  # dummy data for dummy loss function
        return (inputs, outputs)

    def next_train(self):
        while 1:
            ret = self.get_batch(self.cur_train_index, self.minibatch_size, train=True)
            self.cur_train_index += self.minibatch_size
            if self.cur_train_index >= self.val_split:
                self.cur_train_index = self.cur_train_index % 32
                (self.X_text, self.Y_data, self.Y_len) = shuffle_mats_or_lists(
                    [self.X_text, self.Y_data, self.Y_len], self.val_split)
            yield ret

    def next_val(self):
        while 1:
            ret = self.get_batch(self.cur_val_index, self.minibatch_size, train=False)
            self.cur_val_index += self.minibatch_size
            if self.cur_val_index >= self.num_words:
                self.cur_val_index = self.val_split + self.cur_val_index % 32
            yield ret

    def on_train_begin(self, logs={}):
        self.build_word_list(16000, 4, 1)
        self.paint_func = lambda text: paint_text(text, self.img_w, self.img_h,
                                                  rotate=False, ud=False, multi_fonts=False)

    def on_epoch_begin(self, epoch, logs={}):
        # rebind the paint function to implement curriculum learning
        if 3 <= epoch < 6:
            self.paint_func = lambda text: paint_text(text, self.img_w, self.img_h,
                                                      rotate=False, ud=True, multi_fonts=False)
        elif 6 <= epoch < 9:
            self.paint_func = lambda text: paint_text(text, self.img_w, self.img_h,
                                                      rotate=False, ud=True, multi_fonts=True)
        elif epoch >= 9:
            self.paint_func = lambda text: paint_text(text, self.img_w, self.img_h,
                                                      rotate=True, ud=True, multi_fonts=True)
        if epoch >= 21 and self.max_string_len < 12:
            self.build_word_list(32000, 12, 0.5)



OUTPUT_DIR = 'image_ocr'
regex = r'^[a-z ]+$'
alphabet = u'abcdefghijklmnopqrstuvwxyz '
pool_size = 2
fdir = os.path.dirname(get_file('wordlists.tgz',origin='http://www.mythic-ai.com/datasets/wordlists.tgz', untar=True))
img_gen = TextImageGenerator(monogram_file=os.path.join(fdir, 'wordlist_mono_clean.txt'),
                             bigram_file=os.path.join(fdir, 'wordlist_bi_clean.txt'),
                             minibatch_size=1,
                             img_w=128,
                             img_h=64,
                             downsample_factor=(pool_size ** 2),
                             val_split=0.2
                             )
for b in img_gen.next_train():
    print b
```
















