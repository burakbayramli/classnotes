'''
This is a Morgan-customized version of Keras' image_ocr generalised to
handle real-world data fields, especially long numbers.


The original description, which is outdated in some senses, follows:

This example uses a convolutional stack followed by a recurrent stack
and a CTC logloss function to perform optical character recognition
of generated text images. I have no evidence of whether it actually
learns general shapes of text, or just is able to recognize all
the different fonts thrown at it...the purpose is more to demonstrate CTC
inside of Keras.  Note that the font list may need to be updated
for the particular OS in use.

This starts off with 4 letter words.  For the first 12 epochs, the
difficulty is gradually increased using the TextImageGenerator class
which is both a generator class for test/train data and a Keras
callback class. After 20 epochs, longer sequences are thrown at it
by recompiling the model to handle a wider image and rebuilding
the word list to include two words separated by a space.

This requires cairo and editdistance packages:
pip install cairocffi
pip install editdistance

Originally created by Mike Henry
https://github.com/mbhenry/
'''
import itertools
import os
import re
from multiprocessing import Pool

import cairocffi as cairo
import editdistance
import keras.callbacks
import numpy as np
import unicodedata
from keras import backend as K
from keras.callbacks import ModelCheckpoint, CSVLogger
from keras.layers import Input, Dense, Activation
from keras.layers import Reshape, Lambda, merge, RepeatVector
from keras.layers.convolutional import Convolution2D, MaxPooling2D
from keras.layers.recurrent import GRU
from keras.layers.wrappers import Bidirectional
from keras.models import Model
from keras.preprocessing import image
from scipy import ndimage

from synthset import size_plainpad
from synthset import CropImageIterator

OUTPUT_DIR = 'image_ocr'

ltypes = ['account_num', 'amount_due', 'amount_paid', 'amount_rounding', 'amount_total', 'amount', 'bank_num',
          'bic', 'const_sym', 'customer_id', 'date_due', 'date_issue', 'date_uzp', 'iban', 'invoice_id', 'order_id',
          'phone_num', 'recipient_dic', 'recipient_ic', 'sender_dic', 'sender_ic', 'spec_sym', 'var_sym']

def deaccent(unistr):
    # http://www.abclinuxu.cz/clanky/programovani/standardni-knihovna-pro-python-7-retezce-3
    return "".join(aChar
                   for aChar in unicodedata.normalize("NFD", unistr)
                   if not unicodedata.combining(aChar))

# this creates larger "blotches" of noise which look
# more realistic than just adding gaussian noise
# assumes greyscale with pixels ranging from 0 to 1

def speckle(img):
    severity = np.random.uniform(0, 0.6)
    blur = ndimage.gaussian_filter(np.random.randn(*img.shape) * severity, 1)
    img_speck = (img + blur)
    img_speck[img_speck > 1] = 1
    img_speck[img_speck <= 0] = 0
    return img_speck


# Redundant instance of paint_text only kept for generation of empty samples within the module.

def paint_text(text, w, h, rotate=False, ud=False, multi_fonts=False):
    surface = cairo.ImageSurface(cairo.FORMAT_RGB24, w, h)
    with cairo.Context(surface) as context:
        context.set_source_rgb(1, 1, 1)  # White
        context.paint()
        # this font list works in Centos 7
        if multi_fonts:
            fonts = ['Century Schoolbook', 'Courier', 'STIX', 'URW Chancery L', 'FreeMono']
            context.select_font_face(np.random.choice(fonts), cairo.FONT_SLANT_NORMAL,
                                     np.random.choice([cairo.FONT_WEIGHT_BOLD, cairo.FONT_WEIGHT_NORMAL]))
        else:
            context.select_font_face('Courier', cairo.FONT_SLANT_NORMAL, cairo.FONT_WEIGHT_BOLD)
        context.set_font_size(25)
        box = context.text_extents(text)
        border_w_h = (4, 4)
        if box[2] > (w - 2 * border_w_h[1]) or box[3] > (h - 2 * border_w_h[0]):
            raise IOError('Could not fit string into image. Max char count is too large for given image width.')

        # teach the RNN translational invariance by
        # fitting text box randomly on canvas, with some room to rotate
        max_shift_x = w - box[2] - border_w_h[0]
        max_shift_y = h - box[3] - border_w_h[1]
        top_left_x = np.random.randint(0, int(max_shift_x))
        if ud:
            top_left_y = np.random.randint(0, int(max_shift_y))
        else:
            top_left_y = h // 2
        context.move_to(top_left_x - int(box[0]), top_left_y - int(box[1]))
        context.set_source_rgb(0, 0, 0)
        context.show_text(text)

    buf = surface.get_data()
    a = np.frombuffer(buf, np.uint8)
    a.shape = (h, w, 4)
    a = a[:, :, 0]  # grab single channel
    a = a.astype(np.float32) / 255
    a = np.expand_dims(a, 0)
    if rotate:
        a = image.random_rotation(a, 3 * (w - top_left_x) / w + 1)
    a = speckle(a)

    return a


def shuffle_mats_or_lists(matrix_list, stop_ind=None):
    ret = []
    assert all([len(i) == len(matrix_list[0]) for i in matrix_list])
    len_val = len(matrix_list[0])
    if stop_ind is None:
        stop_ind = len_val
    assert stop_ind <= len_val

    a = range(stop_ind)
    np.random.shuffle(a)
    a += range(stop_ind, len_val)
    for mat in matrix_list:
        if isinstance(mat, np.ndarray):
            ret.append(mat[a])
        elif isinstance(mat, list):
            ret.append([mat[i] for i in a])
        else:
            raise TypeError('shuffle_mats_or_lists only supports '
                            'numpy.array and list objects')
    return ret


#  Enumerative translation of individual characters of the supported set to integer classes accepted within the NN itself.
def text_to_labels(text, num_classes):
    text = deaccent(unicode(text, 'utf-8'))

    ret = []
    low_offset = 26
    upp_offset = 26
    num_offset = 10

    try:
        for char in text:
            if char >= 'a' and char <= 'z':
                ret.append(ord(char) - ord('a'))
            elif char >= 'A' and char <= 'Z':
                ret.append(ord(char) - ord('A') + low_offset)
            elif char in '0123456789':
                ret.append(ord(char) - ord('0') + low_offset + upp_offset)
            elif char == ' ':
                ret.append(num_offset + low_offset + upp_offset)
            elif char == ',':
                ret.append(num_offset + low_offset + upp_offset + 1)
            elif char == '.':
                ret.append(num_offset + low_offset + upp_offset + 2)
            elif char == '-':
                ret.append(num_offset + low_offset + upp_offset + 3)
            elif char == '/':
                ret.append(num_offset + low_offset + upp_offset + 4)
            else:
                '''All other possible characters are also recognised as '/' for now
                we will have to change the model to fit in more characters, also,
                other characters are all too rare in training data for now.'''
                ret.append(num_offset + low_offset + upp_offset + 4)
        return ret
    except Exception:
        ret.append(num_offset + low_offset + upp_offset + 4)
        return ret


# Reverse translation of numerical classes back to human readable ASCII characters
def labels_to_text(labels):
    ret = []
    low_offset = 26
    upp_offset = 26
    num_offset = 10

    for c in labels:
        if c < 26:
            ret.append(chr(c + ord('a')))
        elif c < 52:
            ret.append(chr(c + ord('A') - 26))
        elif c < 62:
            ret.append(chr(c + ord('0') - 52))
        elif c == 62:
            ret.append(' ')
        elif c == 63:
            ret.append(',')
        elif c == 64:
            ret.append('.')
        elif c == 65:
            ret.append('-')
        elif c == 66:
            ret.append('/')
    return "".join(ret)


# For a real OCR application, this should be beam search with a dictionary
# and language model.  For this example, best path is sufficient.

def decode_batch(test_func, word_batch, labtype_batch, debug=False):
    out = test_func([word_batch, labtype_batch])[0]
    ret = []
    for j in range(out.shape[0]):
        out_best = list(np.argmax(out[j, 2:], 1))
        out_best = [k for k, g in itertools.groupby(out_best)]
        outstr = labels_to_text(out_best)
        ret.append(outstr)
    if debug:
        return ret, out
    return ret


# only a-z and space..probably not to difficult
# to expand to uppercase and symbols

def is_valid_str(in_str):
    # search = re.compile(r'[^a-z\ ]').search
    # return not bool(search(in_str))
    return True


# the actual loss calc occurs here despite it not being
# an internal Keras loss function

def ctc_lambda_func(args):
    y_pred, labels, input_length, label_length = args
    # the 2 is critical here since the first couple outputs of the RNN
    # tend to be garbage:
    y_pred = y_pred[:, 2:, :]
    return K.ctc_batch_cost(labels, y_pred, input_length, label_length)


class VizCallback(keras.callbacks.Callback):
    def __init__(self, run_name, test_func, text_img_gen, model, val_words):
        self.test_func = test_func
        self.val_words = val_words
        self.output_dir = os.path.join(
            OUTPUT_DIR, run_name)
        self.text_img_gen = text_img_gen
        self.model = model
        if not os.path.exists(self.output_dir):
            os.makedirs(self.output_dir)

    def show_edit_distance(self, num):
        num_left = num
        mean_norm_ed = 0.0
        mean_ed = 0.0
        wrong = 0
        right = 0
        while num_left > 0:
            word_batch = next(self.text_img_gen)[0]
            num_proc = min(word_batch['the_input'].shape[0], num_left)
            decoded_res = decode_batch(self.test_func, word_batch['the_input'][0:num_proc], word_batch['labeltype_input'][0:num_proc])
            for j in range(0, num_proc):
                ocr_result = deaccent(unicode(re.sub("[\+\/]", "", re.sub("\\s", "", decoded_res[j])), 'utf-8'))
                gold_label = re.sub("[\+\/]", "", re.sub("\\s", "", word_batch['source_str'][j]))
                if gold_label == ocr_result:
                    right += 1
                else:
                    wrong += 1
                edit_dist = editdistance.eval(decoded_res[j], word_batch['source_str'][j])
                mean_ed += float(edit_dist)
                mean_norm_ed += float(edit_dist) / len(word_batch['source_str'][j])
            num_left -= num_proc
        absacc = float(right) / (float(right) + float(wrong))
        mean_norm_ed = mean_norm_ed / num
        mean_ed = mean_ed / num
        outline = ' Out of %d samples:  Mean edit distance: %.3f Mean normalized edit distance: %0.3f\n Absolute accuracy over labels is %0.2f\n' % (
            num, mean_ed, mean_norm_ed, absacc)
        print(outline)

        return mean_norm_ed, absacc

    def on_epoch_end(self, epoch, logs={}):
        # self.model.save_weights(os.path.join(self.output_dir, 'weights%02d.h5' % (epoch)))
        mean_norm_ed, absacc = self.show_edit_distance(self.val_words)
        logs["mean_norm_ed"] = mean_norm_ed
        logs["crop_accuracy"] = absacc


# TODO: At some convenient moment, move on to List and .index()
ltypedict = {'account_num': 0,
             'amount_due': 1,
             'amount_paid': 2,
             'amount_rounding': 3,
             'amount_total': 4,
             'amount': 4,  ## Dirty cheat because somebody keeps sending us bad labels like this!
             'bank_num': 5,
             'bic': 6,
             'const_sym': 7,
             'customer_id': 8,
             'date_due': 9,
             'date_issue': 10,
             'date_uzp': 11,
             'iban': 12,
             'invoice_id': 13,
             'order_id': 14,
             'phone_num': 15,
             'recipient_dic': 16,
             'recipient_ic': 18,
             'sender_dic': 18,
             'sender_ic': 19,
             'spec_sym': 20,
             'var_sym': 21,
             }


#  The main internal dataset batch handler, built on top of the external cropset iterator.
class DataGenerator(keras.callbacks.Callback):
    def __init__(self, minibatch_size, img_w, img_h, downsample_factor, train_crop_iter, val_crop_iter, absolute_max_string_len=30,
                 train_realratio=0.0,
                 val_realratio=1.0):
        self.minibatch_size = minibatch_size
        self.img_w = img_w
        self.img_h = img_h
        self.absolute_max_string_len = absolute_max_string_len
        self.train_realratio = train_realratio
        self.val_realratio = val_realratio
        self.train_crop_iter = train_crop_iter
        self.val_crop_iter = val_crop_iter

        self.paint_func = lambda text: paint_text(text, self.img_w, self.img_h, rotate=False, ud=False, multi_fonts=False)
        self.blank_label = self.get_output_size() - 1
        self.downsample_factor = downsample_factor

        self.batch_num = 0;

    @staticmethod
    def get_output_size():
        return 68

    @staticmethod
    def ltype_to_onehot(ltype):
        num_labels = 22
        return np.eye(num_labels)[ltypedict[ltype]]

    def ltype_translate(name):
        if name is 'amount':
            na = 'amount_total'
        else:
            na = name
        return np.eye(len(ltypes))[ltypes.index(na)]

    def on_epoch_begin(self, fake1, fake2):
        pass

    def on_epoch_end(self, fake1, fake2):
        self.batch_num = 0;

    def __enter__(self):
        self.pool = Pool(processes=1, maxtasksperchild=30)
        return self

    def __exit__(self, type, value, trace):
        self.pool.close()
        self.pool.join()

    def next_train(self):
        while 1:
            yield self.get_batch(self.minibatch_size, train=True)

    def next_val(self):
        while 1:
            yield self.get_batch(self.minibatch_size, train=False)

    def get_batch(self, size, train):
        # width and height are backwards from typical Keras convention
        # because width is the time dimension when it gets fed into the RNN
        if K.image_dim_ordering() == 'th':
            x_data = np.ones([size, 1, self.img_w, self.img_h])
        else:
            x_data = np.ones([size, self.img_w, self.img_h, 1])

        labels = np.ones([size, self.absolute_max_string_len])
        input_length = np.zeros([size, 1])
        label_length = np.zeros([size, 1])
        source_str = []
        ltypes = np.zeros([size, 22], dtype='float32')
        if train:
            trstr = "TRAIN"
        else:
            trstr = "VAL"
        # print("#%d Got asked to do a nice %s minitbatch, the contents are:" % (self.batch_num, trstr))
        self.batch_num += 1
        for i in range(0, size):
            # Mix in some blank inputs.  This seems to be important for
            # achieving translational invariance
            if train and i > size - 4:
                if K.image_dim_ordering() == 'th':
                    x_data[i, 0, 0:self.img_w, :] = self.paint_func('')[0, :, :].T
                else:
                    x_data[i, 0:self.img_w, :, 0] = self.paint_func('', )[0, :, :].T
                labels[i, 0] = self.blank_label
                input_length[i] = self.img_w // self.downsample_factor - 2
                label_length[i] = 1
                source_str.append('')
                # print("Some purposefully blank input")
            else:

                if train:
                    centeredimage, gold_label, bbox, ltype = self.train_crop_iter.next()
                else:
                    centeredimage, gold_label, bbox, ltype = self.val_crop_iter.next()

                if K.image_dim_ordering() == 'th':
                    x_data[i, 0, 0:self.img_w, :] = centeredimage
                else:
                    x_data[i, 0:self.img_w, :, 0] = centeredimage
                labels[i, 0:len(text_to_labels(gold_label, self.get_output_size()))] = text_to_labels(gold_label, self.get_output_size())
                input_length[i] = self.img_w // self.downsample_factor - 2
                label_length[i] = len(text_to_labels(gold_label, self.get_output_size()))
                ltypes[i][ltypedict[ltype]] = 1
                source_str.append(deaccent(unicode(gold_label, 'utf-8')))
                # print("%d GL: \t%s\t ltype: %s" % (i, gold_label, ltype))
        inputs = {'the_input': x_data,
                  'the_labels': labels,
                  'input_length': input_length,
                  'label_length': label_length,
                  'source_str': source_str,  # used for visualization only
                  'labeltype_input': ltypes
                  }
        outputs = {'ctc': np.zeros([size])}  # dummy data for dummy loss function
        return (inputs, outputs)


# Optical Character Recognition Engine in Keras, read as "ochre", the colour
class OCkRE:
    def __init__(self, img_w=512, labeltype_hinting=True, verbose=1):
        self.verbose = verbose

        # Input Parameters
        self.img_h = 64

        # Network parameters
        self.conv_num_filters = 16
        self.filter_size = 3
        self.pool_size = 1
        self.time_dense_size = 32
        self.rnn_size = 512
        self.absolute_max_string_len = 30
        self.img_w = img_w

        if K.image_dim_ordering() == 'th':
            input_shape = (1, self.img_w, self.img_h)
        else:
            input_shape = (self.img_w, self.img_h, 1)

        act = 'relu'
        input_data = Input(name='the_input', shape=input_shape, dtype='float32')
        inner = Convolution2D(self.conv_num_filters, self.filter_size, self.filter_size, border_mode='same',
                              activation=act, init='he_normal', name='conv1')(input_data)
        inner = MaxPooling2D(pool_size=(self.pool_size, self.pool_size), name='max1')(inner)
        inner = Convolution2D(self.conv_num_filters, self.filter_size, self.filter_size, border_mode='same',
                              activation=act, init='he_normal', name='conv2')(inner)
        inner = MaxPooling2D(pool_size=(self.pool_size, self.pool_size), name='max2')(inner)

        conv_to_rnn_dims = (self.img_w // (self.pool_size ** 2), (self.img_h // (self.pool_size ** 2)) * self.conv_num_filters)
        inner = Reshape(target_shape=conv_to_rnn_dims, name='reshape')(inner)

        # cuts down input size going into RNN:
        inner = Dense(self.time_dense_size, activation=act, name='dense1')(inner)

        # experimental auxiliary label hinting input
        if labeltype_hinting:
            labeltype = Input(shape=(22,), name='labeltype_input', dtype='float32')
            compresstype = Dense(8, activation=act, name='compresstype')(labeltype)
            repeated = RepeatVector(512)(compresstype)
            inner = merge([inner, repeated], mode='concat')

        # Two layers of bidirecitonal GRUs
        # GRU seems to work as well, if not better than LSTM:
        gru_1 = Bidirectional(GRU(self.rnn_size, return_sequences=True, init='he_normal', name='gru1'), merge_mode='sum')(inner)
        gru_2 = Bidirectional(GRU(self.rnn_size, return_sequences=True, init='he_normal', name='gru2'), merge_mode='concat')(gru_1)

        # transforms RNN output to character activations:
        inner = Dense(DataGenerator.get_output_size(), init='he_normal', name='dense2')(gru_2)
        y_pred = Activation('softmax', name='softmax')(inner)
        if labeltype_hinting:
            if self.verbose != 0:
                Model(input=[input_data, labeltype], output=y_pred).summary()
            outmod = Model(input=[input_data, labeltype], output=y_pred)
        else:
            if self.verbose != 0:
                Model(input=[input_data], output=y_pred).summary()
            outmod = Model(input=[input_data], output=y_pred)

        labels = Input(name='the_labels', shape=[self.absolute_max_string_len], dtype='float32')
        input_length = Input(name='input_length', shape=[1], dtype='int64')
        label_length = Input(name='label_length', shape=[1], dtype='int64')
        # Keras doesn't currently support loss funcs with extra parameters
        # so CTC loss is implemented in a lambda layer
        loss_out = Lambda(ctc_lambda_func, output_shape=(1,), name='ctc')([y_pred, labels, input_length, label_length])

        # clipnorm seems to speeds up convergence
        # sgd = SGD(lr=0.02, decay=1e-6, momentum=0.9, nesterov=True, clipnorm=5)

        if labeltype_hinting:
            self.model = Model(input=[input_data, labels, input_length, label_length, labeltype], output=[loss_out])
        else:
            self.model = Model(input=[input_data, labels, input_length, label_length], output=[loss_out])

        # captures output of softmax so we can decode the output during visualization
        if labeltype_hinting:
            self.test_func = K.function([input_data, labeltype], [y_pred])
        else:
            self.test_func = K.function([input_data], [y_pred])

    def loadweights(self, weightsfile='densified_labeltype_best.h5'):
        if weightsfile:
            self.model.load_weights(weightsfile)

    def train(self, run_name, start_epoch, stop_epoch, verbose=1, epochlen=2048, vallen=2000):
        
        #Kind of dummy iterators, they would be passed from outside,
        #along with content of separate Training and validation real
        #data.
        train_crop_iter = CropImageIterator()
        val_crop_iter = CropImageIterator()
        
        words_per_epoch = epochlen
        val_words = len(val_crop_iter)
        img_gen = DataGenerator(minibatch_size=32, img_w=self.img_w, img_h=self.img_h, downsample_factor=(self.pool_size ** 2),
                                train_crop_iter=train_crop_iter,
                                val_crop_iter=val_crop_iter,
                                absolute_max_string_len=self.absolute_max_string_len,
                                train_realratio=1.0,
                                val_realratio=1.0
                                )
        if vallen:
            val_words = vallen

        adam = keras.optimizers.Adam(lr=1e-4, beta_1=0.9, beta_2=0.999, epsilon=1e-08)

        output_dir = os.path.join(OUTPUT_DIR, run_name)

        # the loss calc occurs elsewhere, so use a dummy lambda func for the loss
        self.model.compile(loss={'ctc': lambda y_true, y_pred: y_pred}, optimizer=adam)
        if start_epoch > 0:
            weight_file = os.path.join(OUTPUT_DIR, os.path.join(run_name, 'weights%02d.h5' % (start_epoch - 1)))
            self.model.load_weights(weight_file)

        viz_cb = VizCallback(run_name, self.test_func, img_gen.next_val(), self.model, val_words)

        weights_best_fname = os.path.join(output_dir, '%s-weights-best_loss.h5' % run_name)
        weights_best_fnamev = os.path.join(output_dir, '%s-weights-best_val_loss.h5' % run_name)
        weights_best_fnamemned = os.path.join(output_dir, '%s-weights-best_mned.h5' % run_name)
        weights_best_cro_accu = os.path.join(output_dir, '%s-weights-best_crop_accu.h5' % run_name)

        csv_logger = CSVLogger(os.path.join(output_dir, '%s.training.log' % run_name))

        checkpointer_loss = ModelCheckpoint(weights_best_fname, monitor='loss', save_best_only=True, save_weights_only=False, mode='min')
        checkpointer_vloss = ModelCheckpoint(weights_best_fnamev, monitor='val_loss', save_best_only=True, save_weights_only=False, mode='min')
        checkpointer_mned = ModelCheckpoint(weights_best_fnamemned, monitor='mean_norm_ed', save_best_only=True, save_weights_only=False, mode='min')
        checkpointer_accu = ModelCheckpoint(weights_best_cro_accu, monitor='crop_accuracy', save_best_only=True, save_weights_only=False, mode='max')

        self.model.fit_generator(generator=img_gen.next_train(), samples_per_epoch=words_per_epoch,
                                 nb_epoch=stop_epoch, validation_data=img_gen.next_val(), nb_val_samples=val_words,
                                 callbacks=[viz_cb, img_gen, checkpointer_loss, checkpointer_vloss, checkpointer_mned, checkpointer_accu, csv_logger],
                                 initial_epoch=start_epoch, verbose=verbose)

    def ocr_frompic(self, image, labeltype='amount', debug=False):
        batchimage = np.ones([1, 512, 64, 1])
        im = image
        if type(im) != np.ndarray:
            im = size_plainpad(im)
        batchimage[0, 0:512, :, 0] = im
        labdecoded = DataGenerator.ltype_to_onehot(labeltype)
        ltypes = np.zeros([1, 22], dtype='float32')
        ltypes[0, :] = labdecoded
        return decode_batch(self.test_func, batchimage, ltypes, debug)
