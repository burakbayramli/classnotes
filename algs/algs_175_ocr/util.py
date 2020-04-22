import os, random, re, itertools
import codecs, datetime
import cairocffi as cairo
import editdistance
import numpy as np
from scipy import ndimage
from keras import backend as K
from keras.preprocessing import image
import keras.callbacks

alphabet = u',.0123456789abcdefghijklmnopqrstuvwxyz '
all_chars_idx = range(len(alphabet))

def randomstring(max_string_len):
    rlen = random.choice(range(4,max_string_len))
    ridx = [random.choice(all_chars_idx) for i in range(rlen)]
    rchars = [alphabet[i] for i in ridx]
    str = "".join(rchars)
    return str

#np.random.seed(55)

def speckle(img):
    severity = np.random.uniform(0, 0.6)
    blur = ndimage.gaussian_filter(np.random.randn(*img.shape) * severity, 1)
    img_speck = (img + blur)
    img_speck[img_speck > 1] = 1
    img_speck[img_speck <= 0] = 0
    return img_speck

def paint_text(text, w, h, rotate=False, ud=False, multi_fonts=False):
    surface = cairo.ImageSurface(cairo.FORMAT_RGB24, w, h)
    with cairo.Context(surface) as context:
        context.set_source_rgb(1, 1, 1)  # White
        context.paint()
        # this font list works in CentOS 7
        if multi_fonts:
            fonts = ['Century Schoolbook',
                     'Courier', 'STIX',
                     'URW Chancery L',
                     'FreeMono']
            context.select_font_face(np.random.choice(fonts),
                                     cairo.FONT_SLANT_NORMAL,
                                     np.random.choice([cairo.FONT_WEIGHT_BOLD,
                                                       cairo.FONT_WEIGHT_NORMAL]))
        else:
            context.select_font_face('Courier',
                                     cairo.FONT_SLANT_NORMAL,
                                     cairo.FONT_WEIGHT_BOLD)
        context.set_font_size(25)
        box = context.text_extents(text)
        border_w_h = (4, 4)
        if box[2] > (w - 2 * border_w_h[1]) or box[3] > (h - 2 * border_w_h[0]):
            raise IOError('Could not fit string into image.')

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

def text_to_labels(text):
    ret = []
    for char in text:
        ret.append(alphabet.find(char))
    return ret

class TextImageGenerator(keras.callbacks.Callback):

    def __init__(self, minibatch_size,
                 img_w, img_h, downsample_factor, 
                 absolute_max_string_len):

        self.minibatch_size = minibatch_size
        self.img_w = img_w
        self.img_h = img_h
        self.downsample_factor = downsample_factor
        self.blank_label = self.get_output_size() - 1
        self.absolute_max_string_len = absolute_max_string_len
        self.paint_func = lambda text: paint_text(text, self.img_w, self.img_h,
                                                  rotate=True, ud=True, multi_fonts=True)

        self.max_string_len = 4
        
    def get_output_size(self):
        return len(alphabet) + 1

    def get_batch(self, size):
        X_data = np.ones([size, self.img_w, self.img_h, 1])
        labels = np.ones([size, self.absolute_max_string_len])
        input_length = np.zeros([size, 1])
        label_length = np.zeros([size, 1])
        source_str = []
        for i in range(size):
            word = randomstring(self.absolute_max_string_len)
            input_length[i] = self.img_w // self.downsample_factor - 2
            if random.choice(range(10)) == 0: 
                X_data[i, 0:self.img_w, :, 0] = self.paint_func('',)[0, :, :].T
                labels[i, 0] = self.blank_label
                label_length[i] = 1
                source_str.append('')
            else:
                X_data[i, 0:self.img_w, :, 0] = self.paint_func(word)[0, :, :].T
                Y_data = np.ones([1, self.absolute_max_string_len]) * -1
                Y_data[0, 0:len(word)] = text_to_labels(word)
                labels[i, :] = Y_data
                label_length[i] = len(word)
                source_str.append(word)

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
            ret = self.get_batch(self.minibatch_size)
            yield ret

