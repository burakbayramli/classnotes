#!/usr/bin/env python 

'''
Fetches invoices based on filenames from a text file and proceeds
to crop out each gold field's content based on it's textbox, saving
all results into a "crops" directory as .PNG files.
The gold standard text, field type and even the bounding box coordinates
for each crop out are stored in the .PNG's metadata under the keys "text",
"labeltype" and "bbox".

^^^
This would have been the original functionality, but this is a "light" version, only operating with synthetic
data. All the real data infrastructure has been removed.

'''

from shutil import copyfile
import sys
from random import choice as oldchoice
from random import randint as randint
import traceback
import itertools
from collections import defaultdict

from PIL import Image, ImageDraw, ImageFont
from PIL import PngImagePlugin
import numpy as np
from sklearn.utils import check_random_state
import cairocffi as cairo
from scipy import ndimage

from fakestrings import randomstring

from scipy.ndimage.filters import gaussian_filter
from numpy import *


def weighted_choice(choices):
    total = sum(w for c, w in choices)
    r = random.uniform(0, total)
    upto = 0
    for c, w in choices:
        if upto + w >= r:
            return c
        upto += w
    assert False, "Shouldn't get here"


def speckle(img):
    severity = np.random.uniform(0, 0.3)
    blur = ndimage.gaussian_filter(np.random.randn(*img.shape) * severity, 1)
    img_speck = (img + blur)
    img_speck[img_speck > 1] = 1
    img_speck[img_speck <= 0] = 0
    return img_speck


def paint_text(text, w, h, rotate=False, ud=False, multi_fonts=False, randshift=False):
    surface = cairo.ImageSurface(cairo.FORMAT_RGB24, w, h)
    with cairo.Context(surface) as context:
        context.set_source_rgb(1, 1, 1)  # White
        context.paint()

        ang = pi * random.random()

        mid_x = w / 2
        mid_y = h / 2
        len = sqrt((w / 2) ** 2 + (h / 2) ** 2)

        c1x = len * sin(ang)
        c1y = len * cos(ang)
        c2x = - c1x
        c2y = - c1y

        cc1x = mid_x + c1x
        cc1y = mid_y + c1y
        cc2x = mid_x + c2x
        cc2y = mid_y + c2y

        # Lets draw some random lines
        if randint(0, 1):
            for _ in range(randint(1, 3)):
                context.new_path()
                context.move_to(randint(0, w), randint(0, h))
                context.line_to(randint(0, w), randint(0, h))
                context.set_line_width(random.random() * 3)
                shade = random.random() * .5
                context.set_source_rgb(shade, shade, shade)
                context.stroke()

        # this font list works in Centos 7
        if multi_fonts:
            fonts = ['Century Schoolbook', 'Courier', 'STIX', 'FreeMono', 'Anonymous Pro Font Family', 'Adamina Font', 'Aleo Font Family', 'Cutive Font','Habibi', 'Kameron Font Family', 'Nimbus Mono', 'Comme ExtraLight', 'Gruppo', 'MathJax_Main', 'MathJax_Typewriter', 'cmr10', 'URW Palladio L', 'Bitstream Vera Sans', 'Bitstream Vera Sans Mono', 'URW Gothic L','URW Chancery L', 'Bitstream Vera Serif', 'Nimbus Sans L', 'Standard Symbols L', 'MathJax_SansSerif', 'Nimbus Mono L', 'URW Bookman L']
            font = np.random.choice(fonts)
            slant = np.random.choice([cairo.FONT_SLANT_NORMAL, cairo.FONT_SLANT_ITALIC, cairo.FONT_SLANT_OBLIQUE])
            weight = np.random.choice([cairo.FONT_WEIGHT_BOLD, cairo.FONT_WEIGHT_NORMAL])
            context.select_font_face(font, slant, weight)
        else:
            context.select_font_face('Comme ExtraLight', cairo.FONT_SLANT_NORMAL, cairo.FONT_WEIGHT_BOLD)
        context.set_font_size(np.random.randint(15, 25))
        box = context.text_extents(text)
        border_w_h = (4, 10)
        if box[2] > (w - 2 * border_w_h[1]) or box[3] > (h - 2 * border_w_h[0]):
            raise IOError('Could not fit string into image. Max char count is too large for given image width.')

        # teach the RNN translational invariance by
        # fitting text box randomly on canvas, with some room to rotate
        max_shift_x = w - box[2] - border_w_h[0]
        max_shift_y = h - box[3] - border_w_h[1]
        if randshift:
            top_left_x = np.random.randint(0, int(max_shift_x))
        else:
            top_left_x = w // 2 - box[2] // 2
        if randshift:
            top_left_y = np.random.randint(0, int(max_shift_y))
        else:
            top_left_y = h // 2 - box[3] // 2

        context.move_to(top_left_x - int(box[0]), top_left_y - int(box[1]))
        context.set_source_rgb(0, 0, 0)
        context.show_text(text)

        ''' more trouble to maybe revisit later.
        #  lets draw some blobby ellipsoids
        with context: #randint(0, 1):
            for i in range(10):
                context.new_sub_path()
                context.move_to(w / 2., h / 2.)
                context.translate(randint(0, w), randint(0, h))
                context.scale(randint(5, 20), randint(5, 20))
                context.arc(0, 0, 1, 0, 2 * pi)
                context.set_source_rgba(0, 0, 0, 0.3)
                context.fill()
        '''

    buf = surface.get_data()
    a = np.frombuffer(buf, np.uint8)
    a.shape = (h, w, 4)
    a = a[:, :, 0]  # grab single channel
    a = a.astype(np.float32) / 255
    a = np.expand_dims(a.T, 0)
    if rotate:
        a = Image.random_rotation(a, 3 * (w - top_left_x) / w + 1)
    # a = speckle(a)

    return a


def augment(numpy_arraypic):
    nump = numpy_arraypic.reshape(512, 64)
    nump = cloud(nump)
    if randint(0, 9):
        nump = gaussianblur(nump)
    if weighted_choice(((True, 0.8), (False, 0.2))):
        nump = speckle(nump)
    contrasttype = weighted_choice([(1, 0.2), (2, 0.7), (3, 0.1)])
    if contrasttype is 2:
        nump = grayout(nump)
    if contrasttype is 3:
        nump = invertor(nump)
    return nump.reshape(1, 512, 64)


def cloud(img, h_rec=20, w_rec=40, intensity=0.1, num_rec=20):
    [x, y] = img.shape
    mask = zeros((y, x))

    for iter in range(num_rec):
        j = randint(0, x - w_rec)
        i = randint(0, y - h_rec)
        mask[i: i + h_rec, j: j + w_rec] = ones((h_rec, w_rec)) * intensity * randint(2, 5)
    mask = gaussian_filter(mask, sigma=10)
    # plt.imshow(mask, cmap='gray', norm=NoNorm())
    # plt.show()
    img = img.T + mask
    img = clip(img, 0.0, 1.0)
    return img.T


def grayout(numpypic):
    multi = float(randint(75, 100)) / 100.0
    offset = float(randint(0, (100 - multi * 100))) / 100.0
    numpypic = multi * numpypic + offset
    return numpypic


def gaussianblur(numpypic):
    sigma = float(randint(0, 120)) / 100
    return ndimage.filters.gaussian_filter(numpypic, sigma)


def invertor(numpypic):
    base = np.ones([512, 64])
    graying = float(randint(50, 100)) / 100
    return base - numpypic * graying


def sample_background(picture):
    """Desperate attempt at inferring the genuine background colour from multiple points along the outer edge of the crop"""
    return int(np.median([picture.getpixel((0, 0))[0], \
                          picture.getpixel((0, picture.getbbox()[3] / 2))[0], \
                          picture.getpixel((0, picture.getbbox()[3] - 1))[0], \
 \
                          picture.getpixel((picture.getbbox()[2] / 2, 0))[0], \
                          picture.getpixel((picture.getbbox()[2] / 2, picture.getbbox()[3] - 1))[0], \
 \
                          picture.getpixel((picture.getbbox()[2] - 1, 0))[0], \
                          picture.getpixel((picture.getbbox()[2] - 1, picture.getbbox()[3] / 2))[0], \
                          picture.getpixel((picture.getbbox()[2] - 1, picture.getbbox()[3] - 1))[0]
                          ]))


def size_plainpad(picture):
    """Padding a PIL image to a centered position and returning it as a keras friendly matrix"""
    backc = sample_background(picture)
    newpic = Image.new('RGB', (512, 64), (backc, backc, backc, 255))
    orw = picture.getbbox()[2]
    orh = picture.getbbox()[3]
    new = newpic.getbbox()[2]
    neh = newpic.getbbox()[3]
    newpic.paste(picture, (new / 2 - orw / 2, neh / 2 - orh / 2))
    newpic = newpic.convert("L")
    numparr = np.array(newpic)
    numparr = numparr.astype(np.float32) / 255
    numparr = numparr.transpose()[None, :, :]
    return numparr


def size_randpad(picture):
    """Padding a PIL image to a random position and returning it as a keras friendly matrix"""
    backc = sample_background(picture)
    newpic = Image.new('RGB', (512, 64), (backc, backc, backc, 255))
    orw = picture.getbbox()[2]
    orh = picture.getbbox()[3]
    new = newpic.getbbox()[2]
    neh = newpic.getbbox()[3]

    if orw > new or orh > neh:
        wr = float(new) / float(orw)
        hr = float(neh) / float(orh)
        rat = min([wr, hr]) * .9
        picture = picture.resize((int(orw * rat) - 1, int(orh * rat) - 1), Image.LANCZOS)
        orw = picture.getbbox()[2]
        orh = picture.getbbox()[3]

    if orh > float(neh) * .5:
        multw = float(randint(int(orw * .5), new)) / float(orw)
        multh = float(randint(int(orh * .5), neh)) / float(orh)
    else:
        multw = float(randint(orw, new)) / float(orw)
        multh = float(randint(orh, neh)) / float(orh)
    mult = min([multw, multh])
    orw = int(float(orw) * float(mult))
    orh = int(float(orh) * float(mult))
    picture = picture.resize((orw, orh), Image.LANCZOS)
    randoffw = randint(0, new - orw)
    randoffh = randint(0, neh - orh)
    tup = (randoffw, randoffh)
    newpic.paste(picture, tup)
    newpic = newpic.convert("L")
    numparr = np.array(newpic)
    numparr = numparr.astype(np.float32) / 255
    numparr = numparr.transpose()[None, :, :]
    return numparr


class CropImageIterator(object):
    """
    Uses the two methods above to produce a somewhat more useful iterator, which can be iterated over
    infinitely, shuffling itself automatically whenever it runs out of crops to offer.
    It also returns actual 512x64 padded crops, rather than whatever dimension comes out of the original boundiboxes.
    In future it shall also include possibility of various data augmentation through noising and whatnot.

    It has two modes. The default mode yields tuples of;
    (Keras friendly np array of the picture, correspnoding label, bbox, type of the label)
    The pass_by_file mode applies the usual padding, creating a new temporary .png file in /tmp/, to be used by the other
    end application. THE RECEIVING APPLICATION IS EXPECTED TO CLEAN THE .PNG FILE UP ON IT'S OWN!
    """

    def __init__(self, wantedlablist=None, random_state=42, img_w=512, img_h=64, pass_by_file=False, fakemixin=None, randpad=False):
        self.wantedlablist = wantedlablist
        self.random_state = check_random_state(random_state)
        self.init_loop()
        self.img_w = img_w
        self.img_h = img_h
        self.pass_by_file = pass_by_file
        self.fakemixin = fakemixin
        self.randpad = randpad

    def __enter__(self):
        return self

    def __exit__(self, type, value, trace):
        pass

    def init_loop(self):
        self.i = 0

    def __iter__(self):
        return self

    def __next__(self):
        return self.next()

    def __len__(self):
        return 2000 #Firmly set to stop validation at a reasonable amount, has no purpose without real data.

    def set_pass_by_file(self, pass_by_file=True):
        self.pass_by_file = pass_by_file

    def next(self):
        rstr = randomstring(self.wantedlablist)
        return augment(paint_text(rstr[0], 512, 64, multi_fonts=True, randshift=self.randpad)), rstr[0], "no_file", rstr[1]
        

