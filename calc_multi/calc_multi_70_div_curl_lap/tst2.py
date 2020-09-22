import numpy as np
import scipy.io as sio

mat = sio.loadmat('wind.mat')
x = mat['x']; y = mat['y']; z = mat['z']
u = mat['u']; v = mat['v']; w = mat['w']

i=10;j=10;k=8;


