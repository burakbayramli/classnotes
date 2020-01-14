import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from skimage import io
img = io.imread('lena2.jpg', as_gray=True)
io.imsave('/tmp/out1.png', img)

print (img.shape)
print (img.shape[0]*img.shape[0])
x = range(0,img.shape[0])
y = range(0,img.shape[0])
xx,yy = np.meshgrid(x,y)
zz = np.array([img[i,j] for i,j in zip(xx,yy)])
xr = xx.flatten()
yr = yy.flatten()
zr = zz.flatten()

S = 4000
np.random.seed(0)
idx = np.random.choice(range(img.shape[0]*img.shape[0]),size=S,replace=False)
xr = xr[idx]
yr = yr[idx]
zr = zr[idx]

from scipy.interpolate import Rbf

rbfi = Rbf(xr,yr,zr,function='gaussian')

znew = rbfi(xx,yy).T

io.imsave('/tmp/out2.png', znew)












