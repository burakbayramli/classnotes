import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from skimage import io
img = io.imread('lena2.jpg', as_gray=True)
io.imsave('/tmp/out1.png', img)

print ( img.shape )
x = range(0,img.shape[0])
y = range(0,img.shape[0])
xx,yy = np.meshgrid(x,y)
zz = np.array([img[i,j] for i,j in zip(xx,yy)])
xr = xx.flatten()
yr = yy.flatten()
zr = zz.flatten()

from scipy.interpolate import Rbf

rbfi = Rbf(xr,yr,zr,function='gaussian')
#print (list(rbfi.nodes))
print (len(rbfi.nodes))
rbfi.xi = rbfi.xi[:,np.abs(rbfi.nodes)>0.03]
rbfi.nodes = rbfi.nodes[np.abs(rbfi.nodes)>0.03]
print (len(rbfi.nodes))

znew = rbfi(xx,yy).T

io.imsave('/tmp/out2.png', znew)












