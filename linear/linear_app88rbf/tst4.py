from mpl_toolkits.mplot3d import Axes3D
import matplotlib.pyplot as plt
from matplotlib import cm
import numpy as np
import matplotlib.pyplot as plt

def func(x, y):
    s1 = 0.2; x1 = 36.5; y1 = 32.5
    g1 = np.exp( -4 *np.log(2) * ((x-x1)**2+(y-y1)**2) / s1**2)
    s2 = 0.4; x2 = 36.1; y2 = 32.8
    g1 = np.exp( -4 *np.log(2) * ((x-x1)**2+(y-y1)**2) / s1**2)
    g2 = np.exp( -2 *np.log(2) * ((x-x2)**2+(y-y2)**2) / s2**2)    
    return g1 + g2 

D = 200
S = 400
gamma = 400.0

x = np.linspace(36,37,D)
y = np.linspace(32,33,D)

xx,yy = np.meshgrid(x,y)
zz = func(xx,yy)

xxx = xx.reshape(D*D)
yyy = yy.reshape(D*D)
zzz = zz.reshape(D*D)
idx = np.random.choice(range(D*D),S)

xr = xxx[idx].reshape(S,1)
yr = yyy[idx].reshape(S,1)
zr = zzz[idx].reshape(S,1)
X = np.vstack((xr,yr)).T
print (X.shape)

import numpy.linalg as lin
from scipy.spatial.distance import cdist

Phi = np.exp(-gamma*cdist(xr,yr,'euclid'))

print (Phi.shape)

w = np.dot(lin.pinv(Phi),zr)

rmse = 0
for i in range(len(xr)):
    tmp = np.exp(-gamma * np.sqrt( (xr[i]-xr)**2 + (yr[i]-yr)**2 ) )
    rmse += (np.dot(w.T,tmp)-zr[i])**2
rmse = np.sqrt(np.mean(rmse) / len(xr))
print ('rmse',rmse)

