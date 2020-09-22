import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import axes3d
import scipy.io as sio

mat = sio.loadmat('wind.mat')
x = mat['x']; y = mat['y']; z = mat['z']
u = mat['u']; v = mat['v']; w = mat['w']

i=5;j=7;k=8;

S = 3
x = x[i-S:i+S, j-S:j+S, k-S:k+S]; 
y = y[i-S:i+S, j-S:j+S, k-S:k+S]; 
z = z[i-S:i+S, j-S:j+S, k-S:k+S];

u = u[i-S:i+S, j-S:j+S, k-S:k+S]; 
v = v[i-S:i+S, j-S:j+S, k-S:k+S]; 
w = w[i-S:i+S, j-S:j+S, k-S:k+S];

fig = plt.figure()
ax = fig.gca(projection='3d')
ax.view_init(elev=47, azim=-145)

ax.quiver(x, y, z, u, v, w, length=0.05, color = 'black')
plt.show()
