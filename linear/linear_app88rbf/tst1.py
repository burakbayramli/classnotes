from mpl_toolkits.mplot3d import Axes3D
import matplotlib.pyplot as plt
from matplotlib import cm
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

def func(x, y):
    s1 = 0.2; x1 = 36.5; y1 = 32.5
    g1 = np.exp( -4 *np.log(2) * ((x-x1)**2+(y-y1)**2) / s1**2)
    s2 = 0.4; x2 = 36.1; y2 = 32.8
    g1 = np.exp( -4 *np.log(2) * ((x-x1)**2+(y-y1)**2) / s1**2)
    g2 = np.exp( -2 *np.log(2) * ((x-x2)**2+(y-y2)**2) / s2**2)
    return g1 + g2

points = np.random.rand(1000, 2)

x = np.linspace(36,37,100)
y = np.linspace(32,33,100)

xx,yy = np.meshgrid(x,y)
zz = func(xx,yy)
#print (zz)

fig = plt.figure()
ax = fig.gca(projection='3d')
surf = ax.plot_surface(xx, yy, zz, cmap=cm.coolwarm,linewidth=0, antialiased=False)
fig.colorbar(surf, shrink=0.5, aspect=5)

#grid_x, grid_y = np.mgrid[0:1:100j, 0:1:200j]
#import matplotlib.pyplot as plt
#plt.subplot(221)
#plt.imshow(func(grid_x, grid_y).T, extent=(0,1,0,1), origin='lower')
#plt.plot(points[:,0], points[:,1], 'k.', ms=1)
#plt.title('Original')

plt.savefig('/data/data/com.termux/files/home/Downloads/out1.png')
