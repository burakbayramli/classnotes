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
S = 500
x = np.linspace(36,37,D)
y = np.linspace(32,33,D)

xx,yy = np.meshgrid(x,y)
zz = func(xx,yy)

xxx = xx.reshape(D*D)
yyy = yy.reshape(D*D)
zzz = zz.reshape(D*D)
idx = np.random.choice(range(D*D),S)

xr = xxx[idx]
yr = yyy[idx]
zr = zzz[idx]
print (xr.shape)

gamma = 43.0
xxx = np.subtract.outer(xr,xr).T
yyy = np.subtract.outer(yr,yr).T
#aaa = np.exp(-gamma * np.sqrt(xxx**2 + yyy**2))
aaa = np.exp(-gamma * (xxx**2 + yyy**2))
print (aaa.shape)
import scipy.linalg as lin
w = lin.solve(aaa, zr)
print (w[:5])

xnew = [36.4,32.4]
#tmp = np.exp(-gamma * np.sqrt((xnew[0]-xr)**2 + (xnew[1]-yr)**2))
tmp = np.exp(-gamma * ((xnew[0]-xr)**2 + (xnew[1]-yr)**2))
res = np.dot(w,tmp)
print (res)

fig = plt.figure()
ax = fig.gca(projection='3d')
surf = ax.plot_surface(xx, yy, zz, cmap=cm.coolwarm,linewidth=0, antialiased=False)
fig.colorbar(surf, shrink=0.5, aspect=5)
znew = np.zeros(S)
for i in range(len(xr)):
    tmp = np.exp(-gamma * (xr[i]-xr)**2 + (yr[i]-yr)**2)
    znew[i] = np.dot(w,tmp)

plt.plot(xr,yr,znew,'.')
    
plt.savefig('/data/data/com.termux/files/home/Downloads/out2.png')

