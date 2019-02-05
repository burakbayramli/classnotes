from mpl_toolkits.mplot3d import Axes3D
import matplotlib.pyplot as plt
from matplotlib import cm
import numpy as np
import matplotlib.pyplot as plt
import numpy.linalg as lin
from scipy.spatial.distance import cdist
from scipy.spatial.distance import pdist


def func(x, y):
    s1 = 0.2; x1 = 36.5; y1 = 32.5
    g1 = np.exp( -4 *np.log(2) * ((x-x1)**2+(y-y1)**2) / s1**2)
    s2 = 0.4; x2 = 36.1; y2 = 32.8
    g1 = np.exp( -4 *np.log(2) * ((x-x1)**2+(y-y1)**2) / s1**2)
    g2 = np.exp( -2 *np.log(2) * ((x-x2)**2+(y-y2)**2) / s2**2)    
    return g1 + g2 

D = 50
S = 100
gamma = 2.0

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
X = np.hstack((xr,yr))
print (X.shape)
Phi = np.exp(-gamma*cdist(X,X,metric='euclid'))

print (Phi.shape)

w = np.dot(lin.pinv(Phi),zr)
print (w)


znew = np.zeros(D*D)
for i in range(len(xxx)):
    tmp = 0
    if i % 100 == 0: print (i)
    for m in range(S):
        a = np.array([[xr[m,0],yr[m,0]]])
        b = np.array([[xxx[i],yyy[i]]])
        #print (a,b)
        #print (np.exp(cdist(a,b,'euclid')[0][0]))
        #print (np.exp(-gamma*cdist(a,b,'euclid')[0][0]))
        #print (w[m])
        tmp += (w[m] * np.exp(-gamma*cdist(a,b,'euclid')[0][0]))
    #print (tmp)
    znew[i] = tmp*100.0
    
fig = plt.figure()
ax = fig.gca(projection='3d')
surf = ax.plot_surface(xx, yy, znew.reshape(D,D), cmap=cm.coolwarm,linewidth=0, antialiased=False)
fig.colorbar(surf, shrink=0.5, aspect=5)
plt.savefig('/data/data/com.termux/files/home/Downloads/out2.png')
