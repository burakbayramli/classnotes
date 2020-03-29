import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import numpy.linalg as lin

import numpy.linalg as lin

N = 30
x = np.linspace(-3,3,N); y = np.linspace(-3,3,N)
xx,yy = np.meshgrid(x,y)

@np.vectorize
def rbf1(xarg,yarg):
    m1=np.array([1,1]); gamma1=0.5; w1=-0.5
    return w1*np.exp(-gamma1*lin.norm(np.array([xarg,yarg])-m1)  )

res = rbf1(xarg=1, yarg=2)
#print (res)
zz = rbf1(xx, yy)
print (zz.shape)

from mpl_toolkits.mplot3d import Axes3D
from matplotlib import cm
fig = plt.figure()
ax = fig.gca(projection='3d')
ax.view_init(elev=29, azim=-29)

point  = np.array([2, 2, -0.3])
normal = np.array([0, 0, 1])
d = -point.dot(normal)
zp = (-normal[0] * xx - normal[1] * yy - d) * 1. /normal[2]
ax.plot_wireframe(xx, yy, zp)

ax.plot_surface(xx, yy, zz, cmap=cm.coolwarm,linewidth=0, antialiased=False)
plt.show()
