import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import numpy.linalg as lin

N = 30
x = np.linspace(-3,3,N); y = np.linspace(-3,3,N)
xx,yy = np.meshgrid(x,y)

@np.vectorize
def rbf1(xarg,yarg):
    m=np.array([1,1]); gamma=0.1; w=0.5
    return w*np.exp(-gamma*lin.norm(np.array([xarg,yarg])-m)  )

res = rbf1(xarg=1, yarg=2)
#print (res)
zz = rbf1(xx, yy)
print (zz.shape)

from mpl_toolkits.mplot3d import Axes3D
from matplotlib import cm
fig = plt.figure()
ax = fig.gca(projection='3d')
ax.view_init(elev=29, azim=29)
surf = ax.plot_surface(xx, yy, zz, cmap=cm.coolwarm,linewidth=0, antialiased=False)
plt.show()
