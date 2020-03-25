from scipy.interpolate import Rbf
from mpl_toolkits.mplot3d import Axes3D
import matplotlib.pyplot as plt
from matplotlib import cm
import numpy as np

np.random.seed(0)
N = 20

def Rosen(x,y):
    return 100*(y-x**2)**2+(1-x)**2

def Peaks(x,y):
    return 3*(1-x)**2*np.exp(-(x**2) - (y+1)**2) \
   - 10*(x/5 - x**3 - y**5)*np.exp(-x**2-y**2) \
   - 1/3*np.exp(-(x+1)**2 - y**2)

x = np.linspace(-2,2,100)
y = np.linspace(-3,3,100)
X, Y = np.meshgrid(x, y)
#Z = Rosen(X, Y)
Z = Peaks(X, Y)

fig = plt.figure(figsize = (8,4))
ax = fig.gca(projection='3d')
ax.plot_surface(X,Y,Z,rstride = 5, cstride = 5, cmap = 'jet', alpha = .4, edgecolor = 'none' )
ax.view_init(10, -133)
plt.show()
