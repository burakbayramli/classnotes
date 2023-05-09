import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation, writers
from scipy import *
from scipy import integrate
from scipy.integrate import ode, odeint
import numpy as np

fig,ax = plt.subplots(1,1,figsize=(8,8))

xmin,xmax = 0,8
ymin,ymax = -2,2
gridpoints = 500
I = .9
alpha = .05
x1,y1 = np.linspace(xmin,xmax,gridpoints),np.linspace(ymin,ymax,gridpoints)
X,Y = np.meshgrid(x1,y1 )
U = Y
V = I - np.sin(X) - alpha*Y

start = [[2,1],[4,-1]]

strm = ax.streamplot( X,Y,U, V, linewidth=.2)
strmS = ax.streamplot(x1,y1, U, V, start_points=start,
                      color="crimson",
                      linewidth=.5)
ax.set_facecolor(plt.cm.gray(.95))
ax.set_title(r'Josephson Histeresis for $ I = {0},  \alpha = {1}$ in Strogatz 8.5'.format(I, alpha))
ax.set_xticks([0., .5*np.pi, np.pi, 1.5*np.pi, 2*np.pi])
ax.set_xticklabels(["$0$", r"$\frac{1}{2}\pi$",
                     r"$\pi$", r"$\frac{3}{2}\pi$", r"$2\pi$"])

ax.set_xlim([xmin,xmax])
ax.set_ylim([ymin,ymax])
ax.set_xlabel(r"$\phi$")
ax.set_ylabel(r"$y$")
ax.grid(True)
plt.savefig('13_13.png')
