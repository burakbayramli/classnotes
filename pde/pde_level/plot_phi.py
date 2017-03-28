import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
import numpy as np

def plot_phi(phi):
    fig = plt.figure()
    ax = Axes3D(fig)
    x = []
    y = []
    for (i,j),val in np.ndenumerate(phi):
        x.append(i)
        y.append(j)
    ax.plot(xs=x, ys=y, zs=phi.flatten(), 
            zdir='z', label='ys=0, zdir=z')
    plt.show()
