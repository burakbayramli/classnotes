import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits import mplot3d
import numpy as np
from stl import mesh
import numpy.linalg as lin

def plot_vector1(fig, orig, v, color='blue'):
   orig = np.array(orig); v=np.array(v)
   ax.quiver(orig[0], orig[1], orig[2], v[0], v[1], v[2],color=color)

def plot_vector2(fig, torig, tend, color='blue'):
   v = tend - torig
   ax.quiver(torig[0], torig[1], torig[2], v[0], v[1], v[2],color=color)

mesh = mesh.Mesh.from_file('../../sk/2020/08/shapes/Prism_hexagon.stl')
cog = mesh.get_mass_properties()[1]
tidx = 7

m = 1 # kg
p = np.ones(3) * 0 # linear momentum
w = np.ones(3) * 0 # angular vel
q = np.ones(3) * 0 # orientation
F = np.ones(3) * 0 # force
tau  = np.ones(3) * 0 # torque
dt = 0.05
f0 = np.array([40,20,10])
f1 = mesh.vectors[tidx][0]
F_ext  = f1 - f0

a = f1-f0
b = cog-f0
flin = (a.dot(b) / (lin.norm(b)**2))*b
tau_ext = np.cross(f1-cog,f1-f0)
x = cog

for i,t in enumerate(np.linspace(0,1,20)):
    fig, ax = plt.subplots(1, 1, subplot_kw={'projection': '3d'})
    # x ilk degeri cog, x degistikce cog'den ne kadar uzaklasmissa
    # o farki mesh.vectors uzerine eklersek figurde gorulen objeyi
    # o kadar yerinden oynatabiliriz.
    obj = mplot3d.art3d.Poly3DCollection(mesh.vectors + (x - cog))
    obj.set_edgecolor('k')
    obj.set_alpha(0.3)
    ax.add_collection3d(obj)
    plot_vector2(ax, f0, f1)
    plot_vector1(ax, f0, flin, color='cyan')
    print (t)
    F = 0
    if t==0: p = F_ext*m
    x = x + dt*(p / m)
    print (x)
    ax.set_xlim(30,70);ax.set_ylim(-10,30); ax.set_zlim(-10,30)
    ax.view_init(elev=20, azim=200)    
    plt.savefig('img/out-%02d.jpg' % i)
    plt.close(fig)
