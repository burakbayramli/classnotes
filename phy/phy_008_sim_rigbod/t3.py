import numpy as np
from stl import mesh
import numpy.linalg as lin

your_mesh = mesh.Mesh.from_file('torus.stl')   
prop = your_mesh.get_mass_properties()

Ibody = np.round(prop[2],3)
Ibodyinv = lin.inv(Ibody)
dt = 0.1
x = np.zeros((3,1))
R = np.eye(3,3)
L = np.zeros((3,1))
v = np.zeros((3,1))
F = np.zeros((3,1))
M = 1
P = M*v

def skew(a):
   return np.array([[0,-a[2],a[1]],[a[2],0,-a[0]],[-a[1],a[0],0]])

tidx = 2000
apply_at = np.mean(your_mesh.vectors[tidx],axis=0)
f_at = your_mesh.get_unit_normals()[tidx]
tau0 = np.cross(apply_at, f_at).reshape(3,1) * 10.0

res = []
for i in range(30):
   xold,Rold,Pold,Lold = x.copy(),R.copy(),P.copy(),L.copy()
   
   Iinv = np.dot(np.dot(Rold, Ibodyinv), Rold.T)
   omega = np.dot(Iinv, Lold)
   skew_omega = skew(omega.reshape(3))
   R = Rold + np.dot(skew_omega, Rold) * dt
   
   v = Pold / M
   x = x + v*dt
   #F = ...
   P = Pold
   if i==0:
      L = Lold + tau0*dt
      print ('first L',L)
   else:      
      L = Lold
   res.append([x,R,P,L])
   print (R)

#exit()   

import matplotlib.pyplot as plt
from mpl_toolkits import mplot3d

LIM = 5

def plot_vector(fig, orig, v, color='blue'):
   ax = fig.gca(projection='3d')
   orig = np.array(orig); v=np.array(v)
   ax.quiver(orig[0], orig[1], orig[2], v[0], v[1], v[2],color=color)
   ax = fig.gca(projection='3d')  
   return fig

SCALE = 4


for i, [x,R,P,L] in enumerate(res):
   print (R)
   fig = plt.figure()
   axes = mplot3d.Axes3D(fig)
   your_mesh = mesh.Mesh.from_file('torus.stl')
   # force application
   o = np.mean(your_mesh.vectors[tidx],axis=0)
   n = your_mesh.get_unit_normals()[tidx]
   plot_vector(fig, o, -n*SCALE, color='red')
   
   your_mesh.rotate_using_matrix(R)
   scale = your_mesh.points.flatten()
   axes.add_collection3d(mplot3d.art3d.Poly3DCollection(your_mesh.vectors,alpha=0.3))
   plot_vector(fig, [0,0,0], omega, color='red')
   axes.auto_scale_xyz(scale, scale, scale)
   axes.set_xlim(-LIM,LIM);axes.set_ylim(-LIM,LIM);axes.set_zlim(-LIM,LIM)
   
   axes.view_init(azim=84,elev=28)
   plt.savefig('/tmp/rotate_%02d.png' % i)
   plt.close('all') 
   
# convert -delay 20 -loop 0 /tmp/rotate*.png /tmp/rotate1.gif
