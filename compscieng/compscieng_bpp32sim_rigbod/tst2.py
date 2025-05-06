import matplotlib.pyplot as plt
from mpl_toolkits import mplot3d
import numpy as np
from stl import mesh
import numpy.linalg as lin

LIM = 5
SCALE = 4

your_mesh = mesh.Mesh.from_file('torus.stl')   
prop = your_mesh.get_mass_properties()
cog = np.round(prop[1],3) # baslangic aninda obje COG
Ibody = np.round(prop[2],3)
Ibodyinv = lin.inv(Ibody)
dt = 0.1
x = np.zeros((1,3))
R = np.eye(3,3)
L = np.zeros((1,3))
v = np.zeros((1,3))
F = np.zeros((3,1))
M = 1
P = M*v

def skew(a):
   return np.array([[0,-a[2],a[1]],[a[2],0,-a[0]],[-a[1],a[0],0]])

def plot_vector(fig, orig, v, color='blue'):
   ax = fig.gca(projection='3d')
   orig = np.array(orig); v=np.array(v)
   ax.quiver(orig[0], orig[1], orig[2], v[0], v[1], v[2],color=color)
   ax = fig.gca(projection='3d')  
   return fig


#tidx = 670; az = 300
tidx = 2200; az = 30
apply_at = np.mean(your_mesh.vectors[tidx],axis=0) - cog
f_at = 1 * 5 * your_mesh.get_unit_normals()[tidx]
tau0 = np.cross(apply_at, f_at).reshape(1,3) * 10.0
flindir = cog-apply_at
flin0 = np.dot(f_at,flindir)*(flindir/np.abs(lin.norm(flindir)))

for i in range(30):
   xold,Rold,Pold,Lold = x.copy(),R.copy(),P.copy(),L.copy()
   
   Iinv = np.dot(np.dot(Rold, Ibodyinv), Rold.T)
   omega = np.dot(Iinv, Lold.T).T
   omega = omega.reshape(3)
   skew_omega = skew(omega)
   R = Rold + np.dot(skew_omega, Rold) * dt

   v = Pold / M
   x = x + v*dt
   P = Pold
   if i==0: # baslangic ani
      L = Lold + tau0*dt
      P = Pold + (flin0*dt)
   else:      
      L = Lold # sonraki adimlarda degisim yok
      P = Pold # momentum ayni kaliyor
      
   fig = plt.figure()
   axes = mplot3d.Axes3D(fig)
   # t-0 aninda uygulanan kuvvet yonunu goster
   mesh0 = mesh.Mesh.from_file('torus.stl')   
   o = np.mean(mesh0.vectors[tidx],axis=0)
   n = mesh0.get_unit_normals()[tidx]
   plot_vector(fig, o, n*SCALE, color='red')
   
   your_mesh.rotate_using_matrix(R)
   your_mesh.translate(x.reshape(3))
   scale = your_mesh.points.flatten()
   axes.add_collection3d(mplot3d.art3d.Poly3DCollection(your_mesh.vectors,alpha=0.3))
   axes.auto_scale_xyz(scale, scale, scale)
   axes.set_xlim(-LIM,LIM);axes.set_ylim(-LIM,LIM);axes.set_zlim(-LIM,LIM)

   #az = 80-(i*2) # kamera dondurmek icin
   axes.view_init(azim=az,elev=28)
   plt.savefig('/tmp/rotate/rotate_%02d.jpg' % i)
   plt.close('all')       
