import numpy as np
from stl import mesh
import numpy.linalg as lin

your_mesh = mesh.Mesh.from_file('torus.stl')   
prop = your_mesh.get_mass_properties()

Ibody = np.round(prop[2],3)
Ibodyinv = lin.inv(Ibody)
dt = 0.01
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
f_at = -your_mesh.get_unit_normals()[tidx]
tau0 = np.cross(apply_at, f_at).reshape(3,1)

for i in range(3):
   print ('iteration=',i)
   xold,Rold,Pold,Lold = x.copy(),R.copy(),P.copy(),L.copy()
   v = Pold / M
   x = x + v*dt
   Iinv = np.dot(np.dot(Rold, Ibodyinv), Rold.T)
   omega = np.dot(Iinv, Lold)
   skew_omega = skew(omega.reshape(3))
   R = Rold + np.dot(skew_omega, Rold) * dt
   #F = ...
   if i==0:
      L = Lold + tau0*dt
   print (R)
