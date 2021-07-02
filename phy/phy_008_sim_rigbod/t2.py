import numpy as np
import matplotlib.pyplot as plt
from scipy.integrate import odeint
from stl import mesh

def skew(a):
   return np.array([[0,-a[2],a[1]],[a[2],0,-a[0]],[-a[1],a[0],0]])

your_mesh = mesh.Mesh.from_file('torus.stl')
prop = your_mesh.get_mass_properties()
R0 = np.eye(3,3)
omega = np.array([1.0,1.0,1.0])
skew_omega = skew(omega)
   
def rhs(u,t):   
   R1x,R1y,R1z,R2x,R2y,R2z,R3x,R3y,R3z = u
   R = np.array([R1x,R1y,R1z,R2x,R2y,R2z,R3x,R3y,R3z])
   R = R.reshape((3,3)).T
   res = np.dot(skew_omega, R)
   return list(res.T.flatten())

LIM = 5
STEPS = 2
t=np.linspace(0.0, 1.0, STEPS)
R0 = np.eye(3,3)
u0 = R0.flatten()


tidx = 2000
apply_at = np.mean(your_mesh.vectors[tidx],axis=0)
f_at = -your_mesh.get_unit_normals()[tidx]
print (apply_at.shape)
print (f_at.shape)
tau_at = np.cross(apply_at, f_at)
print (tau_at.shape)
print (tau_at)

#u1=odeint(rhs,list(u0),t)
#print (u1)

