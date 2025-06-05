import numpy as np
from stl import mesh
import numpy.linalg as lin

obj = mesh.Mesh.from_file('../../sk/2020/08/shapes/Prism_hexagon.stl')
cog = obj.get_mass_properties()[1]
tidx = 7

m = 1 # kg
p = np.ones(3) * 0 # linear momentum
w = np.ones(3) * 0 # angular vel
q = np.ones(3) * 0 # orientation
F = np.ones(3) * 0 # force
tau  = np.ones(3) * 0 # torque
dt = 0.05
f0 = np.array([40,20,10])
f1 = obj.vectors[tidx][0]
F_ext  = f1 - f0

a = f1-f0
b = cog-f0
flin = (a.dot(b) / (lin.norm(b)**2))*b
tau_ext = np.cross(f1-cog,f1-f0)
x = cog

for t in np.linspace(0,1,20):
    print (t)
    F = 0
    if t==0: p = F_ext*m
    x = x + dt*(p / m)
    print (x)
