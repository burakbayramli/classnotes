

```python
import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits import mplot3d
from stl import mesh
import numpy.linalg as lin, sys, copy

def plot_vector1(fig, orig, v, color='blue'):
   orig = np.array(orig); v=np.array(v)
   ax.quiver(orig[0], orig[1], orig[2], v[0], v[1], v[2],color=color)

def plot_vector2(fig, torig, tend, color='blue'):
   v = tend - torig
   ax.quiver(torig[0], torig[1], torig[2], v[0], v[1], v[2],color=color)

p0 = np.array([2,1,0])
p1 = np.array([2,2,2])
p2 = np.array([1,4,0])

v1 = p1 - p0
v2 = p2 - p1
tau = np.cross(v1,v2) / 4

fig, ax = plt.subplots(1, 1, subplot_kw={'projection': '3d'})
plot_vector2(ax, p0, p1,color='red')
plot_vector2(ax, p1, p2)
plot_vector1(ax, p1, tau, color='cyan')

ax.set_xlim(0,5);ax.set_ylim(0,5); ax.set_zlim(0,5)
ax.set_xlabel('x'); ax.set_ylabel('y'); ax.set_zlabel('z'); 
ax.view_init(elev=20, azim=80)
ax.plot(0,0,0,'rd')
plt.savefig('/tmp/out1.jpg')
```

```python
f0 = np.array([40, 20, 10])
f1 = np.array([45.669872,  7.5,      10.      ])
cog = np.array([49.99997644,  4.99999825,  4.99999825])


v1 = f1 - f0
v2 = cog - f1
tau = np.cross(v2,v1) / 4

fig, ax = plt.subplots(1, 1, subplot_kw={'projection': '3d'})
plot_vector2(ax, f0, f1,color='red')
plot_vector2(ax, f1, cog)
plot_vector1(ax, f1, tau, color='cyan')

ax.set_xlim(30,70);ax.set_ylim(-10,30); ax.set_zlim(-10,30)
ax.set_xlabel('x'); ax.set_ylabel('y'); ax.set_zlabel('z'); 
ax.view_init(elev=20, azim=80)
ax.plot(0,0,0,'rd')
plt.savefig('/tmp/out2.jpg')
```






