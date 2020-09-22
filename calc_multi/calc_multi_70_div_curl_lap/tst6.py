import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import axes3d
import scipy.io as sio

def curl(x,y,z,u,v,w):
    dx = x[0,:,0]
    dy = y[:,0,0]
    dz = z[0,0,:]

    dummy, dFx_dy, dFx_dz = np.gradient (u, dx, dy, dz, axis=[1,0,2])
    dFy_dx, dummy, dFy_dz = np.gradient (v, dx, dy, dz, axis=[1,0,2])
    dFz_dx, dFz_dy, dummy = np.gradient (w, dx, dy, dz, axis=[1,0,2])

    rot_x = dFz_dy - dFy_dz
    rot_y = dFx_dz - dFz_dx
    rot_z = dFy_dx - dFx_dy

    l = np.sqrt(np.power(u,2.0) + np.power(v,2.0) + np.power(w,2.0));

    m1 = np.multiply(rot_x,u)
    m2 = np.multiply(rot_y,v)
    m3 = np.multiply(rot_z,w)

    tmp1 = (m1 + m2 + m3)
    tmp2 = np.multiply(l,2.0)

    av = np.divide(tmp1, tmp2)

    return rot_x, rot_y, rot_z, av

mat = sio.loadmat('wind.mat')
x = mat['x']; y = mat['y']; z = mat['z']
u = mat['u']; v = mat['v']; w = mat['w']

i=5;j=7;k=8;

S = 3
x1 = x[i-S:i+S, j-S:j+S, k-S:k+S]; 
y1 = y[i-S:i+S, j-S:j+S, k-S:k+S]; 
z1 = z[i-S:i+S, j-S:j+S, k-S:k+S];

u1 = u[i-S:i+S, j-S:j+S, k-S:k+S]; 
v1 = v[i-S:i+S, j-S:j+S, k-S:k+S]; 
w1 = w[i-S:i+S, j-S:j+S, k-S:k+S];

fig = plt.figure()
ax = fig.gca(projection='3d')
ax.view_init(elev=47, azim=-145)

ax.quiver(x1, y1, z1, u1, v1, w1, length=0.05, color = 'black')

rot_x, rot_y, rot_z, av = curl(x,y,z,u,v,w)

i=5;j=7;k=8;
x0=x[i,j,k]
y0=y[i,j,k]
z0=z[i,j,k]
cx0=rot_x[i,j,k]
cy0=rot_y[i,j,k]
cz0=rot_z[i,j,k]
ax.quiver(x0, y0, z0, 0, cy0, cz0, length=1.0, color = 'blue')

plt.show()
