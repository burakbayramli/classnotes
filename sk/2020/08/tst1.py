import sys; sys.path.append("/home/burak/Documents/classnotes/phy/phy_073_rot")
import euclid
from stl import mesh
from mpl_toolkits import mplot3d
fig = plt.figure()

ax = fig.add_subplot(1, 2, 1, projection='3d')
mesh1 = mesh.Mesh.from_file('shapes/Prism_hexagon.stl')
obj = mplot3d.art3d.Poly3DCollection(mesh1.vectors)
obj.set_edgecolor('k')
ax.add_collection3d(obj)
ax.set_xlim(30,70);ax.set_ylim(-10,30); ax.set_zlim(-10,30)
ax.view_init(elev=21, azim=40)

ax = fig.add_subplot(1, 2, 2, projection='3d')
#...
#mesh1.rotate_using_matrix(R)
obj = mplot3d.art3d.Poly3DCollection(mesh1.vectors)
obj.set_edgecolor('k')
ax.add_collection3d(obj)
ax.set_xlim(30,70);ax.set_ylim(-10,30); ax.set_zlim(-10,30)
ax.view_init(elev=21, azim=40)

plt.savefig('/tmp/out1.jpg')

