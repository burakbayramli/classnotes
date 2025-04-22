import sys; sys.path.append("randall")
import mpl_toolkits.mplot3d as a3, numpy as np
import matplotlib.colors as colors
import matplotlib.pyplot as plt
import AABB

class Tetrahedron(AABB.IAABB):
    def __init__(self,offset):
        self.offset = offset
	# alttakiler tetrahedron seklini veren bilinen dort nokta
        self.base_tri = np.array([[ 0.2309401,  1.2309401,  0.2309401],
                                  [-0.2309401,  1.2309401, -0.2309401],
                                  [ 0.2309401,  0.7690599, -0.2309401],
                                  [-0.2309401,  0.7690599,  0.2309401]])

    def get_triangles(self):
        res = []
        res.append(self.base_tri[0:3,:] + self.offset)
        res.append(self.base_tri[1:4,:] + self.offset)
        last = np.vstack((self.base_tri[2:4,:],self.base_tri[0,:]))
        res.append(last + self.offset)
        return res

    def set_offset(offset):
        self.offset = offset

    def plot(self,ax):
        tris = self.get_triangles()
        for x in tris: 
            tri = a3.art3d.Poly3DCollection([x])
            tri.set_edgecolor('k')
            tri.set_color('red')
            tri.set_alpha(0.2)
            ax.add_collection3d(tri)

    def __repr__(self):
        return f"Tetrahedron {self.offset}"

    def get_aabb(self):
        mins = np.min(self.offset + self.base_tri,axis=0)
        maxs = np.max(self.offset + self.base_tri,axis=0)
        x,y,z,w,h,d = list(mins) + list(maxs)
        return AABB.AABB(x,y,z,w,h,d)


if __name__ == "__main__": 

    dirs = []
    ts = []

    dirs.append(np.array([1,1,0]))
    ts.append(Tetrahedron(offset=np.array([0,0,0])))

    for i in range(15):
        fig = plt.figure()
        ax = a3.Axes3D(fig)
        ax.set_xlim(-1,2)
        ax.set_ylim(-1,2)
        ax.set_zlim(-1,2)
        ts[0].plot(ax)
        plt.savefig('/tmp/tetra/tetra_%02d.jpg' % i)
        ts[0].offset = ts[0].offset + dirs[0]*0.1
        plt.close(fig)
