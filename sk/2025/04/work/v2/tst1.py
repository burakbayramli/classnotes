import sys; sys.path.append("randall")
import mpl_toolkits.mplot3d as a3, numpy as np
import matplotlib.colors as colors
import matplotlib.pyplot as plt
from scipy.spatial.distance import cdist
import AABB

class Tetrahedron(AABB.IAABB):
    def __init__(self,offset):
        self.offset = offset
	# alttakiler tetrahedron seklini veren bilinen dort nokta
        self.base_tri = np.array([[ 0.2309401,  1.2309401,  0.2309401],
                                  [-0.2309401,  1.2309401, -0.2309401],
                                  [ 0.2309401,  0.7690599, -0.2309401],
                                  [-0.2309401,  0.7690599,  0.2309401]])
        self.init_triangles()

    def init_triangles(self):
        self.triangles = []
        # dort noktadan dort ucgen cikartiyoruz, base_tri icindeki 1-3 noktalari,
        # 2,3,4 noktalari, 3,4,1 noktalari ve 4,1,2 noktalarini kullanarak.
        self.triangles.append(self.base_tri[0:3,:] + self.offset)
        self.triangles.append(self.base_tri[1:4,:] + self.offset)
        tmp = np.vstack((self.base_tri[2,:],self.base_tri[3,:],self.base_tri[0,:]))
        self.triangles.append(tmp + self.offset)
        tmp = np.vstack((self.base_tri[3,:],self.base_tri[0,:],self.base_tri[1,:]))
        self.triangles.append(tmp + self.offset)
        self.tri_centers = [np.mean(x,axis=0) for x in self.triangles]

    def set_offset(self,offset):
        self.offset = offset
        self.init_triangles()

    def plot(self,ax):
        for x in self.triangles: 
            tri = a3.art3d.Poly3DCollection([x])
            tri.set_edgecolor('k')
            tri.set_color('blue')
            tri.set_alpha(0.2)
            ax.add_collection3d(tri)

    def __repr__(self):
        return f"Tetrahedron {self.offset}"

    def get_aabb(self):
        mins = np.min(self.offset + self.base_tri,axis=0)
        maxs = np.max(self.offset + self.base_tri,axis=0)
        x,y,z,w,h,d = list(mins) + list(maxs)
        return AABB.AABB(x,y,z,w,h,d)

    def get_center(self):
        c = np.mean(self.base_tri + self.offset,axis=0).reshape(1,3)
        return c

    def find_closest_triangle(self, p, i=0):
        d = cdist(p.get_center(),self.tri_centers,metric='euclid')[0]
        d2 = np.argsort(d)
        return self.triangles[d2[i]]

if __name__ == "__main__": 

    dirs = [[1,1,0],[-1,0,0]]
    offsets = [[0,0,0],[1,0,0]]

    dirs = np.array(dirs)
    ts = [Tetrahedron(offset=np.array(o)) for o in offsets]

    tree = AABB.AABBTree(initial_size=4)    
    for t in ts: tree.insert_object(t)

    for i in range(15):
        fig = plt.figure()
        ax = a3.Axes3D(fig)
        ax.view_init(elev=21, azim=40)
        
        cts = ts[0].find_closest_triangle(ts[1],i=0)
        tri = a3.art3d.Poly3DCollection([cts])
        tri.set_edgecolor('k')
        tri.set_color('red')
        ax.add_collection3d(tri)

        cts = ts[0].find_closest_triangle(ts[1],i=1)
        tri = a3.art3d.Poly3DCollection([cts])
        tri.set_edgecolor('k')
        tri.set_color('green')
        ax.add_collection3d(tri)

        cts = ts[1].find_closest_triangle(ts[0],i=0)
        tri = a3.art3d.Poly3DCollection([cts])
        tri.set_edgecolor('k')
        tri.set_color('red')
        ax.add_collection3d(tri)
        
        cts = ts[1].find_closest_triangle(ts[0],i=1)
        tri = a3.art3d.Poly3DCollection([cts])
        tri.set_edgecolor('k')
        tri.set_color('green')
        ax.add_collection3d(tri)
        
        ax.set_xlim(-1,2);ax.set_ylim(-1,2); ax.set_zlim(-1,2)
        olsum = 0
        for j in range(len(ts)):
            ts[j].plot(ax)
            ts[j].set_offset(ts[j].offset + dirs[j]*0.1)
            tree.update_object(ts[j])
        for j in range(len(ts)):
            overlaps = tree.query_overlaps(ts[j])
            olsum += len(overlaps)
        ax.text(3, 3, 4, "Overlaps: %d" % olsum)
        plt.savefig('/tmp/tetra/tetra_%02d.jpg' % i)
        plt.close(fig)
        plt.clf()
        #break
