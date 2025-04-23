import sys; sys.path.append("randall")
import mpl_toolkits.mplot3d as a3, numpy as np
import matplotlib.colors as colors, os
import matplotlib.pyplot as plt, itertools
from scipy.spatial.distance import cdist
import AABB

def pairwise(iterable):
    a, b = itertools.tee(iterable)
    return itertools.zip_longest(a, b, fillvalue=next(b, None))

def plot_box_imp(x_min, y_min, z_min, x_max, y_max, z_max, axx):        
    axx.plot3D([x_min, x_max], [y_min, y_min], [z_min, z_min], 'y--')
    axx.plot3D([x_min, x_max], [y_max, y_max], [z_min, z_min], 'y--')
    axx.plot3D([x_min, x_max], [y_min, y_min], [z_max, z_max], 'y--')
    axx.plot3D([x_min, x_max], [y_max, y_max], [z_max, z_max], 'y--')
    axx.plot3D([x_min, x_min], [y_min, y_max], [z_min, z_min], 'y--')
    axx.plot3D([x_max, x_max], [y_min, y_max], [z_min, z_min], 'y--')
    axx.plot3D([x_min, x_min], [y_min, y_max], [z_max, z_max], 'y--')
    axx.plot3D([x_max, x_max], [y_min, y_max], [z_max, z_max], 'y--')
    axx.plot3D([x_min, x_min], [y_min, y_min], [z_min, z_max], 'y--')
    axx.plot3D([x_max, x_max], [y_min, y_min], [z_min, z_max], 'y--')
    axx.plot3D([x_min, x_min], [y_max, y_max], [z_min, z_max], 'y--')
    axx.plot3D([x_max, x_max], [y_max, y_max], [z_min, z_max], 'y--')

class Triangle(AABB.IAABB):
    def __init__(self,corners):
        self.corners = corners

    def get_aabb(self):
        mins = np.min(self.corners,axis=0)
        maxs = np.max(self.corners,axis=0)
        x,y,z,w,h,d = list(mins) + list(maxs)
        return AABB.AABB(x,y,z,w,h,d)
    
    def __repr__(self):
        aabb = self.get_aabb()
        return f"T {aabb}"
    
    def plot(self,axx):
        xs = []; ys = []; zs = []
        for fr,to in list(pairwise(self.corners)):
            xs.append(fr[0]); ys.append(fr[1]); zs.append(fr[2])
            xs.append(to[0]); ys.append(to[1]); zs.append(to[2])        
        axx.plot(xs, ys, zs, 'red')

    def plot_box(self,axx):
        mins = np.min(self.corners,axis=0)
        maxs = np.max(self.corners,axis=0)
        x,y,z,w,h,d = list(mins) + list(maxs)
        plot_box_imp(x,y,z,w,h,d,axx)
        
class PrismTri(AABB.IAABB):
    def __init__(self,offset):
        self.offset = offset
        self.init_triangles()

    def get_aabb_triangles(self):
        return [Triangle(t) for t in self.triangles]
        
    def init_triangles(self):
        self.triangles = []
        self.triangles.append(np.array([[40.,  0.,  0.],
                                        [35., 10.,  0.],
                                        [40.,  0., 10.]]) + self.offset)

        self.triangles.append(np.array( [[40.,  0., 10.],
                                         [35., 10.,  0.],
                                         [35., 10., 10.]]) + self.offset)

        self.triangles.append(np.array( [[30.,  0.,  0.],
                                          [40.,  0.,  0.],
                                          [30.,  0., 10.]]) + self.offset)

        self.triangles.append(np.array([[30.,  0., 10.],
                                        [40.,  0.,  0.],
                                        [40.,  0., 10.]]) + self.offset)

        self.triangles.append(np.array( [[35., 10.,  0.],
                                         [30.,  0.,  0.],
                                         [35., 10., 10.]]) + self.offset)

        self.triangles.append(np.array( [[35., 10., 10.],
                                         [30.,  0.,  0.],
                                         [30.,  0., 10.]]) + self.offset)

        self.triangles.append(np.array([[35., 10., 10.],
                                        [30.,  0., 10.],
                                        [40.,  0., 10.]]) + self.offset)

        self.triangles.append(np.array([[30.,  0.,  0.],
                                        [35., 10.,  0.],
                                        [40.,  0.,  0.]]) + self.offset)

    def set_offset(self,offset):
        self.offset = offset
        self.init_triangles()

    def plot(self,ax):
        for x in self.triangles: 
            tri = a3.art3d.Poly3DCollection([x])
            tri.set_color('blue')
            tri.set_linestyle('dotted')
            tri.set_alpha(0.1)
            ax.add_collection3d(tri)
            Triangle(x).plot_box(ax)

    def __repr__(self):
        return f"PrismTri {self.offset}"

    def get_aabb(self):
        tmp = np.vstack(self.triangles)
        mins = np.min(tmp,axis=0)
        maxs = np.max(tmp,axis=0)
        x,y,z,w,h,d = list(mins) + list(maxs)
        return AABB.AABB(x,y,z,w,h,d)


if __name__ == "__main__":

    if not os.path.exists("/tmp/coll"): os.mkdir ("/tmp/coll")

    offsets = [[20,0,0],[-10,-10,0]]
    dirs = [[-1,-2,-1],[4,2,1]]

    dirs = np.array(dirs)
    ts = [PrismTri(offset=np.array(o)) for o in offsets]

    tree = AABB.AABBTree(initial_size=4)    
    for t in ts: tree.insert_object(t)

    for i in range(15):
        print (i,'------------------')
        fig = plt.figure()
        ax = a3.Axes3D(fig)
        ax.view_init(elev=21, azim=40)
        #ax.view_init(elev=21, azim=90)
        #ax.view_init(elev=21, azim=150)
        #ax.view_init(elev=21, azim=180)
        #ax.view_init(elev=21, azim=270)

        ax.set_xlim(20,60);ax.set_ylim(-20,20); ax.set_zlim(-20,30)
        olsum = 0
        
        for j in range(len(ts)):
            ts[j].set_offset(ts[j].offset + dirs[j]*0.5)
            tree.update_object(ts[j])
            ts[j].plot(ax)
            
        for j in range(len(ts)):
            overlaps = tree.query_overlaps(ts[j])
            for other in overlaps:
                narrow_tree = AABB.AABBTree(initial_size=10)
                for x in other.get_aabb_triangles(): narrow_tree.insert_object(x)
                for x in ts[j].get_aabb_triangles(): narrow_tree.insert_object(x)
                for tobj in ts[j].get_aabb_triangles(): 
                    overlaps_narrow = narrow_tree.query_overlaps(tobj)                    
                    for tt in overlaps_narrow:
                        print ('overlap')
                        tt.plot(ax)
                
            olsum += len(overlaps)
        ax.text(45, 0, 35, "Overlaps: %d" % olsum)
        ax.set_xlabel("x axis")
        ax.set_ylabel("y axis")
        ax.set_zlabel("z axis")
        plt.savefig('/tmp/coll/coll_%02d.jpg' % i)
        plt.close(fig)
        plt.clf()
        #break
