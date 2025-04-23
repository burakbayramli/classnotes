import sys; sys.path.append("randall")
import mpl_toolkits.mplot3d as a3, numpy as np
import matplotlib.colors as colors, os
import matplotlib.pyplot as plt
from scipy.spatial.distance import cdist
import AABB

def plot_box(x_min, y_min, z_min, x_max, y_max, z_max, axx):        
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
        #print (x,y,z,w,h,d)
        return AABB.AABB(x,y,z,w,h,d)
    
    def __repr__(self):
        #hid = hash(str(self.corners))
        aabb = self.get_aabb()
        return f"T {aabb}"

    def plot(self,axx):
        #tri = a3.art3d.Poly3DCollection([self.corners])
        #tri.set_alpha(0.5)
        #tri.set_color('red')
        #axx.add_collection3d(tri)
        for xx in self.corners: axx.plot(xx[0],xx[1],xx[2], 'r.')
    
class PrismTri(AABB.IAABB):
    def __init__(self,offset):
        self.offset = offset
	# alttakiler tetrahedron seklini veren bilinen dort nokta
        self.init_triangles()

    def get_aabb_triangles(self):
        #print ('**************')
        #for t in self.triangles: print (t,Triangle(t,marker).get_aabb())
        #print ('**************')
        return [Triangle(t) for t in self.triangles]
        
    def init_triangles(self):
        self.triangles = []
        # dort noktadan dort ucgen cikartiyoruz, base_tri icindeki 1-3 noktalari,
        # 2,3,4 noktalari, 3,4,1 noktalari ve 4,1,2 noktalarini kullanarak.
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
            tri.set_edgecolor('k')
            tri.set_color('blue')
            tri.set_alpha(0.2)
            ax.add_collection3d(tri)

    def __repr__(self):
        return f"PrismTri {self.offset}"

    def get_aabb(self):
        tmp = np.vstack(self.triangles)
        mins = np.min(tmp,axis=0)
        maxs = np.max(tmp,axis=0)
        x,y,z,w,h,d = list(mins) + list(maxs)
        return AABB.AABB(x,y,z,w,h,d)


if __name__ == "__main__":

    if not os.path.exists("/tmp/coll"): print ("not")

    dirs = [[-2,0,4],[2,2,0]]
    offsets = [[0,0,0],[-13,-13,0]]

    dirs = np.array(dirs)
    ts = [PrismTri(offset=np.array(o)) for o in offsets]

    tree = AABB.AABBTree(initial_size=4)    
    for t in ts: tree.insert_object(t)

    for i in range(15):
        print (i,'------------------')
        fig = plt.figure()
        ax = a3.Axes3D(fig)
        ax.view_init(elev=21, azim=40)
        #ax.view_init(elev=21, azim=150)
        #ax.view_init(elev=21, azim=90)
        #ax.view_init(elev=21, azim=180)

        ax.set_xlim(20,60);ax.set_ylim(-20,20); ax.set_zlim(-20,30)
        olsum = 0
        
        for j in range(len(ts)):
            ts[j].set_offset(ts[j].offset + dirs[j]*0.5)
            tree.update_object(ts[j])
            ts[j].plot(ax)

#        for tetra in ts:
#            for tri in tetra.get_aabb_triangles():
#                aabb = tri.get_aabb()
#                plot_box(aabb.min_x, aabb.min_y, aabb.min_z,
#                         aabb.max_x, aabb.max_y, aabb.max_z, ax)
            
        for j in range(len(ts)):
            overlaps = tree.query_overlaps(ts[j])
            for other in overlaps:
                # test detailed intersection of ts[j] with other
                narrow_tree = AABB.AABBTree(initial_size=10)
                for x in other.get_aabb_triangles(): narrow_tree.insert_object(x)
                for x in ts[j].get_aabb_triangles(): narrow_tree.insert_object(x)
                for tobj in other.get_aabb_triangles(): 
                    overlaps_narrow = narrow_tree.query_overlaps(tobj)                    
                    #print ('obj',tobj.get_aabb())
                    #print ('obj obj overlaps',overlaps_narrow)
                    for tt in overlaps_narrow:
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
