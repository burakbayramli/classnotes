import sys; sys.path.append("randall")
import mpl_toolkits.mplot3d as a3, numpy as np
import matplotlib.colors as colors
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
    def __init__(self,corners,marker):
        self.corners = corners
        self.marker = marker

    def get_aabb(self):
        mins = np.min(self.corners,axis=0)
        maxs = np.max(self.corners,axis=0)
        x,y,z,w,h,d = list(mins) + list(maxs)
        #print (x,y,z,w,h,d)
        return AABB.AABB(x,y,z,w,h,d)
    
    def __repr__(self):
        #hid = hash(str(self.corners))
        aabb = self.get_aabb()
        return f"T {self.marker}, {aabb}"
    
class Tetrahedron(AABB.IAABB):
    def __init__(self,offset):
        self.offset = offset
	# alttakiler tetrahedron seklini veren bilinen dort nokta
        self.base_tri = np.array([[ 0.2309401,  1.2309401,  0.2309401],
                                  [-0.2309401,  1.2309401, -0.2309401],
                                  [ 0.2309401,  0.7690599, -0.2309401],
                                  [-0.2309401,  0.7690599,  0.2309401]]) * 1.2
        self.init_triangles()

    def get_aabb_triangles(self,marker):
        #print ('**************')
        #for t in self.triangles: print (t,Triangle(t,marker).get_aabb())
        #print ('**************')
        return [Triangle(t,marker) for t in self.triangles]
        
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


if __name__ == "__main__": 

    dirs = [[1,1,0],[-0.9,0,0]]
    offsets = [[0,0,0],[0.8,-0.25,0]]

    dirs = np.array(dirs)
    ts = [Tetrahedron(offset=np.array(o)) for o in offsets]

    tree = AABB.AABBTree(initial_size=4)    
    for t in ts: tree.insert_object(t)

    for i in range(15):
        print (i,'------------------')
        fig = plt.figure()
        ax = a3.Axes3D(fig)
        #ax.view_init(elev=21, azim=40)
        ax.view_init(elev=21, azim=100)
                
        ax.set_xlim(-1,2);ax.set_ylim(-1,2); ax.set_zlim(-1,2)
        olsum = 0
        
        for j in range(len(ts)):
            ts[j].set_offset(ts[j].offset + dirs[j]*0.1)
            tree.update_object(ts[j])
            ts[j].plot(ax)

        for tetra in ts:
            for tri in tetra.get_aabb_triangles("aa"):
                aabb = tri.get_aabb()
                plot_box(aabb.min_x, aabb.min_y, aabb.min_z,
                         aabb.max_x, aabb.max_y, aabb.max_z, ax)
            
        for j in range(len(ts)):
            overlaps = tree.query_overlaps(ts[j])
            for other in overlaps:
                # test detailed intersection of ts[j] with other
                narrow_tree = AABB.AABBTree(initial_size=10)
                for x in other.get_aabb_triangles(marker="other"): narrow_tree.insert_object(x)
                for x in ts[j].get_aabb_triangles(marker="main"): narrow_tree.insert_object(x)
                for tobj in other.get_aabb_triangles(marker="other"): 
                    overlaps_narrow = narrow_tree.query_overlaps(tobj)                    
                    #print ('obj',tobj.get_aabb())
                    #print ('obj obj overlaps',overlaps_narrow)
                
            olsum += len(overlaps)
        ax.text(3, 3, 4, "Overlaps: %d" % olsum)
        plt.savefig('/tmp/tetra/tetra_%02d.jpg' % i)
        plt.close(fig)
        plt.clf()
        #break
