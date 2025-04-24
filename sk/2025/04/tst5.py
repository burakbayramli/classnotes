import sys; sys.path.append("randall")
import mpl_toolkits.mplot3d as a3, numpy as np
import os, pickle, matplotlib.pyplot as plt
from stl import mesh
import AABB, util

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
        for fr,to in list(util.pairwise(self.corners)):
            xs.append(fr[0]); ys.append(fr[1]); zs.append(fr[2])
            xs.append(to[0]); ys.append(to[1]); zs.append(to[2])        
        axx.plot(xs, ys, zs, 'red')

    def plot_box(self,axx):
        mins = np.min(self.corners,axis=0)
        maxs = np.max(self.corners,axis=0)
        x,y,z,w,h,d = list(mins) + list(maxs)
        util.plot_box_imp(x,y,z,w,h,d,axx)
        
class STLObj(AABB.IAABB):
    def __init__(self,offset):
        self.offset = offset
        self.init_triangles()

    def get_aabb_triangles(self):
        return [Triangle(t) for t in self.triangles]
        
    def init_triangles(self):
        m = mesh.Mesh.from_file('../../2020/08/shapes/Prism_hexagon.stl')
        self.triangles = m.vectors + self.offset
        
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
        return f"STLObj {self.offset}"

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
    sobjs = [STLObj(offset=np.array(o)) for o in offsets]

    tree = AABB.AABBTree(initial_size=4)    
    for t in sobjs: tree.insert_object(t)

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
        
        for j in range(len(sobjs)):
            sobjs[j].set_offset(sobjs[j].offset + dirs[j]*0.5)
            tree.update_object(sobjs[j])
            sobjs[j].plot(ax)

        # kesisme olan objeler icin
        for j in range(len(sobjs)):
            overlaps = tree.query_overlaps(sobjs[j])
            for other in overlaps:
                # A-B kesismesi tahmin edilen her B objesinin ucgenleri icin bir
                # aabb agaci yarat
                narrow_tree = AABB.AABBTree(initial_size=10)
                for x in other.get_aabb_triangles(): narrow_tree.insert_object(x)
                # A objesinin ucgenlerini alip B agacina kesisip kesismedigini sor
                for a_tri in sobjs[j].get_aabb_triangles(): 
                    overlaps_narrow = narrow_tree.query_overlaps(a_tri) 
                    for b_tri in overlaps_narrow:
                        print ('overlap')
                        b_tri.plot(ax)
                        # burada a_tri ile b_tri arasinda nihai
                        # kesisme noktasi bulunabilir
                        print (type(a_tri))
                        print (type(b_tri))
                            
                        
                
            olsum += len(overlaps)
        ax.text(45, 0, 35, "Overlaps: %d" % olsum)
        ax.set_xlabel("x axis")
        ax.set_ylabel("y axis")
        ax.set_zlabel("z axis")
        plt.savefig('/tmp/coll/coll_%02d.jpg' % i)
        plt.close(fig)
        plt.clf()
        #break
