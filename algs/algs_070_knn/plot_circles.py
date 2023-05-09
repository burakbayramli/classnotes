from matplotlib.patches import Circle
import matplotlib.pyplot as plt
import numpy as np
import time
import dist

__rmin__ = 2

# node: [pivot, radius, points, [child1,child2]]
def new_node(): return  [None,None,None,[None,None]]

def circle(x,rad,ax):
    c = Circle([x[0], x[1]], rad, color='lightgreen')
    ax.add_patch(c)
    plt.xlim(-5,15)
    plt.ylim(-5,15)

def plot_points(pts,color,ax):
    for x in pts: ax.plot(x[0],x[1],color)

def plot_circles(pivot, radius, points, all_points):
    global i
    f = plt.figure()
    ax = f.gca()
    plot_points(all_points,'ko',ax)
    plot_points(points,'ro',ax)
    i += 1
    circle(pivot,radius,ax)
    plt.savefig('knn%s.png' % str(i))
    
    
def form_tree(points,node,all_points):
    pivot = points[0]
    radius = np.max(dist.dist(points,pivot))
    plot_circles(pivot, radius, points, all_points)
    
    node[0] = pivot
    node[1] = radius
    if len(points) <= __rmin__:
        node[2] = points
        return
    idx = np.argmax(dist.dist(points,pivot))
    furthest = points[idx,:]
    idx = np.argmax(dist.dist(points,furthest))
    furthest2 = points[idx,:]
    dist1=dist.dist(points,furthest)
    dist2=dist.dist(points,furthest2)
    diffs = dist1-dist2
    p1 = points[diffs <= 0]
    p2 = points[diffs > 0]
    node[3][0] = new_node() # left child
    node[3][1] = new_node() # right child
    form_tree(p1,node[3][0],all_points)
    form_tree(p2,node[3][1],all_points)
       
if __name__ == "__main__":
    i = 0
    points = np.array([[3.,4.],[5.,5.],[9.,2.],[3.2,5.],[7.,5.],
                       [8.,9.],[7.,6.],[8,4],[6,2]])
    tree = new_node()
    form_tree(points,tree,points)
    
