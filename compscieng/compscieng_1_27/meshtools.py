from pylab import triplot, tripcolor, axis, axes, text
from py_distmesh2d import *
import numpy as np

__all__ = ["fixmesh", "longedgelist", "edgelist", "bdyrefine", "plotmesh"]

# fixmesh by C. Khroulev based on Matlab version by M. Truffer
def fixmesh(pts, tri):
    # find doubles
    doubles = []
    N = pts.shape[0]
    for i in range(N):
        for j in range(i+1,N):
            if np.linalg.norm(pts[i] - pts[j]) == 0:
                doubles.append(j)
    # remove doubles
    while len(doubles) > 0:
        j = doubles.pop()
        # remove a double
        pts = np.vstack([pts[0:j], pts[j+1:]])
        # update all triangles that reference points after the one removed
        for k in xrange(tri.shape[0]):
            for l in xrange(3):
                if tri[k, l] > j:
                    tri[k, l] -= 1
    # check (and fix) node order in triangles
    for k in range(tri.shape[0]):
        a = pts[tri[k, 0]]
        b = pts[tri[k, 1]]
        c = pts[tri[k, 2]]
        if np.cross(b - a, c - a) > 0:
            tri[k, 2], tri[k, 1] = tri[k, 1], tri[k, 2]
    return pts, tri

# longedgelist by E. Bueler based on Matlab version by D. Maxwell
def longedgelist(pts, tri):
    """From a triangulation (pts,tri) create a 'long' list of edges, with
    redundancies.

    in:  pts = list of points (nx2)
         tri = list indices into pts defining a triangulation (Nx3)
    out: longe = a sorted list of edges with three entries per triangle.

    For each edge in each triangle, longe has a row of indices
         [ p0, p1, f, e ]
    where p0 < p1 = indices of the endpoints,
          f       = index into tri that this edge came from, and
          e       = {0,1,2} for which of the three edges it is.
    The shape of longe is  (m,4)  where m = 3N if N is the number of triangles.

    Sorting: longe is sorted in increasing order by p0, and then in increasing
    order by p1 if p0 are equal.

    Example:  If  tri[42,:] = [ 3, 9, 7 ]  then we will put these
    three entries in the long edge list:
        ...
        [ 3 7 42 2 ]
        [ 3 9 42 0 ]
        ...
        [ 7 9 42 1 ]
        ...
    """
    N = np.shape(tri)[0]
    longe = np.zeros((N*3,4),dtype=np.int32)
    for i in range(N):
        for j in range(3):
            M = tri[i,j]
            m = tri[i,np.mod(j+1,3)]
            if M < m:
                M, m = m, M
            eindex = 3*i + j
            longe[eindex] = [m, M, i, j]
    # sort the long edge list so that p0 increases
    longe = longe[longe[:,0].argsort(),]
    # for equal p0, sort so that p1 increases
    for i in range(np.shape(pts)[0]):
        tmp = longe[longe[:,0]==i,:]
        if len(tmp)>0:   # in some cases p0 may not be a first point value
            longe[longe[:,0]==i] = tmp[tmp[:,1].argsort(),]
    return longe

# edgelist by E. Bueler based on Matlab version by D. Maxwell
def edgelist(pts, tri):
    """From a triangulation (pts,tri) extract a list of unique edges.  Also get
    a list of the triangles by their edges.
    in:  pts = list of points (nx2)
         tri = list indices into pts defining a triangulation (Nx3)
    out: e   = for each edge a list of indices into p defining the
               endpoints of the edge (mx2)
         te  = for each triangle in the triangulation a list of indices
               into e of the associated edges (Nx3)
    """
    longe = longedgelist(pts,tri)
    tmp = longe[:,[0,1]]
    d = ( np.sum(abs(np.diff(tmp,axis=0)),axis=1) != 0 ) # Tag unique elements
    d = np.concatenate(([True],d))
    e = longe[d][:,[0,1]]      # extract unique elements

    te = np.zeros(np.shape(tri),dtype=np.int32)
    ecum = np.cumsum(d) - 1
    for k in range(np.shape(longe)[0]):
       te[longe[k,2],longe[k,3]] = ecum[k]
    return e, te

# bdyrefine by E. Bueler based on Matlab version by D. Maxwell and J. Brown
def bdyrefine(pts,tri,f_dist,h0,*args):
    """Refines a triangulation using edge bisection while also adjusting
    boundary points to lie on a domain boundary.
    in:  pts    = location of points (nx2)
         tri    = matrix of indices into pts defining a triangulation (Nx3)
         f_dist = distance function a-la distmesh2d
         h0     = scale for coarse (input) mesh
    out: rp     = an augmented list of points in a refined triangulation.
                  The first N points will be the same as those in pts,
                  possibly shifted at the boundary
         rt     = a list of triangles in the refined triangulation
         e      = edgelist used for refinement
         ind    = logical mask for interior elements of rp (?)
    """
    e, te = edgelist(pts,tri)
    N = np.shape(pts)[0]
    rp = np.zeros( (N + np.shape(e)[0], 2) )
    rp[:N] = pts.copy()
    # add new points to the end of the point list
    rp[N:] = 0.5 * (pts[e[:,0],:] + pts[e[:,1],:])
    # find boundary edges by traversing edges and counting
    bdy_count = np.zeros(np.shape(e)[0],dtype=np.int32)
    for i in range(np.shape(te)[0]):
        for j in range(3):
            bdy_count[te[i,j]] += 1
    bdy_edge  = (bdy_count == 1)
    ind       = (bdy_count == 2)
    # a boundary point on the fine grid is either an endpoint of a coarse
    # grid boundary edge or a midpoint of a coarse grid boundary edge
    bdy_point = np.zeros(np.shape(rp)[0],dtype=np.bool)   # filled w False
    bdy_point[ e[bdy_edge,0] ] = True
    bdy_point[ e[bdy_edge,1] ] = True
    bdy_point[N:] = bdy_edge
    # construct 4 triangles for every triangle in the original mesh
    rt = np.zeros( (4*np.shape(tri)[0],3), dtype=np.int32 )
    for i in range(np.shape(tri)[0]):
        rt[4*i,  :] = [ N+te[i,2], tri[i,0],  N+te[i,0] ]
        rt[4*i+1,:] = [ N+te[i,0], tri[i,1],  N+te[i,1] ]
        rt[4*i+2,:] = [ N+te[i,1], tri[i,2],  N+te[i,2] ]
        rt[4*i+3,:] = [ N+te[i,0], N+te[i,1], N+te[i,2] ]
    # use numerical gradient technique stolen from distmesh2d to project
    # boundary points onto the boundary
    eps = 2.0e-16
    deps = np.sqrt(eps) * h0;  # distance used in finite differencing
    xy = rp[bdy_point,:]
    fdxy = f_dist(xy,*args)
    dx = deps * np.ones(np.shape(xy))
    dy = dx.copy()
    dx[:,1] = 0.0
    dy[:,0] = 0.0
    dfgrad = np.zeros(np.shape(xy))
    dfgrad[:,0] = fdxy * ( ( f_dist(xy+dx,*args) - fdxy ) / deps )
    dfgrad[:,1] = fdxy * ( ( f_dist(xy+dy,*args) - fdxy ) / deps )
    rp[bdy_point,:] = xy - dfgrad
    return rp, rt, e, ind

# plotmesh by C. Khroulev and E. Bueler
def plotmesh(pts, tri, *args, **kwargs):
    showindex = kwargs.get('index',False)
    mylinewidth = kwargs.get('lw',2.0)
    h0 = kwargs.get('h0',0.0)
    edges = kwargs.get('edges',np.array([]))
    if len(args) > 0:
        tripcolor(pts[:,0], pts[:,1], tri, args[0], edgecolor='black', cmap="Blues", lw=mylinewidth)
    else:
        triplot(pts[:,0], pts[:,1], tri, "k-", lw=mylinewidth)
    if showindex:
        dx = h0/10.0
        for i in range(np.shape(tri)[0]):   # label triangle index in green
            x = np.sum(pts[tri[i,:],0]) / 3.0
            y = np.sum(pts[tri[i,:],1]) / 3.0
            text(x,y,'%d' % i, color='g')
        for j in range(np.shape(pts)[0]):   # label point index in blue
            text(pts[j,0]+dx,pts[j,1]+dx,'%d' % j, color='b')
        if len(edges) > 0:
            for k in range(np.shape(edges)[0]):   # label edge index in red
                x = np.sum(pts[edges[k,:],0]) / 2.0
                y = np.sum(pts[edges[k,:],1]) / 2.0
                text(x+dx,y-dx,'%d' % k, color='r')
    axis('tight')
    axis('equal')
