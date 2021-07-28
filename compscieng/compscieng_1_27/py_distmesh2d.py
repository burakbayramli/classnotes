from numpy import sqrt, sum, vstack
import numpy as np
from scipy.spatial import Delaunay

def delaunay(pts):
    return Delaunay(pts).vertices

def distmesh2d(fd, fh, h0, bbox, pfix, *args):
    """A re-implementation of the MATLAB distmesh2d function by Persson and Strang.

    See P.-O. Persson, G. Strang, A Simple Mesh Generator in MATLAB.
    SIAM Review, Volume 46 (2), pp. 329-345, June 2004

    and http://persson.berkeley.edu/distmesh/

    Parameters:
    ==========

    fd: a signed distance function, negative inside the domain
    fh: a triangle size function
    bbox: bounding box, [[x_min, x_max], [y_min, y_max]]
    pfix: fixed points, [[x1, y1], [x2, y2], ...]

    Extra arguments are passed to fd and fh.

    Returns
    =======

    p: list of points
    t: list of triangles (list of triples of indices in p)
    """
    # parameters
    dptol = 0.001; ttol = 0.1; Fscale = 1.2; deltat = 0.2;
    geps = 0.001 * h0; deps = sqrt(np.finfo(float).eps) * h0

    # create the initial point distribution:
    x, y = np.meshgrid(np.arange(bbox[0][0], bbox[0][1], h0),
                       np.arange(bbox[1][0], bbox[1][1], h0 * sqrt(3) / 2))

    x[1::2,:] += h0 / 2

    p = np.array((x.flatten(), y.flatten())).T

    # discard exterior points
    p = p[fd(p, *args) < geps]

    # decimate (rejection method) using triangle size function
    r0 = 1.0 / fh(p, *args)**2
    selection = np.random.rand(p.shape[0], 1) < r0 / r0.max()
    p = p[selection[:,0]]

    # add fixed points:
    if len(pfix) > 0:
        p = np.vstack((pfix, p))

    pold = np.zeros_like(p); pold[:] = np.inf
    Ftot = np.zeros_like(p)

    def triangulate(pts):
        """
        Compute the Delaunay triangulation and remove trianges with
        centroids outside the domain.
        """
        tri = np.sort(delaunay(pts), axis=1)
        pmid = sum(pts[tri], 1) / 3
        return tri[fd(pmid, *args) < -geps]

    while True:
        # check if it is time to re-compute the triangulation
        if sqrt(sum((p - pold)**2, 1)).max() > ttol:
            pold[:] = p[:]
            t = triangulate(p)
            # find unique edges of triangles
            bars = t[:, [[0,1], [1,2], [0,2]]].reshape((-1, 2))
            bars = np.unique(bars.view("i,i")).view("i").reshape((-1,2))

        barvec = p[bars[:,0]] - p[bars[:,1]]
        L = sqrt(sum(barvec**2, 1)).reshape((-1,1))
        hbars = fh((p[bars[:,0]] + p[bars[:,1]]) / 2.0, *args).reshape((-1,1))
        L0 = hbars * Fscale * sqrt(sum(L**2) / sum(hbars**2))

        # Compute forces for each bar:
        F = np.maximum(L0 - L, 0)
        Fvec = F * (barvec / L)

        # Sum to get total forces for each point:
        Ftot[:] = 0
        for j in range(bars.shape[0]):
            Ftot[bars[j]] += [Fvec[j], -Fvec[j]]

        # zero out forces at fixed points:
        Ftot[0:len(pfix), :] = 0.0

        # update point locations:
        p += deltat * Ftot

        # find points that ended up outside the domain and project them onto the boundary:
        d = fd(p, *args); ix = d > 0
        dgradx = (fd(vstack((p[ix,0] + deps, p[ix,1])).T, *args)        - d[ix]) / deps
        dgrady = (fd(vstack((p[ix,0],        p[ix,1] + deps)).T, *args) - d[ix]) / deps
        p[ix] -= vstack((d[ix] * dgradx, d[ix] * dgrady)).T

        # the stopping criterion:
        if (sqrt(sum((deltat * Ftot[d < -geps])**2, 1)) / h0).max() < dptol:
            break

    return p, triangulate(p)

def dcircle(pts, xc, yc, r):
    "Distance function for the circle centered at (xc, yc)."
    return sqrt((pts[:,0] - xc)**2 + (pts[:,1] - yc)**2) - r

def drectangle(pts, x1, x2, y1, y2):
    "Distance function for the rectangle (x1, x2) * (y1, y2)."
    return -np.minimum(np.minimum(np.minimum(-y1+pts[:,1], y2-pts[:,1]),
                                  -x1+pts[:,0]), x2-pts[:,0])

def ddiff(d1, d2):
    "Distance function for the difference of two sets."
    return np.maximum(d1, -d2)

def dintersect(d1, d2):
    "Distance function for the intersection of two sets."
    return np.maximum(d1, d2)

def dunion(d1, d2):
    "Distance function for the union of two sets."
    return np.minimum(d1, d2)

def huniform(pts, *args):
    "Triangle size function giving a near-uniform mesh."
    return np.ones((pts.shape[0], 1))

