from py_distmesh2d import *
import numpy as np
from cg import cg

def det2(A):
    '''Determinant of 2x2 matrix.'''
    return A[0,0] * A[1,1] - A[1,0] * A[0,1]

def inv2(A):
    '''Inverse of 2x2 matrix.'''
    d = det2(A)
    adj = np.array([[A[1,1], -A[0,1]], [-A[1,0], A[0,0]]])
    return adj / d

def poisson(f_rhs,f_dist,h0,pts,tri,*args,**kwargs):
    """Solve Poisson's equation on a domain D by the FE method:
         - Laplacian u = f_rhs
    on D and
         u = 0
    on boundary of D.   The right-hand side is  f = f_rhs(pts,*args).
    We use a triangulation described by points pts, triangles tri,
    a mesh scale h0, and a signed distance function f_dist(pts,*args);
    see py_distmesh2d.py.  Returns
       uh     = approximate solution value at pts
       inside = index of interior point (or -1 if not interior)
    See fem_examples.py for examples.
    """
    announce = kwargs.get('announce',False)
    geps = 0.001 * h0
    ii = (f_dist(pts, *args) < -geps)      # boolean array for interior nodes
    Npts = np.shape(pts)[0]     # = number of nodes
    N = ii.sum()                # = number of *interior* nodes
    if announce:
        print ("  poisson: assembling on mesh with  %d  nodes and  %d  interior nodes" \
            % (Npts,N))
    inside = np.zeros(Npts,dtype=np.int32) # index only the interior nodes
    count = 0
    for j in range(Npts):
        if ii[j]:
            inside[j] = count
            count = count + 1
        else:
            inside[j] = -1
    # eval f_rhs once for each node
    ff = np.zeros(Npts)
    for j in range(Npts):
        ff[j] = f_rhs(pts[j], *args)
    # loop over triangles to set up stiffness matrix A and load vector b
    # NOTE: not using sparse matrices at all
    A = np.zeros((N,N))
    b = np.zeros(N)
    for n in range(np.shape(tri)[0]):        # loop over triangles
        # indices, coordinates, and Jacobian of triangle
        j, k, l = tri[n,:]
        vj = inside[j]
        vk = inside[k]
        vl = inside[l]
        Jac = np.array([[ pts[k,0] - pts[j,0], pts[l,0] - pts[j,0] ],
                        [ pts[k,1] - pts[j,1], pts[l,1] - pts[j,1] ]])
        ar = abs(det2(Jac))/2.0
        C = ar/12.0
        Q = inv2(np.dot(Jac.transpose(),Jac))
        fT = np.array([ff[j], ff[k], ff[l]])
        # add triangle's contribution to linear system  A x = b
        if ii[j]:
            A[vj,vj] += ar * np.sum(Q)
            b[vj]    += C * np.dot(fT, np.array([2,1,1]))
        if ii[k]:
            A[vk,vk] += ar * Q[0,0]
            b[vk]    += C * np.dot(fT, np.array([1,2,1]))
        if ii[l]:
            A[vl,vl] += ar * Q[1,1]
            b[vl]    += C * np.dot(fT, np.array([1,1,2]))
        if ii[j] & ii[k]:
            A[vj,vk] -= ar * np.sum(Q[:,0])
            A[vk,vj] = A[vj,vk]
        if ii[j] & ii[l]:
            A[vj,vl] -= ar * np.sum(Q[:,1])
            A[vl,vj] = A[vj,vl]
        if ii[k] & ii[l]:
            A[vk,vl] += ar * Q[0,1]
            A[vl,vk] = A[vk,vl]
    if announce:
        print ("  poisson: solving linear system  A uh = b  using cg (N=%d unknowns)" % N)
    uh = np.zeros(Npts)
    # solve by cg including (weak) test of positive definiteness
    uh[ii], iters, r = cg(A,b,np.zeros(np.shape(b)),tol=1.0e-4,h=h0,test=True)
    #from numpy.linalg import solve
    #uh[ii] = solve(A,b)
    if (announce & (not(kwargs.get('getsys',False)))):
        if np.dot(b,b) > 0.0:
          err = np.sqrt(np.dot(r,r) / np.dot(b,b))
          print ("  poisson: cg did  %d  iterations to get |r|/|b| = %.4e" % (iters,err))
        else:
          print ("  poisson: cg did  %d  iterations" % iters)
    if kwargs.get('getsys',False):
        return uh, inside, A, b
    else:
        return uh, inside

