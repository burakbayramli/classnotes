import numpy as np

def ip(x,y):
    return np.dot(x,y)

def sym_pos_def(A,**kwargs):
    strong = kwargs.get('strong',False)
    m = np.shape(A)[0]
    if m != np.shape(A)[1]:        # necessary: square
        return False
    for j in range(m):
        if any(A[j,:] != A[:,j]):  # necessary: symmetric
            return False
        if A[j,j] <= 0.0:          # necessary: positive diagonal
            return False
    if strong:
        # FIXME: run cholesky
        raise Exception("strong test for pos. def. not implemented")
    else:
        # random test with 3 vectors
        for k in range(3):         # necessary: x^T A x > 0 if x^T x > 0
            x = np.random.randn(m)
            if np.dot(x,np.dot(A,x)) <= 0.0:
                return False
    return True

def cg(A, f, u0, **kwargs):
    """Conjugate gradient for
       A u = f,  A symmetric and positive definite
    without preconditioning. Call
       u, k, r = cg(A, f, u0)
    with u0 an initial estimate, returns
       u = approximate solution
       k = number of iterations
       r = final residual."""
    if kwargs.get('test',False):
        assert sym_pos_def(A)
    h = kwargs.get('h',1.0)              # scale stopping criterion a la FEM
    maxiter = kwargs.get('maxiter',100)
    tol = kwargs.get('tol',1.0e-7)
    u = u0.astype(float)
    r = f.astype(float) - np.dot(A,u)
    rr = ip(r,r)
    normr0 = np.sqrt(rr)
    if normr0 == 0.0:
        return u0, 0, r
    p = r
    for k in range(maxiter):
        Ap = np.dot(A,p)
        alpha = rr / ip(Ap,p)
        u = u + alpha * p
        rnew = r - alpha * Ap
        rrnew = ip(rnew,rnew)
        normrnew = np.sqrt(rrnew)
        if (normrnew / normr0) < tol * h**2:
            return u, k, r
        beta = rrnew / rr
        p = rnew + beta * p
        r = rnew
        rr = rrnew
    print ('WARNING: maximum number of iterations (=%d) reached ...' % maxiter)
    return u, k, r

if __name__ == '__main__':
    A = np.array([[2, -1, 0], [-1, 2, -1], [0, -1, 2]])
    print (sym_pos_def(A))
    f = np.array([-3, 2, 1])             # = A * [[-1],[1],[1]]
    uexact = np.array([-1,1,1])
    u, _, _ = cg(A,f,np.array([0,0,0]),test=True,maxiter=5)
    du = uexact - u
    print ("difference from exact solution:  %.4e" % np.sqrt(ip(du,du)))
