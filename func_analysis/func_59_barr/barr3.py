import numpy as np

def lp_solve(A,b,c):
    #solving LP without initial values
    m,n = A.shape
    nsteps = np.zeros((2,1))
    x0 = np.linalg.lstsq(A,b)[0]
    t0 = 2+np.max([0,-np.min(x0)])
    print (x0, t0)
    if np.min(x0)>0.0:
        print ('Ax=b solution already feasible')
        x_0 = x0
    else:
        print ("phase I")
        A1 = np.hstack((A,np.dot(-A,np.ones((n,1)))))
        b1 = b-np.dot(A,np.ones((n,1)))
        z0 = x0+t0*np.ones((n,1))-np.ones((n,1))
        c1 = np.vstack((np.zeros((n,1)),1))
        arg = np.vstack((z0,t0))
        print (arg)

