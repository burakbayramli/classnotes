import numpy as np

def lp_acent(A,b,c,x_0):
    b = b.flatten()
    c = c.flatten()
    ALPHA = 0.01
    BETA = 0.5
    EPSILON = 1e-6
    MAXITERS = 100
    if (np.min(x_0)<=0) and (np.linalg.norm>1e-3):
        print ('failed' )
        return 0
    lambda_hist = []
    x = x_0
    for iter in range(MAXITERS):
        g = c-np.power(x,-1)
        w = np.linalg.solve(np.dot(np.dot(A,np.diag(np.power(x,2))),A.T),
                            np.dot(np.dot(-A,np.diag(np.power(x,2))),g))
        dx = np.dot(-np.diag(np.power(x,2)),np.dot(A.T,w)+g)
        lambdasqr = np.dot(-g.T,dx)
        lambda_hist.append(lambdasqr/2)
        if lambdasqr/2 <= EPSILON:
            break
        t  = 1
        while np.min(x+t*dx)<=0:
            t =BETA*t
        while np.dot(c.T,np.dot(t,dx))- \
              np.sum(np.log(x+t*dx))+np.sum(np.log(x))-\
              ALPHA*t*np.dot(g.T,dx)>0:
            t = BETA*t
        x = x+t*dx
    if iter == MAXITERS:
        print ('ERROR: MAXITERS reached')
    else:
        return x,w,lambda_hist
        
def lp_barrier(A,b,c,x_0):
    T_0 =1
    MU = 20
    EPSILON = 1e-3
    n = len(x_0)
    t = T_0
    x = x_0.flatten()
    history = []
    while True:
        x_star, nu_star,lambda_hist = lp_acent(A,b,t*c,x)
        x = x_star
        gap = n/t
        history.append(lambda_hist)
        if gap<EPSILON:
            break
        t = MU*t
    return x_star,nu_star,gap
    

def lp_solve(A,b,c):
    m,n = A.shape
    nsteps = np.zeros((2,1))
    x0 = np.linalg.lstsq(A,b)[0]
    t0 = 2+np.max([0,-np.min(x0)])
    if np.min(x0)>0.0:
        print ('Ax=b solution already feasible')
        x_0 = x0
    else:
        print ("phase I")
        A1 = np.hstack((A,np.dot(-A,np.ones((n,1)))))
        b1 = b-np.dot(A,np.ones((n,1)))
        z0 = x0+t0*np.ones((n,1))-np.ones((n,1))
        c1 = np.vstack((np.zeros((n,1)),1))
        z_star,nu_star,gap = lp_barrier(A1,b1,c1,np.vstack((z0,t0)))
        if z_star[n] >= 1:
            print ('Problem is infeasible')
            exit()
        x_0 = z_star[0:n]-(z_star[n]*np.ones((n,1))).flatten()+\
              np.ones((n,1)).flatten()

    x_star,nu_star,gap = lp_barrier(A,b,c,x_0)
    return x_star,gap,nsteps
