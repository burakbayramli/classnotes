# -*- coding: utf-8 -*-
"""
Created on Tue Apr 15 08:00:10 2014

@author: admin
"""

import numpy as np
from scipy.io import loadmat
import scipy.linalg as slin
import time

def lp_acent(A,b,c,x_0):
    """solve problem
    minimize c'*X-sum(log(x))
    """
    #Parameters
    b = b.flatten()
    c = c.flatten()
    ALPHA = 0.01
    BETA = 0.5
    EPSILON = 1e-6
    MAXITERS = 100
    if (np.min(x_0)<=0) and (np.linalg.norm>1e-3):
        print ('failed' )
        return 0
    #m = len(b)
    #n = len(x_0)
    lambda_hist = []
    x = x_0
    for iter in range(MAXITERS):
       # H = np.diag(1/np.power(x,3))
        g = c-np.power(x,-1)
        #print g.shape
        #solving KKT system
        w = np.linalg.solve(np.dot(np.dot(A,np.diag(np.power(x,2))),A.T),
                            np.dot(np.dot(-A,np.diag(np.power(x,2))),g))
        dx = np.dot(-np.diag(np.power(x,2)),np.dot(A.T,w)+g)
        lambdasqr = np.dot(-g.T,dx) #dx'*T*dx: newton incremental
        lambda_hist.append(lambdasqr/2)
        if lambdasqr/2 <= EPSILON:
            break
        # backtracking line search
        t  = 1
        # brin the point inside the domain
        while np.min(x+t*dx)<=0:
            t =BETA*t
        while np.dot(c.T,np.dot(t,dx))-np.sum(np.log(x+t*dx))+np.sum(np.log(x))-ALPHA*t*np.dot(g.T,dx)>0:
            t = BETA*t
        x = x+t*dx
    if iter == MAXITERS:
        print ('ERROR: MAXITERS reached')
    else:
        #plt.figure()
        #plt.plot(range(len(lambda_hist)),lambda_hist,'b-',range(len(lambda_hist)),lambda_hist,'bo')
        return x,w,lambda_hist
        
def lp_barrier(A,b,c,x_0):
    #solving standard form LP
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
    return x_star,nu_star,history,gap
    
    
def lp_solve(A,b,c):
    #solving LP without initial values
    m,n = A.shape
    nsteps = np.zeros((2,1))
    #phase I    
    x_0 = np.linalg.lstsq(A,b)[0]
    print ('x0',x_0)
    print (np.min(x_0))
    t0 = 2+np.max([0,-np.min(x_0)])
    print ('t0',t0)
    A1 = np.hstack((A,np.dot(-A,np.ones((n,1)))))
    b1 = b-np.dot(A,np.ones((n,1)))
    z0 = x_0+t0*np.ones((n,1))-np.ones((n,1))
    c1 = np.vstack((np.zeros((n,1)),1))
    #print 'c1 = ',c1.shape
    z_star,nu_star,history,gap = lp_barrier(A1,b1,c1,np.vstack((z0,t0)))
    if z_star[n]>=1:
        print ('Problem is infeasible')
        x_star = []
        p_star = np.inf;
        status = 'infeasible'
        nsteps[0] = np.sum(np.array(history[0]))
        gap = []
        return x_star,p_star,gap,status,nsteps
    print ('Feasible point found')
    nsteps[0] = np.sum(np.array(history[0]))
    x_0 = z_star[0:n]-(z_star[n]*np.ones((n,1))).flatten()+np.ones((n,1)).flatten()
    #phase II
    x_star,nu_star,history,gap = lp_barrier(A,b,c,x_0)
    status ='Solved'
    p_star = np.dot(c.T,x_star)
    nsteps[1]=np.sum(history[0])
    return x_star,p_star,gap,status,nsteps
