import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from numpy import *
from scipy import ndimage

def imageplot(f, str='', sbpt=[]):
    """
        Use nearest neighbor interpolation for the display.
    """
    if sbpt != []:
        plt.subplot(sbpt[0], sbpt[1], sbpt[2])
    imgplot = plt.imshow(f, interpolation='nearest')
    imgplot.set_cmap('gray')
    plt.axis('off')

def perform_dijstra_fm(W, pstart, niter=np.inf, method='dijstr', bound='sym', svg_rate=10):
    n = W.shape[0]
    neigh = np.array([[1, -1, 0, 0], [0, 0,  1, -1]])

    def symmetrize(x,n):
        if (x<0):
            x = -x;
        elif (x>=n):
            x = 2*(n-1)-x
        return x

    if bound=='per':
        boundary = lambda x: np.mod(x,n)
    else:
        boundary = lambda x: [symmetrize(x[0],n), symmetrize(x[1],n)] # todo

    ind2sub1 = lambda k: [int( (k-np.fmod(k,n))/n ), np.fmod(k,n)]
    sub2ind1 = lambda u: int( u[0]*n + u[1] )
    Neigh = lambda k,i: sub2ind1(boundary(ind2sub1(k) + neigh[:,i]))
    extract   = lambda x,I: x[I]
    extract1d = lambda x,I: extract(x.flatten(),I)

    nstart = pstart.shape[1]
    I = list( np.zeros( (nstart, 1) ) )
    for i in np.arange(0, nstart):
        I[i] = int( sub2ind1(pstart[:,i]) )

    D = np.zeros( (n,n) ) + np.inf # current distance
    for i in np.arange(0, nstart):
        D[pstart[0,i],pstart[1,i]] = 0

    S = np.zeros( (n,n) )
    for i in np.arange(0, nstart):
        S[pstart[0,i],pstart[1,i]] = 1 # open

    iter = 0
    q = 100  # maximum number of saves
    Dsvg = np.zeros( (n,n,q) )
    Ssvg = np.zeros( (n,n,q) )
    while ( not(I==[]) & (iter<=niter) ):
        iter = iter+1;
        if iter==niter:
            break
        j = np.argsort( extract1d(D,I)  )
        if np.ndim(j)==0:
            j = [j]
        j = j[0]
        i = I[j]
        a = I.pop(j)
        u = ind2sub1(i);
        S[u[0],u[1]] = -1
        J = []
        for k in np.arange(0,4):
            j = Neigh(i,k)
            if extract1d(S,j)!=-1:
                J.append(j)
                if extract1d(S,j)==0:
                    u = ind2sub1(j)
                    S[u[0],u[1]] = 1
                    I.append(j)
        DNeigh = lambda D,k: extract1d(D,Neigh(j,k))
        for j in J:
            dx = min(DNeigh(D,0), DNeigh(D,1))
            dy = min(DNeigh(D,2), DNeigh(D,3))
            u = ind2sub1(j)
            w = extract1d(W,j);
            if method=='dijstr':
                D[u[0],u[1]] = min(dx + w, dy + w)
            else:
                Delta = 2*w - (dx-dy)**2
                if (Delta>=0):
                    D[u[0],u[1]] = (dx + dy + np.sqrt(Delta))/ 2
                else:
                    D[u[0],u[1]] = min(dx + w, dy + w)
        t = iter/svg_rate
        if (np.mod(iter,svg_rate)==0) & (t<q):
            print (t)
            Dsvg[:,:,int(t-1)] = D
            Ssvg[:,:,int(t-1)] = S

    Dsvg = Dsvg[:,:,:int(t-1)]
    Ssvg = Ssvg[:,:,:int(t-1)]
    return (D,Dsvg,Ssvg);

def exo3(x0,W):
    """
    Compute the distance map to these starting point using the FM algorithm.
    """
    n = W.shape[0]
    pstart = transpose(array([x0]))
    [D,Dsvg,Ssvg] = perform_dijstra_fm(W, pstart, inf,'fm', 'sym',n*6)
    # display
    k = 8
    displ = lambda D: cos(2*pi*k*D/ max(D.flatten()))
    plt.figure()
    imageplot(displ(D))
    plt.set_cmap('jet')
    plt.savefig('out-560.png')
    return D


n = 100
x = linspace(-1, 1, n)
[Y, X] = meshgrid(x, x)
sigma = .2
W = 1 + 8 * exp(-(X**2 + Y**2)/ (2*sigma**2))
imageplot(W)
plt.savefig('out-520.png')

x0 = [round(.1*n), round(.1*n)]
exo3(x0,W)



