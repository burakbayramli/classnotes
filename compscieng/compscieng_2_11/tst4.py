import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from scipy import ndimage

def bilinear_interpolate(im, x, y):
    x = np.asarray(x)
    y = np.asarray(y)

    x0 = np.floor(x).astype(int)
    x1 = x0 + 1
    y0 = np.floor(y).astype(int)
    y1 = y0 + 1

    x0 = np.clip(x0, 0, im.shape[1]-1);
    x1 = np.clip(x1, 0, im.shape[1]-1);
    y0 = np.clip(y0, 0, im.shape[0]-1);
    y1 = np.clip(y1, 0, im.shape[0]-1);

    Ia = im[ y0, x0 ]
    Ib = im[ y1, x0 ]
    Ic = im[ y0, x1 ]
    Id = im[ y1, x1 ]

    wa = (x1-x) * (y1-y)
    wb = (x1-x) * (y-y0)
    wc = (x-x0) * (y1-y)
    wd = (x-x0) * (y-y0)

    return wa*Ia + wb*Ib + wc*Ic + wd*Id

def grad(M, bound="sym", order=1):
    """
        grad - gradient, forward differences
        
          [gx,gy] = grad(M, options);
        or
          g = grad(M, options);
        
          options.bound = 'per' or 'sym'
          options.order = 1 (backward differences)
                        = 2 (centered differences)
        
          Works also for 3D array.
          Assme that the function is evenly sampled with sampling step 1.
        
          See also: div.
        
          Copyright (c) Gabriel Peyre
    """    


    # retrieve number of dimensions
    nbdims = np.ndim(M)
    
    
    if bound == "sym":  
        nx = np.shape(M)[0]
        if order == 1:
            fx = M[np.hstack((np.arange(1,nx),[nx-1])),:] - M
        else:
            fx = (M[np.hstack((np.arange(1,nx),[nx-1])),:] - M[np.hstack(([0],np.arange(0,nx-1))),:])/2.
            # boundary
            fx[0,:] = M[1,:]-M[0,:]
            fx[nx-1,:] = M[nx-1,:]-M[nx-2,:]
            
        if nbdims >= 2:
            ny = np.shape(M)[1]
            if order == 1:
                fy = M[:,np.hstack((np.arange(1,ny),[ny-1]))] - M
            else:
                fy = (M[:,np.hstack((np.arange(1,ny),[ny-1]))] - M[:,np.hstack(([0],np.arange(ny-1)))])/2.
                # boundary
                fy[:,0] = M[:,1]-M[:,0]
                fy[:,ny-1] = M[:,ny-1]-M[:,ny-2]
    
        if nbdims >= 3:
            nz = np.shape(M)[2]
            if order == 1:
                fz = M[:,:,np.hstack((np.arange(1,nz),[nz-1]))] - M
            else:
                fz = (M[:,:,np.hstack((np.arange(1,nz),[nz-1]))] - M[:,:,np.hstack(([0],np.arange(nz-1)))])/2.
                # boundary
                fz[:,:,0] = M[:,:,1]-M[:,:,0]
                fz[:,:,ny-1] = M[:,:,nz-1]-M[:,:,nz-2]            
    else:
        nx = np.shape(M)[0]
        if order == 1:
            fx = M[np.hstack((np.arange(1,nx),[0])),:] - M
        else:
            fx = (M[np.hstack((np.arange(1,nx),[0])),:] - M[np.hstack(([nx-1],np.arange(nx-1))),:])/2.
            
        if nbdims >= 2:
            ny = np.shape(M)[1]
            if order == 1:
                fy = M[:,np.hstack((np.arange(1,ny),[0]))] - M
            else:
                fy = (M[:,np.hstack((np.arange(1,ny),[0]))] - M[:,np.hstack(([ny-1],np.arange(ny-1)))])/2.
        
        if nbdims >= 3:
            nz = np.shape(M)[2]
            if order == 1:
                fz = M[:,:,np.hstack((np.arange(1,nz),[0]))] - M
            else:
                fz = (M[:,:,np.hstack((np.arange(1,nz),[0]))] - M[:,:,np.hstack(([nz-1],np.arange(nz-1)))])/2.   
   
    if nbdims==2:
        fx = np.concatenate((fx[:,:,np.newaxis],fy[:,:,np.newaxis]), axis=2)
    elif nbdims==3:
        fx = np.concatenate((fx[:,:,:,np.newaxis],fy[:,:,:,np.newaxis],fz[:,:,:,np.newaxis]),axis=3)
    
    return fx

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
    pstart = np.transpose(np.array([x0]))
    [D,Dsvg,Ssvg] = perform_dijstra_fm(W, pstart, np.inf,'fm', 'sym',n*6)
    # display
    k = 8
    displ = lambda D: np.cos(2*np.pi*k*D/ max(D.flatten()))
    plt.figure()
    imageplot(displ(D))
    plt.set_cmap('jet')
    plt.savefig('out-560.png')
    return D


def exo4(tau,x0,x1,G):
    """
    Perform the full geodesic path extraction by iterating the gradient
    descent. You must be very careful when the path become close to
    $x_0$, because the distance function is not differentiable at this
    point. You must stop the iteration when the path is close to $x_0$.
    """
    n = G.shape[0]
    Geval = lambda G,x: bilinear_interpolate(G[:,:,0], np.imag(x), np.real(x) ) + 1j * bilinear_interpolate(G[:,:,1],np.imag(x), np.real(x))
    niter = 1.5*n/tau;
    # init gamma
    gamma = [x1]
    xtgt = x0[0] + 1j*x0[1]
    for i in np.arange(0,niter):
        g = Geval(G, gamma[-1] )
        gamma.append( gamma[-1] - tau*g )
        if abs(gamma[-1]-xtgt)<1:
            break
    gamma.append( xtgt )
    return gamma


n = 100
x = np.linspace(-1, 1, n)
[Y, X] = np.meshgrid(x, x)
sigma = .2
W = 1 + 8 * np.exp(-(X**2 + Y**2)/ (2*sigma**2))
x0 = [round(.1*n), round(.1*n)]
D = exo3(x0,W)

G0 = grad(D)
d = np.sqrt(np.sum(G0**2, axis=2))
U = np.zeros((n,n,2))
U[:,:,0] = d
U[:,:,1] = d
G = G0 / U
tau = .8
x1 = round(.9*n) + 1j*round(.88*n)
gamma = [x1]

Geval = lambda G,x: bilinear_interpolate(G[:,:,0], np.imag(x), np.real(x) ) + 1j * bilinear_interpolate(G[:,:,1],np.imag(x), np.real(x))
g = Geval(G, gamma[-1] )
gamma.append( gamma[-1] - tau*g )
gamma = exo4(tau,x0,x1,G)

imageplot(W) 
plt.set_cmap('gray')
h = plt.plot(np.imag(gamma), np.real(gamma), '.b', linewidth=2)
h = plt.plot(x0[1], x0[0], '.r', markersize=20)
h = plt.plot(np.imag(x1), np.real(x1), '.g', markersize=20)
plt.savefig('out-760.png')

