from scipy import ndimage
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

def imageplot(f, str='', sbpt=[]):
    if sbpt != []:
        plt.subplot(sbpt[0], sbpt[1], sbpt[2])
    imgplot = plt.imshow(f, interpolation='nearest')
    imgplot.set_cmap('gray')
    plt.axis('off')

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

    
