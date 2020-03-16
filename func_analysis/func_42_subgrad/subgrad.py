import scipy.io as sio
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import numpy.linalg as lin

def subgrad_func(A,b,lam):
    n2 = A.shape[1]
    x = np.zeros((n2,1))
    k=1
    g = np.ones((n2,1))
    t = 0.01
    f = []
    while True:
        if k>3: 
            crit = np.abs(f[k-2]-f[k-3])/f[k-2]
            if crit < 1e-5: break
        tmp = 0.5*lin.norm(np.dot(A,x)-b,2)**2+lam*lin.norm(x,1);        
        print (tmp)
        f.append(tmp)
        s = x.copy()
        s[x>0]=1
        s[x<0]=-1
        if len(s[x==0])>0: 
            s[x==0] = -2*np.random.rand(len(x==0))+1
        g = np.dot(A.T,np.dot(A,x)-b) + lam*s
        x = x - t*g
        k = k+1
    return x
                
mat = sio.loadmat('A.mat'); A = mat['A']
mat = sio.loadmat('y.mat'); y = mat['y']

lam = 0.1;
x = subgrad_func(A,y.T,lam);
print (x)



