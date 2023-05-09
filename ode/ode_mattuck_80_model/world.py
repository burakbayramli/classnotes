# World population prediction 
# Logistic growth, Hill function
# Dunya nufus artisi, lojistik denklem
import numpy as np
from scipy import optimize
import matplotlib.pyplot as plt

def f(t,A,k,K):
    return K / (1+A*np.exp(-k*t))    

def resid(p, y, t):
    A,k,K = p
    return y - f(t,A,k,K)

if __name__ == '__main__':
    t, x1 = np.loadtxt('world.txt', unpack=True)
    t = np.linspace(0,1,len(t))
    print ("len t", len(t))
    
    A0,k0,K0 = 1, 1, 1
    [A,k,K], flag  = optimize.leastsq(resid, [A0,k0,K0], args=(x1, t))

    print ("flag",flag)
    print ("A", A)
    print ("k",k)
    print ("K",K)
        
    ahead = 60.
    res = K / (1.+A*np.exp(-k*(1.+(ahead/len(t)))))
    print (res)
    
    plt.plot(t, x1, 'ro')
    t = np.linspace(0,4,len(t))    
    plt.plot(t, f(t,A,k,K), 'go')    
    
    plt.show()
