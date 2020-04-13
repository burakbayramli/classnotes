from numpy import *
from numpy.random import *

class PF:
    def __init__(self, K, n):
        self.H = append(K, [[0], [0], [0]], axis=1)
        self.n = n
        self.x = zeros((self.n, 4))
        self.x[:,:] = array([1., 1., 165., -1])
        
    def normalize_2d(self, x): 
        return array([x[0]/x[2], x[1]/x[2], 1.0])
        
    def resample(self, weights):
       n = len(weights)
       indices = []
       C = [0.] + [sum(weights[:i+1]) for i in range(n)]
       u0, j = random(), 0
       for u in [(u0+i)/n for i in range(n)]:
         while u > C[j]:
           j+=1
         indices.append(j-1)
       return indices
 
    def update(self, y):
        u = uniform(-0.1, -1, self.n) # forward with uncertainty
        self.x[:,2] += u
        u = uniform(-40,40, self.n) # left right uncertainty
        self.x[:,0] += u
        p = dot(self.x,self.H.T)
        for i, item in enumerate(p): # modify in place
            p[i,:] = self.normalize_2d(item)
        self.w  = 1./(1. + (y-p)**2)
        self.w = self.w[:,0]+self.w[:,1]
        #self.w = self.w[:,0]
        self.w /= sum(self.w)
        self.x  = self.x[self.resample(self.w),:]
        
    def average(self):
        return sum(self.x.T*self.w, axis=1)
            
