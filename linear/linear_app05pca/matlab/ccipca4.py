
# Author: Kevin Hughes <kevinhughes27@gmail.com>
# License: BSD Style.

import scipy.sparse as sp
import numpy as np
from scipy import linalg as la
import scipy.sparse as sps
from sklearn import datasets

class CCIPCA:    
    def __init__(self, n_components=2, amnesic=2.0, copy=True):
        self.n_components = n_components            
        self.copy = copy
        self.amnesic = amnesic
        self.iteration = 0
        self.n_samples=0
        self.n_features = 0
        self.mean_ = None
        self.components_ = None

      
    def partial_fit(self, u):
        n = float(self.iteration)
        V = self.components_
        
        # amnesic learning params
        if n <= int(self.amnesic):
            w1 = float(n+2-1)/float(n+2)    
            w2 = float(1)/float(n+2)    
        else:
            w1 = float(n+2-self.amnesic)/float(n+2)    
            w2 = float(1+self.amnesic)/float(n+2)

        # update mean
        self.mean_ = w1*self.mean_ + w2*u

        # mean center u        
        u = u - self.mean_

        # update components
        for j in range(0,self.n_components):
            
            if j > n:
                # the component has already been init to a zerovec
                pass
            
            elif j == n:
                # set the component to u 
                V[j,:] = u
            else:       
                # update the components
                V[j,:] = w1*V[j,:] + w2*np.dot(u,V[j,:])*u / la.norm(V[j,:])
                
                normedV = V[j,:] / la.norm(V[j,:])
            
                u = u - np.dot(np.dot(u.T,normedV),normedV)

        self.iteration += 1
        self.components_ = V
            
        return

    
# Load data
iris = datasets.load_iris()
X = iris.data
y = iris.target

k = 2
ccipca = CCIPCA(n_components=k)
ccipca.n_samples=X.shape[0]
ccipca.n_features = X.shape[1]
ccipca.mean_ = np.zeros([ccipca.n_features], np.float)
#ccipca.components_ = np.zeros([ccipca.n_components,ccipca.n_features], np.float)
ccipca.components_ = np.ones((k,ccipca.n_features)) / (ccipca.n_features*k)
                    
#X = sps.lil_matrix(X)
#for i in range(X.shape[0]): ccipca.partial_fit(X[i,:])

for i in range(X.shape[0]): ccipca.partial_fit(X[i,:])

print ccipca.components_

'''
[[ 0.36158968 -0.08226889  0.85657211  0.35884393]
 [ 0.65653988  0.72971237 -0.1757674  -0.07470647]]
[[ 0.81484887  0.52092446  0.25321053  0.02331202]
 [ 0.09304124 -0.38124227  0.86734622  0.30611795]]
'''
