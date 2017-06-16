import scipy.sparse as sp
import numpy as np
from scipy import linalg as la
import scipy.sparse as sps
from sklearn import datasets

class CCIPCA:    
    def __init__(self, n_components, n_features, amnesic=2.0, copy=True):
        self.n_components = n_components
        self.n_features = n_features
        self.copy = copy
        self.amnesic = amnesic
        self.iteration = 0
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
            
            if j > n: pass            
            elif j == n: V[j,:] = u
            else:       
                # update the components
                V[j,:] = w1*V[j,:] + w2*np.dot(u,V[j,:])*u / la.norm(V[j,:])
                normedV = V[j,:] / la.norm(V[j,:])
                normedV = normedV.reshape((self.n_features, 1))
                tmp1 = np.dot(u,normedV)
                tmp2 = np.dot(tmp1,normedV.T)
                u = u - tmp2
                u = u
                

        self.iteration += 1
        self.components_ = V / la.norm(V)
            
        return

    def post_process(self):
        
        self.explained_variance_ratio_ = np.sqrt(np.sum(self.components_**2,axis=1))

        # sort by explained_variance_ratio_
        idx = np.argsort(-self.explained_variance_ratio_)
        self.explained_variance_ratio_ = self.explained_variance_ratio_[idx]
        self.components_ = self.components_[idx,:]

        # re-normalize
        self.explained_variance_ratio_ = (self.explained_variance_ratio_ / self.explained_variance_ratio_.sum())

        for r in range(0,self.components_.shape[0]):
            self.components_[r,:] /= np.sqrt(np.dot(self.components_[r,:],self.components_[r,:]))

    
    
# Load data
iris = datasets.load_iris()
X = iris.data
y = iris.target

ccipca = CCIPCA(n_components=2,n_features=X.shape[1])
ccipca.mean_ = np.zeros([ccipca.n_features], np.float)
#ccipca.components_ = np.zeros([ccipca.n_components,ccipca.n_features], np.float)
ccipca.components_ = np.ones((ccipca.n_components,ccipca.n_features)) / (ccipca.n_features*ccipca.n_components)
                    
#X = sps.lil_matrix(X)
#for i in range(X.shape[0]): ccipca.partial_fit(X[i,:])
for i in range(X.shape[0]): ccipca.partial_fit(X[i,:])
ccipca.post_process()

print ccipca.components_

'''
[[ 0.36158968 -0.08226889  0.85657211  0.35884393]
 [ 0.65653988  0.72971237 -0.1757674  -0.07470647]]
[[ 0.81484887  0.52092446  0.25321053  0.02331202]
 [ 0.09304124 -0.38124227  0.86734622  0.30611795]]
'''
