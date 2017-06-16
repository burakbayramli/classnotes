""" Incremental Principal Component Analysis
"""

# Author: Kevin Hughes <kevinhughes27@gmail.com>
# License: BSD Style.

import scipy.sparse as sp
import numpy as np
from scipy import linalg as la

from sklearn.base import BaseEstimator, TransformerMixin
#from sklearn.utils import array2d, as_float_array
from sklearn.utils.extmath import safe_sparse_dot


def _assert_all_finite(X):
    """Like assert_all_finite, but only for ndarray."""
    if (X.dtype.char in np.typecodes['AllFloat'] and not np.isfinite(X.sum())
            and not np.isfinite(X).all()):
        raise ValueError("Input contains NaN, infinity"
                         " or a value too large for %r." % X.dtype)


def array2d(X, dtype=None, order=None, copy=False, force_all_finite=True):
    """Returns at least 2-d array with data from X"""
    if sp.issparse(X):
        raise TypeError('A sparse matrix was passed, but dense data '
                        'is required. Use X.toarray() to convert to dense.')
    X_2d = np.asarray(np.atleast_2d(X), dtype=dtype, order=order)
    if force_all_finite:
        _assert_all_finite(X_2d)
    if X is X_2d and copy:
        X_2d = safe_copy(X_2d)
    return X_2d

def as_float_array(X, copy=True, force_all_finite=True):
    """Converts an array-like to an array of floats

    The new dtype will be np.float32 or np.float64, depending on the original
    type. The function can create a copy or modify the argument depending
    on the argument copy.

    Parameters
    ----------
    X : {array-like, sparse matrix}

    copy : bool, optional
        If True, a copy of X will be created. If False, a copy may still be
        returned if X's dtype is not a floating point type.

    Returns
    -------
    XT : {array, sparse matrix}
        An array of type np.float
    """
    if isinstance(X, np.matrix) or (not isinstance(X, np.ndarray)
                                    and not sp.issparse(X)):
        return safe_asarray(X, dtype=np.float64, copy=copy,
                            force_all_finite=force_all_finite)
    elif sp.issparse(X) and X.dtype in [np.float32, np.float64]:
        return X.copy() if copy else X
    elif X.dtype in [np.float32, np.float64]:  # is numpy array
        return X.copy('F' if X.flags['F_CONTIGUOUS'] else 'C') if copy else X
    else:
        return X.astype(np.float32 if X.dtype == np.int32 else np.float64)


from sklearn.decomposition import PCA

class CCIPCA(BaseEstimator, TransformerMixin):
    """Candid covariance-free incremental principal component analysis (CCIPCA)

    Linear dimensionality reduction using an online incremental PCA algorithm.
    CCIPCA computes the principal components incrementally without
    estimating the covariance matrix. This algorithm was designed for high
    dimensional data and converges quickly. 

    This implementation only works for dense arrays. However it should scale
    well to large data.
    
    Time Complexity: per iteration 3 dot products and 2 additions over 'n' where 'n' 
                     is the number of features (n_features).

    Implementation of:
        author={Juyang Weng and Yilu Zhang and Wey-Shiuan Hwang}
        journal={Pattern Analysis and Machine Intelligence, IEEE Transactions on}
        title={Candid covariance-free incremental principal component analysis}
        year={2003}
        month={aug}
        volume={25}
        number={8}
        pages={1034-1040}

    Parameters
    ----------
    n_components : int
        Number of components to keep.
        Must be set
    
    amnesia : float
        A parameter that weights the present more strongly than the
        past. amnesia=1 makes the present count the same as anything else.

    copy : bool
        If False, data passed to fit are overwritten

    Attributes
    ----------
    `components_` : array, [n_components, n_features]
        Components.

    `explained_variance_ratio_` : array, [n_components]
        Percentage of variance explained by each of the selected components.

    Notes
    -----
    Calling fit(X) multiple times will update the components_ etc.

    Examples
    --------

    >>> import numpy as np
    >>> from sklearn.decomposition import CCIPCA
    >>> X = np.array([[-1, -1], [-2, -1], [-3, -2], [1, 1], [2, 1], [3, 2]])
    >>> ccipca = CCIPCA(n_components=2)
    >>> ccipca.fit(X)
    CCIPCA(amnesic=2.0, copy=True, n_components=2)
    >>> print(ccipca.explained_variance_ratio_)
    [ 0.97074203  0.02925797]
    
    See also
    --------
    PCA
    ProbabilisticPCA
    RandomizedPCA
    KernelPCA
    SparsePCA
    IPCA
    """
    
    def __init__(self, n_components=2, amnesic=2.0, copy=True):
        self.n_components = n_components
        if self.n_components < 2:
            raise ValueError ("must specifiy n_components for CCIPCA")
            
        self.copy = copy
        self.amnesic = amnesic
        self.iteration = 0

    def fit(self, X, y=None, **params):
        """Fit the model with X.

        Parameters
        ----------
        X: array-like, shape (n_samples, n_features)
            Training data, where n_samples in the number of samples
            and n_features is the number of features.

        Returns
        -------
        self : object
            Returns the instance itself.
            
        Notes
        -----
        Calling multiple times will update the components
        """
        
        X = array2d(X)
        n_samples, n_features = X.shape 
        X = as_float_array(X, copy=self.copy)
        
        # init
        if self.iteration == 0:  
            self.mean_ = np.zeros([n_features], np.float)
            self.components_ = np.zeros([self.n_components,n_features], np.float)
        else:
            if n_features != self.components_.shape[1]:
                raise ValueError('The dimensionality of the new data and the existing components_ does not match')   
        
        # incrementally fit the model
        for i in range(0,X.shape[0]):
            self.partial_fit(X[i,:])
        
        # update explained_variance_ratio_
        self.explained_variance_ratio_ = np.sqrt(np.sum(self.components_**2,axis=1))
        
        # sort by explained_variance_ratio_
        idx = np.argsort(-self.explained_variance_ratio_)
        self.explained_variance_ratio_ = self.explained_variance_ratio_[idx]
        self.components_ = self.components_[idx,:]
        
        # re-normalize
        self.explained_variance_ratio_ = (self.explained_variance_ratio_ / self.explained_variance_ratio_.sum())
            
        for r in range(0,self.components_.shape[0]):
            self.components_[r,:] /= np.sqrt(np.dot(self.components_[r,:],self.components_[r,:]))
        
        return self
      
    def partial_fit(self, u):
        """ Updates the mean and components to account for a new vector.
        
        Parameters
        ----------
        _u : array [1, n_features]
            a single new data sample
        """
        
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
    
    def transform(self, X):
        """Apply the dimensionality reduction on X.

        Parameters
        ----------
        X : array-like, shape (n_samples, n_features)
            New data, where n_samples in the number of samples
            and n_features is the number of features.

        Returns
        -------
        X_new : array-like, shape (n_samples, n_components)

        """
        X = array2d(X)
        X_transformed = X - self.mean_
        X_transformed = np.dot(X_transformed, self.components_.T)
        return X_transformed

    def inverse_transform(self, X):
        """Transform data back to its original space, i.e.,
        return an input X_original whose transform would be X

        Parameters
        ----------
        X : array-like, shape (n_samples, n_components)
            New data, where n_samples in the number of samples
            and n_components is the number of components.

        Returns
        -------
        X_original array-like, shape (n_samples, n_features)
        """
        return np.dot(X, self.components_) + self.mean_
    
import scipy.sparse as sps
from sklearn import datasets
from sklearn.decomposition import PCA
import pylab as pl

# Load data
iris = datasets.load_iris()
X = iris.data
y = iris.target
target_names = iris.target_names

# PCA
k = 2
pca = PCA(n_components=k)
X_r = pca.fit(X).transform(X)
print pca.components_.T

# CCIPCA
k = 2
ccipca = CCIPCA(n_components=k)   
X1 = sps.lil_matrix(X)
X_r2 = ccipca.fit(X).transform(X1)
print ccipca.components_.T


# Plot
pl.figure()
for c, i, target_name in zip("rgb", [0, 1, 2], target_names):
  pl.scatter(X_r[y == i, 0], X_r[y == i, 1], c=c, label=target_name)
pl.legend()
pl.title('PCA of IRIS dataset')

pl.figure()
for c, i, target_name in zip("rgb", [0, 1, 2], target_names):
  pl.scatter(X_r2[y == i, 0], X_r2[y == i, 1], c=c, label=target_name)
pl.legend()
pl.title('CCIPCA of IRIS dataset')

#pl.show()

'''
[[ 0.36158968  0.65653988]
 [-0.08226889  0.72971237]
 [ 0.85657211 -0.1757674 ]
 [ 0.35884393 -0.07470647]]
[[ 0.81484887  0.09304124]
 [ 0.52092446 -0.38124227]
 [ 0.25321053  0.86734622]
 [ 0.02331202  0.30611795]]
'''
