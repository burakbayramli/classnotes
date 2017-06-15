import numpy as np
from scipy import linalg
import scipy.sparse.linalg as linalg2

def svd_flip(u, v, u_based_decision=True):
    if u_based_decision:
        # columns of u, rows of v
        max_abs_cols = np.argmax(np.abs(u), axis=0)
        signs = np.sign(u[max_abs_cols, xrange(u.shape[1])])
        u *= signs
        v *= signs[:, np.newaxis]
    else:
        # rows of v, columns of u
        max_abs_rows = np.argmax(np.abs(v), axis=1)
        signs = np.sign(v[xrange(v.shape[0]), max_abs_rows])
        u *= signs
        v *= signs[:, np.newaxis]
    return u, v

def _incremental_mean_and_var(X, last_mean=.0, last_variance=None,
                              last_sample_count=0):

    last_sum = last_mean * last_sample_count
    new_sum = X.sum(axis=0)
    new_sample_count = X.shape[0]
    updated_sample_count = last_sample_count + new_sample_count
    updated_mean = (last_sum + new_sum) / updated_sample_count

    if last_variance is None:
        updated_variance = None
    else:
        new_unnormalized_variance = X.var(axis=0) * new_sample_count
        if last_sample_count == 0:  # Avoid division by 0
            updated_unnormalized_variance = new_unnormalized_variance
        else:
            last_over_new_count = last_sample_count / new_sample_count
            last_unnormalized_variance = last_variance * last_sample_count
            updated_unnormalized_variance = (
                last_unnormalized_variance +
                new_unnormalized_variance +
                last_over_new_count / updated_sample_count *
                (last_sum / last_over_new_count - new_sum) ** 2)
        updated_variance = updated_unnormalized_variance / updated_sample_count

    return updated_mean, updated_variance, updated_sample_count


class MyIncrementalPCA:

    def __init__(self, n_components):
        self.n_components = n_components
        self.n_samples_seen_ = 0
        self.mean_ = .0
        self.var_ = .0
        self.singular_values_ = None
        self.explained_variance_ = None
        self.explained_variance_ratio_ = None
        self.noise_variance_ = None        

    def partial_fit(self, X, y=None, check_input=True):
        n_samples, n_features = X.shape
        if not hasattr(self, 'components_'):
            self.components_ = None

        if self.n_components is None:
            self.n_components_ = n_features
        elif not 1 <= self.n_components <= n_features:
            raise ValueError("n_components=%r invalid for n_features=%d, need "
                             "more rows than columns for IncrementalPCA "
                             "processing" % (self.n_components, n_features))
        else:
            self.n_components_ = self.n_components

        if (self.components_ is not None) and (self.components_.shape[0] !=
                                               self.n_components_):
            raise ValueError("Number of input features has changed from %i "
                             "to %i between calls to partial_fit! Try "
                             "setting n_components to a fixed value." %
                             (self.components_.shape[0], self.n_components_))

        # This is the first partial_fit
        if not hasattr(self, 'n_samples_seen_'):
            self.n_samples_seen_ = 0
            self.mean_ = .0
            self.var_ = .0

        # Update stats - they are 0 if this is the fisrt step
        col_mean, col_var, n_total_samples = \
            _incremental_mean_and_var(X, last_mean=self.mean_,
                                      last_variance=self.var_,
                                      last_sample_count=self.n_samples_seen_)

        # Whitening
        if self.n_samples_seen_ == 0:
            # If it is the first step, simply whiten X
            X -= col_mean
        else:
            col_batch_mean = np.mean(X, axis=0)
            X -= col_batch_mean
            # Build matrix of combined previous basis and new data
            mean_correction = \
                np.sqrt((self.n_samples_seen_ * n_samples) /
                        n_total_samples) * (self.mean_ - col_batch_mean)
            X = np.vstack((self.singular_values_.reshape((-1, 1)) *
                          self.components_, X, mean_correction))

        U, S, V = linalg2.svds(X, k=self.n_components_)
        #U, S, V = linalg.svd(X, full_matrices=False)
        U, V = svd_flip(U, V, u_based_decision=False)
        explained_variance = S ** 2 / n_total_samples
        explained_variance_ratio = S ** 2 / np.sum(col_var * n_total_samples)

        self.n_samples_seen_ = n_total_samples
        self.components_ = V[:self.n_components_]
        self.singular_values_ = S[:self.n_components_]
        self.mean_ = col_mean
        self.var_ = col_var
        self.explained_variance_ = explained_variance[:self.n_components_]
        self.explained_variance_ratio_ = \
            explained_variance_ratio[:self.n_components_]
        if self.n_components_ < n_features:
            self.noise_variance_ = \
                explained_variance[self.n_components_:].mean()
        else:
            self.noise_variance_ = 0.
        return self

import pandas as pd

df = pd.read_csv('/home/burak/Documents/classnotes/stat/stat_preproc/iris.csv')
df = np.array(df)[:,:4].astype(float)
print df
pca = MyIncrementalPCA(n_components=2)
S  = 10
df2 = df.copy()
for i in range(15):
    pca.partial_fit(df2[i*S:(i+1)*S, :])
print 'exp var', pca.explained_variance_
print 'var rat',pca.explained_variance_ratio_
print 'pca s', pca.singular_values_
print 'pca c', pca.components_.T

u,s,vt = linalg.svd(df)
print 'full c',vt
print 'full s',s

from sklearn.decomposition import IncrementalPCA
pca = IncrementalPCA(n_components=2)
S  = 10
df2 = df.copy()
for i in range(15):
    pca.partial_fit(df2[i*S:(i+1)*S, :])
print
print 'exp var', pca.explained_variance_
print 'var rat',pca.explained_variance_ratio_
print 'pca2 s', pca.singular_values_
print 'pca2 c', pca.components_.T












