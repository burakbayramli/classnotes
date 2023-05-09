from scipy.stats import multivariate_normal as mvn
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import numpy.linalg as linalg
import math, random, copy, sys
import scipy.stats

class Cov_problem(Exception): pass

def norm_pdf(b,mean,cov):
   k = b.shape[0]
   part1 = np.exp(-0.5*k*np.log(2*np.pi))
   part2 = np.power(np.linalg.det(cov),-0.5)
   dev = b-mean
   part3 = np.exp(-0.5*np.dot(np.dot(dev.transpose(),np.linalg.inv(cov)),dev))
   dmvnorm = part1*part2*part3
   return dmvnorm

def gm_log_likelihood(X, center_list, cov_list, p_k):
    """Finds the likelihood for a set of samples belongin to a Gaussian mixture
    model.
    
    Return log likelighood
    """
    samples = X.shape[0]
    K =  len(center_list)
    log_p_Xn = np.zeros(samples)
    for k in range(K):
        p = logmulnormpdf(X, center_list[k], cov_list[k]) + np.log(p_k[k])
        if k == 0:
            log_p_Xn = p
        else:
            pmax = np.max(np.concatenate((np.c_[log_p_Xn], np.c_[p]), axis=1), axis=1)
            log_p_Xn = pmax + np.log( np.exp( log_p_Xn - pmax) + np.exp( p-pmax))
    logL = np.sum(log_p_Xn)
    return logL

def gm_assign_to_cluster(X, center_list, cov_list, p_k):
    samples = X.shape[0]
    K = len(center_list)
    log_p_Xn_mat = np.zeros((samples, K))
    for k in range(K):
        log_p_Xn_mat[:,k] = logmulnormpdf(X, center_list[k], cov_list[k]) + np.log(p_k[k])
    pmax = np.max(log_p_Xn_mat, axis=1)
    log_p_Xn = pmax + np.log( np.sum( np.exp(log_p_Xn_mat.T - pmax), axis=0).T)
    logL = np.sum(log_p_Xn)
    
    log_p_nk = np.zeros((samples, K))
    for k in range(K):
        log_p_nk[:,k] = log_p_Xn_mat[:,k] - log_p_Xn
    
    maxP_k = np.c_[np.max(log_p_nk, axis=1)] == log_p_nk
    maxP_k = maxP_k * (np.array(range(K))+1)
    return np.sum(maxP_k, axis=1) - 1

def logmulnormpdf(X, MU, SIGMA):
    if MU.ndim != 1:
        raise ValueError, "MU must be a 1 dimensional array"
    mu = MU
    x = X.T
    if x.ndim == 1:
        x = np.atleast_2d(x).T
    sigma = np.atleast_2d(SIGMA) # So we also can use it for 1-d distributions
    N = len(MU)
    ex1 = np.dot(linalg.inv(sigma), (x.T-mu).T)
    ex = -0.5 * (x.T-mu).T * ex1
    if ex.ndim == 2: ex = np.sum(ex, axis = 0)
    K = -(N/2)*np.log(2*np.pi) - 0.5*np.log(np.linalg.det(SIGMA))
    return ex + K

def gmm_init(X, K, verbose = False,
                    cluster_init = 'sample', \
                    cluster_init_prop = {}, \
                    max_init_iter = 5, \
                    cov_init = 'var'):
    samples, dim = np.shape(X)
    if cluster_init == 'sample':
        if verbose: print "Using sample GMM initalization."
        center_list = []
        for i in range(K):
            center_list.append(X[np.random.randint(samples), :])
    elif cluster_init == 'box':
        if verbose: print "Using box GMM initalization."
        center_list = []
        X_max = np.max(X, axis=0)
        X_min = np.min(X, axis=0)
        for i in range(K):
            init_point = ((X_max-X_min)*np.random.rand(1,dim)) + X_min
            center_list.append(init_point.flatten())            
    elif cluster_init == 'kmeans':
        if verbose: print "Using K-means GMM initalization."
        # Normalize data (K-means is isotropic)
        normalizerX = preproc.Normalizer(X)
        nX = normalizerX.transform(X)
        center_list = []
        best_icv = np.inf
        for i in range(max_init_iter):
            m, kcc = kmeans.kmeans(nX, K, iter=100, **cluster_init_prop)
            icv = kmeans.find_intra_cluster_variance(X, m, kcc)
            if best_icv > icv:
                membership = m
                cc = kcc
                best_icv = icv
        cc = normalizerX.invtransform(cc)
        for i in range(cc.shape[0]):
            center_list.append(cc[i,:])
        print cc
    else:
        raise "Unknown initialization of EM of MoG centers."

    # Initialize co-variance matrices
    cov_list = []
    if cov_init=='iso':
        for i in range(K):
            cov_list.append(np.diag(np.ones(dim)/1e10))
            #cov_list.append(np.diag(np.ones(dim)))
    elif cov_init=='var':
        for i in range(K):
            cov_list.append(np.diag(np.var(X, axis=0)/1e10))
    else:
        raise ValueError('Unknown option used for cov_init')

    p_k = np.ones(K) / K # Uniform prior on P(k)
    return (center_list, cov_list, p_k)


def em_gm(X, K, max_iter = 50, verbose = False, \
                iter_call = None,\
                delta_stop = 1e-6,\
                init_kw = {}, \
                max_tries = 10,\
                diag_add = 1e-3):

    samples, dim = np.shape(X)
    clusters_found = False
    while clusters_found==False and max_tries>0:
        max_tries -= 1
        # Initialized clusters
        center_list, cov_list, p_k = gmm_init(X, K, **init_kw)
        # Now perform the EM-steps:
        try:
            center_list, cov_list, p_k, logL = \
                gmm_em_continue(X, center_list, cov_list, p_k,
                        max_iter=max_iter, verbose=verbose,
                        iter_call=iter_call,
                        delta_stop=delta_stop,
                        diag_add=diag_add)
            clusters_found = True
        except Cov_problem:
            if verbose:
                print "Problems with the co-variance matrix, tries left ", max_tries

    if clusters_found:
        return center_list, cov_list, p_k, logL
    else:
        raise Cov_problem()


def gmm_em_continue(X, center_list, cov_list, p_k,
                    max_iter = 50, verbose = False, \
                    iter_call = None,\
                    delta_stop = 1e-6,\
                    diag_add = 1e-3,\
                    delta_stop_count_end=10):
    """
    """
    delta_stop_count = 0
    samples, dim = np.shape(X)
    K = len(center_list) # We should do some input checking
    if diag_add!=0:
        feature_var = np.var(X, axis=0)
        diag_add_vec = diag_add * feature_var
    old_logL = np.NaN
    logL = np.NaN
    for i in xrange(max_iter):
        try:
            center_list, cov_list, p_k, logL = __em_gm_step(X, center_list,\
                cov_list, p_k, K, diag_add_vec)
        except np.linalg.linalg.LinAlgError: # Singular cov matrix
            raise Cov_problem()
        if iter_call is not None:
            iter_call(center_list, cov_list, p_k, i)
        # Check if we have problems with cluster sizes
        for i2 in range(len(center_list)):
            if np.any(np.isnan(cov_list[i2])):
                print "problem"
                raise Cov_problem()

        if old_logL != np.NaN:
            if verbose:
                print "iteration=", i, " delta log likelihood=", \
                    old_logL - logL
            if np.abs(logL - old_logL) < delta_stop: #* samples:
                delta_stop_count += 1
                if verbose: print "gmm_em_continue: delta_stop_count =", delta_stop_count
            else:
                delta_stop_count = 0
            if delta_stop_count>=delta_stop_count_end:
                break # Sufficient precision reached
        old_logL = logL
    try:
        gm_log_likelihood(X, center_list, cov_list, p_k)
    except np.linalg.linalg.LinAlgError: # Singular cov matrix
        raise Cov_problem()
    return center_list, cov_list, p_k, logL

def __em_gm_step(X, center_list, cov_list, p_k, K, diag_add_vec):
    samples = X.shape[0]
    # New way of calculating the log likelihood:
    log_p_Xn_mat = np.zeros((samples, K))
    for k in range(K):
        log_p_Xn_mat[:,k] = logmulnormpdf(X, center_list[k], cov_list[k]) + np.log(p_k[k])
    pmax = np.max(log_p_Xn_mat, axis=1)
    log_p_Xn = pmax + np.log( np.sum( np.exp(log_p_Xn_mat.T - pmax), axis=0).T) # Maybe move this down
    logL = np.sum(log_p_Xn)

    log_p_nk = np.zeros((samples, K))
    for k in range(K):
        log_p_nk[:,k] = log_p_Xn_mat[:,k] - log_p_Xn

    p_Xn = np.e**log_p_Xn
    p_nk = np.e**log_p_nk
       
    # M-step:    
    for k in range(K):
        ck = np.sum(p_nk[:,k] * X.T, axis = 1) / np.sum(p_nk[:,k])
        center_list[k] = ck
        cov_list[k] = np.dot(p_nk[:,k] * ((X - ck).T), (X - ck)) / sum(p_nk[:,k])
        p_k[k] = np.sum(p_nk[:,k]) / samples

    return (center_list, cov_list, p_k, logL)
