from __future__ import (absolute_import, division, print_function,
                        unicode_literals)

import math, util, scipy.linalg as linalg
from numpy import eye, zeros, dot, isscalar, outer
from scipy.linalg import inv, cholesky
from numpy.random import randn
from scipy.stats import norm, multivariate_normal
import numpy as np

def dot3(A,B,C):
    return dot(A, dot(B,C))

def logpdf(x, mean, cov):
    flat_mean = np.asarray(mean).flatten()
    flat_x = np.asarray(x).flatten()
    return multivariate_normal.logpdf(flat_x, flat_mean, cov, True)

def unscented_transform(sigmas, Wm, Wc, noise_cov=None,
                        mean_fn=None, residual_fn=None):
    kmax, n = sigmas.shape

    if mean_fn is None:
        x = np.dot(Wm, sigmas)    # dot = \Sigma^n_1 (W[k]*Xi[k])
    else:
        x = mean_fn(sigmas, Wm)

    if residual_fn is None:
        y = sigmas - x[np.newaxis,:]
        P = y.T.dot(np.diag(Wc)).dot(y)
    else:
        P = np.zeros((n, n))
        for k in range(kmax):
            y = residual_fn(sigmas[k], x)
            P += Wc[k] * np.outer(y, y)

    if noise_cov is not None:
        P += noise_cov

    return (x, P)

def Q_discrete_white_noise(dim, dt=1., var=1.):
    assert dim == 2 or dim == 3
    if dim == 2:
        Q = np.array([[.25*dt**4, .5*dt**3],
                      [ .5*dt**3,    dt**2]], dtype=float)
    else:
        Q = np.array([[.25*dt**4, .5*dt**3, .5*dt**2],
                      [ .5*dt**3,    dt**2,       dt],
                      [ .5*dt**2,       dt,        1]], dtype=float)

    return Q * var

class MerweScaledSigmaPoints(object):

    def __init__(self, n, alpha, beta, kappa, sqrt_method=None, subtract=None):

        self.n = n
        self.alpha = alpha
        self.beta = beta
        self.kappa = kappa
        if subtract is None:
            self.subtract= np.subtract
        else:
            self.subtract = subtract

    def num_sigmas(self):
        return 2*self.n + 1

    def sigma_points(self, x, P):
        assert self.n == np.size(x), \
            "expected size {}, but size is {}".format(self.n, np.size(x))

        n = self.n
        if np.isscalar(x): x = np.asarray([x])
        if  np.isscalar(P): P = np.eye(n)*P
        else: P = np.asarray(P)

        lambda_ = self.alpha**2 * (n + self.kappa) - n
        U = cholesky((lambda_ + n)*P)

        sigmas = np.zeros((2*n+1, n))
        sigmas[0] = x
        for k in range(n):
            sigmas[k+1]   = self.subtract(x, -U[k])
            sigmas[n+k+1] = self.subtract(x, U[k])

        return sigmas


    def weights(self):
        n = self.n
        lambda_ = self.alpha**2 * (n +self.kappa) - n

        c = .5 / (n + lambda_)
        Wc = np.full(2*n + 1, c)
        Wm = np.full(2*n + 1, c)
        Wc[0] = lambda_ / (n + lambda_) + (1 - self.alpha**2 + self.beta)
        Wm[0] = lambda_ / (n + lambda_)
        return Wm, Wc

class UKF(object):
    def __init__(self, dim_x, dim_z, hx, fx, points,
                 x_mean_fn=None, z_mean_fn=None,
                 residual_x=None,
                 residual_z=None):

        self.Q = eye(dim_x)
        self.R = eye(dim_z)
        self.x = zeros(dim_x)
        self.P = eye(dim_x)
        self._dim_x = dim_x
        self._dim_z = dim_z
        self.points_fn = points
        self._num_sigmas = points.num_sigmas()
        self.hx = hx
        self.fx = fx
        self.x_mean = x_mean_fn
        self.z_mean = z_mean_fn
        self.log_likelihood = 0.0
        self.Wm, self.Wc = self.points_fn.weights()

        if residual_x is None:
            self.residual_x = np.subtract
        else:
            self.residual_x = residual_x

        if residual_z is None:
            self.residual_z = np.subtract
        else:
            self.residual_z = residual_z

        self.sigmas_f = zeros((self._num_sigmas, self._dim_x))
        self.sigmas_h = zeros((self._num_sigmas, self._dim_z))


    def predict(self, dt):
        sigmas = self.points_fn.sigma_points(self.x, self.P)        
        for i in range(self._num_sigmas):
            # parametre verilen dt oldugu gibi fx'e geciliyor,
            # yani UKF matematiginde direk kullanilmiyor
            self.sigmas_f[i] = self.fx(sigmas[i], dt)            
        self.x, self.P = unscented_transform(self.sigmas_f,
                                             self.Wm,
                                             self.Wc,
                                             self.Q,
                                             self.x_mean,
                                             self.residual_x)

    def predict(self, dt, u):
        sigmas = self.points_fn.sigma_points(self.x, self.P)        
        for i in range(self._num_sigmas):
            # parametre verilen dt oldugu gibi fx'e geciliyor,
            # yani UKF matematiginde direk kullanilmiyor
            self.sigmas_f[i] = self.fx(sigmas[i], dt, u)
        self.x, self.P = unscented_transform(self.sigmas_f,
                                             self.Wm,
                                             self.Wc,
                                             self.Q,
                                             self.x_mean,
                                             self.residual_x)
        

    def update(self, z):
        for i in range(self._num_sigmas):
            self.sigmas_h[i] = self.hx(self.sigmas_f[i])
        zp, Pz = unscented_transform(self.sigmas_h,
                                     self.Wm,
                                     self.Wc,
                                     self.R,
                                     self.z_mean,
                                     self.residual_z)

        Pxz = zeros((self._dim_x, self._dim_z))
        for i in range(self._num_sigmas):
            dx = self.residual_x(self.sigmas_f[i], self.x)
            dz =  self.residual_z(self.sigmas_h[i], zp)
            Pxz += self.Wc[i] * outer(dx, dz)


        self.K = dot(Pxz, inv(Pz)) 
        self.y = self.residual_z(z, zp) 

        self.x = self.x + dot(self.K, self.y)
        self.P = self.P - dot3(self.K, Pz, self.K.T)

        self.log_likelihood = logpdf(self.y, np.zeros(len(self.y)), Pz)


    def cross_variance(self, x, z, sigmas_f, sigmas_h):
        Pxz = zeros((sigmas_f.shape[1], sigmas_h.shape[1]))
        N = sigmas_f.shape[0]
        for i in range(N):
            dx = self.residual_x(sigmas_f[i], x)
            dz =  self.residual_z(sigmas_h[i], z)
            Pxz += self.Wc[i] * outer(dx, dz)


    @property
    def likelihood(self):
        return math.exp(self.log_likelihood)
