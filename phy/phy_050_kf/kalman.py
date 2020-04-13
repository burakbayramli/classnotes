from scipy.stats import norm, multivariate_normal
import pandas as pd, math
import numpy as np, numpy.linalg as linalg
import matplotlib.pyplot as plt

def logpdf(x, mean, cov):
    flat_mean = np.asarray(mean).flatten()
    flat_x = np.asarray(x).flatten()
    return multivariate_normal.logpdf(flat_x, flat_mean, cov, True)

def dot3(A,B,C):
    return np.dot(A, np.dot(B,C))

def setter_scalar(value, dim_x):
    if np.isscalar(value):
        v = np.eye(dim_x) * value
    else:
        v = np.array(value, dtype=float)
        dim_x = v.shape[0]

    if v.shape != (dim_x, dim_x):
        raise Exception('must have shape ({},{})'.format(dim_x, dim_x))
    return v

def setter(value, dim_x, dim_y):
    v = np.array(value, dtype=float)
    if v.shape != (dim_x, dim_y):
        raise Exception('must have shape ({},{})'.format(dim_x, dim_y))
    return v

def setter_1d(value, dim_x):
    v = np.array(value, dtype=float)
    shape = v.shape
    if shape[0] != (dim_x) or v.ndim > 2 or (v.ndim==2 and shape[1] != 1):
        raise Exception('has shape {}, must have shape ({},{})'.format(shape, dim_x, 1))
    return v


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

class KalmanFilter(object):
    def __init__(self, dim_x, dim_z, dim_u=0):
        assert dim_x > 0
        assert dim_z > 0
        assert dim_u >= 0

        self.dim_x = dim_x
        self.dim_z = dim_z
        self.dim_u = dim_u

        self._x = np.zeros((dim_x,1)) # state
        self._P = np.eye(dim_x)       # uncertainty covariance
        self._Q = np.eye(dim_x)       # process uncertainty
        self._B = 0                # control transition matrix
        self._F = 0                # state transition matrix
        self.H = 0                 # Measurement function
        self.R = np.eye(dim_z)        # state uncertainty
        self._alpha_sq = 1.        # fading memory control
        self.M = 0                 # process-measurement cross correlation

        # gain and residual are computed during the innovation step. We
        # save them so that in case you want to inspect them for various
        # purposes
        self._K = 0 # kalman gain
        self._y = np.zeros((dim_z, 1))
        self._S = np.zeros((dim_z, dim_z)) # system uncertainty

        # identity matrix. Do not alter this.
        self._I = np.eye(dim_x)


    def update(self, z, R=None, H=None):
        if z is None:
            return

        if R is None:
            R = self.R
        elif isscalar(R):
            R = eye(self.dim_z) * R

        # rename for readability and a tiny extra bit of speed
        if H is None:
            H = self.H
        P = self._P
        x = self._x

        # handle special case: if z is in form [[z]] but x is not a column
        # vector dimensions will not match
        if x.ndim==1 and np.shape(z) == (1,1):
            z = z[0]

        if np.shape(z) == (): # is it scalar, e.g. z=3 or z=np.array(3)
            z = np.asarray([z])

        # y = z - Hx
        # error (residual) between measurement and prediction
        Hx = np.dot(H, x)

        assert np.shape(Hx) == np.shape(z) or (np.shape(Hx) == (1,1) and np.shape(z) == (1,)), \
               'shape of z should be {}, but it is {}'.format(
               np.shape(Hx), np.shape(z))
        self._y = z - Hx

        # S = HPH' + R
        # project system uncertainty into measurement space
        S = dot3(H, P, H.T) + R

        # K = PH'inv(S)
        # map system uncertainty into kalman gain
        K = dot3(P, H.T, linalg.inv(S))

        # x = x + Ky
        # predict new x with residual scaled by the kalman gain
        self._x = x + np.dot(K, self._y)

        # P = (I-KH)P(I-KH)' + KRK'
        I_KH = self._I - np.dot(K, H)
        self._P = dot3(I_KH, P, I_KH.T) + dot3(K, R, K.T)

        self._S = S
        self._K = K

        self.log_likelihood = logpdf(z, np.dot(H, x), S)


    def update_correlated(self, z, R=None, H=None):
        if z is None:
            return

        if R is None:
            R = self.R
        elif isscalar(R):
            R = eye(self.dim_z) * R

        # rename for readability and a tiny extra bit of speed
        if H is None:
            H = self.H
        x = self._x
        P = self._P
        M = self.M

        # handle special case: if z is in form [[z]] but x is not a column
        # vector dimensions will not match
        if x.ndim==1 and shape(z) == (1,1):
            z = z[0]

        if shape(z) == (): # is it scalar, e.g. z=3 or z=np.array(3)
            z = np.asarray([z])

        # y = z - Hx
        # error (residual) between measurement and prediction
        self._y = z - dot(H, x)

        # project system uncertainty into measurement space
        S = dot3(H, P, H.T) + dot(H, M) + dot(M.T, H.T) + R

        # K = PH'inv(S)
        # map system uncertainty into kalman gain
        K = dot(dot(P, H.T) + M, linalg.inv(S))

        # x = x + Ky
        # predict new x with residual scaled by the kalman gain
        self._x = x + dot(K, self._y)
        self._P = P - dot(K, dot(H, P) + M.T)

        self._S = S
        self._K = K

        # compute log likelihood
        self.log_likelihood = logpdf(z, dot(H, x), S)


    def predict(self, u=0, B=None, F=None, Q=None):
        if B is None:
            B = self._B
        if F is None:
            F = self._F
        if Q is None:
            Q = self._Q
        elif np.isscalar(Q):
            Q = np.eye(self.dim_x) * Q

        # x = Fx + Bu
        self._x = np.dot(F, self.x) + np.dot(B, u)

        # P = FPF' + Q
        self._P = self._alpha_sq * dot3(F, self._P, F.T) + Q

    def get_prediction(self, u=0):
        x = dot(self._F, self._x) + dot(self._B, u)
        P = self._alpha_sq * dot3(self._F, self._P, self._F.T) + self._Q
        return (x, P)


    def residual_of(self, z):
        return z - dot(self.H, self._x)


    def measurement_of_state(self, x):
        return dot(self.H, x)


    @property
    def alpha(self):
        return self._alpha_sq**.5


    @property
    def likelihood(self):
        return math.exp(self.log_likelihood)


    @alpha.setter
    def alpha(self, value):
        assert np.isscalar(value)
        assert value > 0

        self._alpha_sq = value**2

    @property
    def Q(self):
        return self._Q


    @Q.setter
    def Q(self, value):
        self._Q = setter_scalar(value, self.dim_x)

    @property
    def P(self):
        return self._P


    @P.setter
    def P(self, value):
        self._P = setter_scalar(value, self.dim_x)


    @property
    def F(self):
        return self._F


    @F.setter
    def F(self, value):
        self._F = setter(value, self.dim_x, self.dim_x)

    @property
    def B(self):
        return self._B


    @B.setter
    def B(self, value):
        if np.isscalar(value):
            self._B = value
        else:
            self._B = setter (value, self.dim_x, self.dim_u)


    @property
    def x(self):
        return self._x

    @x.setter
    def x(self, value):
        self._x = setter_1d(value, self.dim_x)

    @property
    def K(self):
        return self._K


    @property
    def y(self):
        return self._y

    @property
    def S(self):
        return self._S

