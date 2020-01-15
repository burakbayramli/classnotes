import matplotlib.pyplot as plt

import numpy as np

f = 'xcor.mat'
import scipy.io as sio
xcor = sio.loadmat(f)
xcor = xcor['xcor']
xcor = np.reshape(xcor,(len(xcor)))

eps = 1e-6
mu = 50.0

def phi_tv(x):
   return np.sum(np.abs(np.diff(x)))
   
def phi_atv(x):
   return np.sum(np.sqrt(eps + np.power(np.diff(x),2)) - eps)
   
def f(u):
   return np.sum(np.power(u-xcor, 2)) + mu*phi_atv(u)

def opt():

   u0 = np.zeros(len(xcor))
   print (f(u0))

   from scipy.optimize import minimize, Bounds, SR1, BFGS

   opts = {'maxiter': 400, 'verbose': 2}
   #opts = {'maxiter': 200, 'disp': True}

   res = minimize (fun=f,
                   x0=u0,
                   options=opts,
                   jac='2-point',
                   hess=BFGS(),
                   method='trust-constr'
                   )

   print (res)
   return res.x

if __name__ == "__main__": 
   r = opt()
   plt.plot(range(5000), r)
   plt.savefig('/tmp/out1.png')
