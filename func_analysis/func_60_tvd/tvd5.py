import matplotlib.pyplot as plt
from scipy.sparse import spdiags
import numpy as np

f = 'xcor.mat'
import scipy.io as sio
xcor = sio.loadmat(f)
xcor = xcor['xcor']
xcor = np.reshape(xcor,(len(xcor)))

eps = 1e-6
mu = 50.0

ALPHA = 0.01;
BETA = 0.5;
MAXITERS = 100;
NTTOL = 1e-10;

n = 4
data = np.array([-1*np.ones(n), np.ones(n)])
diags = np.array([0, 1])
D = spdiags(data, diags, len(xcor)-1, len(xcor)).toarray()
print (D.shape)
x = np.zeros((len(xcor),1))

for iter in range(MAXITERS):
   d = D.dot(x)
   print (d)
   exit()
