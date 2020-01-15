import pandas as pd
import matplotlib.pyplot as plt
from scipy.sparse import spdiags
import numpy as np

df = pd.read_csv('xcor.csv',header=None)
xcor = np.reshape(np.array(df[0]), (5000,1))
print(xcor)
MU = 50.0
EPSILON = 0.001

ALPHA = 0.01;
BETA = 0.5;
MAXITERS = 100;
NTTOL = 1e-10;

n = len(xcor)
data = np.array([-1*np.ones(n), np.ones(n)])
diags = np.array([0, 1])
D = spdiags(data, diags, n-1, n).toarray()

x = np.zeros((len(xcor),1))

for iter in range(MAXITERS):
   d = D.dot(x)
   tmp1 = np.dot((x-xcor).T,(x-xcor))
   tmp21 = np.sqrt(EPSILON**2 + np.power(d,2))
   tmp22 = EPSILON*np.ones((n-1,1))
   tmp2 = np.float(tmp1 + MU*np.sum(tmp21 - tmp22))
   print (tmp2)

   exit()
