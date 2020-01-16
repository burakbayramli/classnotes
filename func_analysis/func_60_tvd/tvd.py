import pandas as pd
import scipy.sparse as sps
import scipy.sparse.linalg as slin

df = pd.read_csv('xcor.csv',header=None)
xcor = np.reshape(np.array(df[0]), (5000,1))
MU = 50.0
EPSILON = 0.001

ALPHA = 0.01;
BETA = 0.5;
MAXITERS = 100;
NTTOL = 1e-10;

n = len(xcor)
data = np.array([-1*np.ones(n), np.ones(n)])
diags = np.array([0, 1])
D = sps.spdiags(data, diags, n-1, n)

x = np.zeros((len(xcor),1))

for iter in range(MAXITERS):
   d = D.dot(x)
   tmp1 = np.dot((x-xcor).T,(x-xcor))
   tmp21 = np.sqrt(EPSILON**2 + np.power(d,2))
   tmp22 = EPSILON*np.ones((n-1,1))
   val = np.float(tmp1 + MU*np.sum(tmp21 - tmp22))
   tmp1 = 2*(x-xcor)
   tmp2 = MU*D.T.dot(d / np.sqrt(EPSILON**2 + d**2))
   grad = tmp1 + tmp2
   tmp1 = 2*sps.eye(n)
   tmp2 = EPSILON**2*(EPSILON**2+d**2)**(-3/2)
   tmp2 = tmp2.reshape((n-1))
   tmp3 = sps.spdiags(tmp2, 0, n-1, n-1)

   hess = tmp1 + MU*tmp3.dot(D).T.dot(D)
   v = slin.spsolve(-hess, grad)
   v = np.reshape(v, (n,1))
   lambdasqr = np.float(np.dot(-grad.T,v))
   if lambdasqr/2 < NTTOL: break
   t = 1;
   while True:
      tmp1 = np.float(np.dot((x+t*v-xcor).T,(x+t*v-xcor)))
      tmp2 = MU*np.sum(np.sqrt(EPSILON**2+(D*(x+t*v))**2)-EPSILON*np.ones((n-1,1)))
      tmp3 = val - ALPHA*t*lambdasqr
      if tmp1 + tmp2 < tmp3: break
      t = BETA*t

   x = x+t*v
   #exit()

print ('x',np.sum(x))
