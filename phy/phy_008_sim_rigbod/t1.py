

\begin{minted}[fontsize=\footnotesize]{python}
import numpy.linalg as lin
from scipy.integrate import odeint

def skew(a):
   return np.array([[0,-a[2],a[1]],[a[2],0,-a[0]],[-a[1],a[0],0]])

prop = your_mesh.get_mass_properties()
xcm0 = np.round(prop[1],3)
R0 = np.eye(3,3)
R0inv = np.eye(3,3)
density = 1.0
print (xcm0)
M = 10
P0 = np.array([0,0,0])
L0 = np.array([0,0,0])

r1 = np.mean(your_mesh.vectors[2000],axis=0)
F1 = -your_mesh.get_unit_normals()[2000]

def rhs(u,t):
   xcm,R,P,L = u
   print ('here')
   v = P / M
   Iinv = np.dot(np.dot(R,R0inv), R)
   omega = np.dot(Iinv, L)
   Rdot = np.dot(skew(omega), R)   
   F = np.array([0,0,0])
   if t==0:
     tau = np.cross(r1, F1)
   else:
     tau = np.array([0,0,0])
   return [v, Rdot, F, tau]
     
t=np.linspace(0.0, 1.0, 2)
u0 = (xcm0, R0, P0, L0)
u1=odeint(rhs,u0,t)
print (u1)
\end{minted}
