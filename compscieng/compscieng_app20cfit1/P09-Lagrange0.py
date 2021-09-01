import numpy as np
import matplotlib.pyplot as plt

def Lagrange(x, y, n, xi):
   yi = 0e0
   for i in range(1,n+1):
      p = 1e0
      for j in range(1,n+1):
          if (j != i): p *= (xi - x[j])/(x[i] - x[j])
      yi += p * y[i]

   return yi

n  = 8                                                # kac veri noktasi
ni = 100                                     # number of interpolation points

x = [0]*(n+1)                                                   
y = [0]*(n+1)

# f(x) = 1/x, x degerleri gelisiguzel secilmis 
x[1] = 0.15; x[2] = 0.2; x[3] = 0.3; x[4] = 0.5;
x[5] = 0.8 ; x[6] = 1.1; x[7] = 1.4; x[8] = 1.7
for i in range(1,n+1): y[i] = np.sin(5*x[i])

h = (x[n]-x[1])/(ni-1)
xx = []; yy = []
for i in range(1,ni+1):
   xi = x[1] + (i-1)*h                               # interpolation argument
   yi = Lagrange(x,y,n,xi)                                # interpolant value
   xx.append(xi)
   yy.append(yi)

xx = np.array(xx)
yy = np.array(yy)

plt.plot(xx,yy)
plt.plot(x,y)
plt.show()
