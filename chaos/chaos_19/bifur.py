import numpy as np
import matplotlib.pyplot as plt

def logistic_func(r,x):
  return r*x*(1-x)

def xnext(r,xo,N):
  no = 0
  X = np.zeros(r.size*N)
  A = np.zeros(r.size*N)
  for r_value in r:
    xinit = xo
    for i in range(200):
      xinit = logistic_func(r_value,xinit)
    X[no] = xinit
    A[no] = r_value
    no += 1
    for j in range(1,N):
      X[no] = logistic_func(r_value,X[no-1])
      A[no]= r_value
      no += 1
  return A, X
  
xo = .4
Num = 71
r_min = 2.899
r_max = 3.999
total_of_r = 1700
r = np.linspace(r_min,r_max,total_of_r)

A, X = xnext(r,xo,Num)

fig, ax = plt.subplots(figsize=(12,8))
ax.plot(A,X,".",markersize=0.1,color="green")
ax.set_xlim(r_min,r_max)
ax.set_ylim(0,1)
ax.set_title(r'Catallasma Diyagrami: $Y_{sonraki} = Y_{onceki}*r*(1-Y_{onceki})$')
ax.set_xlabel('r')
ax.set_ylabel(r'$Y[r,Y_{onceki}]$')

plt.gca().set_aspect('equal', adjustable='box')
plt.savefig('19_08.png')
