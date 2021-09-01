
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
for i in range(1,n+1): y[i] = 1e0/x[i]

out = open("interpol.txt","w")                             # open output file
out.write("      x           y           xi          yi          f\n")
h = (x[n]-x[1])/(ni-1)
for i in range(1,ni+1):
   xi = x[1] + (i-1)*h                               # interpolation argument
   yi = Lagrange(x,y,n,xi)                                # interpolant value
   if (i <= n):
      out.write("{0:12.3e}{1:12.3e}{2:12.3e}{3:12.3e}{4:12.3e}\n".
                format(x[i],y[i],xi,yi,1e0/xi))
   else:
      out.write("{0:36.3e}{1:12.3e}{2:12.3e}\n".format(xi,yi,1e0/xi))
out.close()
