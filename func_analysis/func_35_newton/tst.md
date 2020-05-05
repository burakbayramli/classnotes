```python
from mpl_toolkits.mplot3d import Axes3D

def f2(x1,x2):
    return f([x1,x2])

D = 50
x = np.linspace(-2.0,1.0,D)
y = np.linspace(-1.0,1.0,D)

xx,yy = np.meshgrid(x,y)
zz = f2(xx,yy)

contours = [1,2,3,4,5,6]
cs=plt.contour(xx,yy,zz,contours)
plt.plot(-1.0,0.5,'rd')
plt.savefig('boyd-1092.png')

#fig = plt.figure()
#ax = fig.gca(projection='3d')
#surf = ax.plot_surface(xx, yy, zz)
#plt.savefig('boyd-1091.png')
```

```python
import autograd
import autograd.numpy as anp

def f(x):
    x1,x2=x
    return anp.exp(x1 + 3.0*x2 - 0.1) + anp.exp( x1 - 3.0*x2 - 0.1 ) + anp.exp(-x1-0.1)

print
xx = np.array([1.0,1.0])
h = autograd.hessian(f)
print (h(xx))
```

```text
[[ 49.85777662 147.83997803]
 [147.83997803 445.7241498 ]]
```


```python
import numpy.linalg as lin

alpha = 0.1
beta = 0.7
eps = 0.001
#x0 = np.array([-1.1, 1.0])
x0 = np.array([1.5, -3.5])
x = x0
hist = []
for i in range(4):
   h = autograd.hessian(f)
   g = autograd.grad(f)
   d = np.dot(-lin.inv(h(x)),g(x))
   lamsq = np.dot(np.dot(g(x).T,lin.inv(h(x))),g(x))
   hist.append(x)
   if lamsq/2 <= eps:
      print ('done')
      break
   t = 1.0
   while f(x+t*d) >= f(x)+alpha*t*np.dot(g(x).T,d): t = t*beta
   print (t)
   x = x + t*d
h = np.array(hist)
h = np.reshape(h,(len(h),2))
print (h)
```

```text
1.0
1.0
1.0
1.0
[[ 1.5        -3.5       ]
 [ 2.49558509 -2.83480497]
 [ 3.2825754  -2.23914158]
 [ 2.73353298 -2.08882283]]
```

```python
from mpl_toolkits.mplot3d import Axes3D

def f2(x1,x2):
    return f([x1,x2])

D = 50
x = np.linspace(-2.0,1.0,D)
y = np.linspace(-1.0,1.0,D)

xx,yy = np.meshgrid(x,y)
zz = f2(xx,yy)

contours = [1,2,3,4,5,6]
cs=plt.contour(xx,yy,zz,contours)
plt.plot(h[:,0],h[:,1],'rd')
plt.plot(h[:,0],h[:,1])
plt.savefig('boyd-1092.png')
```

















