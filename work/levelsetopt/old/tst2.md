
https://en.wikipedia.org/wiki/Test_functions_for_optimization

Rosen
1.0,1.0
Peaks
-1.34739625,  0.20451886]
Himmelblau
[3,2][-2.805,3.131],[-3.779,-3.28][3.584,-1.848]

```python
N = 100
D = 2
```


```python
from scipy.ndimage.filters import gaussian_filter

def Rosen2(x):
    return 100*(x[1]-x[0]**2)**2+(1-x[0])**2

def Peaks2(x):
    return 3*(1-x[0])**2*np.exp(-(x[0]**2) - (x[1]+1)**2) \
   - 10*(x[0]/5 - x[0]**3 - x[1]**5)*np.exp(-x[0]**2-x[1]**2) \
   - 1/3*np.exp(-(x[0]+1)**2 - x[1]**2)

def himmelblau(X):
    x = X[0]
    y = X[1]
    a = x*x + y - 11
    b = x + y*y - 7
    return a*a + b*b

np.random.seed(0)
x = np.linspace(-3,3,N); y = np.linspace(-3,3,N)
xx,yy = np.meshgrid(x,y)

xs = xx.reshape(N*N,1)
ys = yy.reshape(N*N,1)

xy = np.hstack((xs,ys))

zs = Peaks2(xy.T)
print (zs.shape)

#sigma = 0.8
#zs = gaussian_filter(zs, sigma)
c = -4
zf = zs[zs<c]
xf = xy[zs<c]
print (xf.shape)
w = np.ones((1,2))*1.0/D
tmp = np.dot( w,np.cov(xf.T))
print ( np.dot(tmp,w.T) )
print ( np.mean(xf,axis=0) )
#print (zf)
#print (xf)
```

```text
(10000,)
(230, 2)
[[0.04086424]]
[ 0.23610013 -1.66376812]
```




```python
zs = Rosen2(xy.T)
print (zs.shape)
c = 0.1
zf = zs[zs<c]
xf = xy[zs<c]
print (xf.shape)
w = np.ones((1,2))*1.0/D
tmp = np.dot( w,np.cov(xf.T))
print ( np.dot(tmp,w.T) )
print ( np.mean(xf,axis=0) )
#print (zf)
#print (xf)
```

```text
(10000,)
(7, 2)
[[0.05806988]]
[1.         1.01731602]
```


