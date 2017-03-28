

```python
from scipy.spatial import ConvexHull
import pandas as pd, math
from math import sqrt, fabs

def plot_line(pt1,pt2,color):
    plt.plot(np.array([pt1[0],pt2[0]]),np.array([pt1[1],pt2[1]]),color=color)
    
def plot_quad(c,color='r'):
    plot_line(c[1],c[0],color)
    plot_line(c[2],c[1],color)
    plot_line(c[3],c[2],color)
    plot_line(c[0],c[3],color)

def pdis(a, b, c):
    t = b[0]-a[0], b[1]-a[1]           # Vector ab
    dd = sqrt(t[0]**2+t[1]**2)         # Length of ab
    t = t[0]/dd, t[1]/dd               # unit vector of ab
    n = -t[1], t[0]                    # normal unit vector to ab
    ac = c[0]-a[0], c[1]-a[1]          # vector ac
    return fabs(ac[0]*n[0]+ac[1]*n[1]) # Projection of ac to n (the minimum distance)

def score_quad_fit(c, pt):
    arr = np.array([pdis(c[1],c[0],pt), pdis(c[2],c[1],pt), \
                    pdis(c[3],c[2],pt), pdis(c[0],c[3],pt) ])
    return np.min(arr)

points = np.array(pd.read_csv('quadri.csv'))
plt.plot(points[:,0], points[:,1], 'o')

rect1 = [[6,8],[12,10],[13,15],[6,14]]
plot_quad(rect1)

print score_quad_fit(rect1,np.array([10,10]))

r1total = np.array([score_quad_fit(rect1,p) for p in points]).sum()
print r1total

rect2 = [[7,9],[12,10],[11,14],[6,12]]
plot_quad(rect2,'g')

r2total = np.array([score_quad_fit(rect2,p) for p in points]).sum()
print r2total

plt.xlim(4,14); plt.ylim(6,17)
plt.savefig('out1.png')
```

```text
0.632455532034
65.8765373106
36.3649774766
```

```python
def same_sign(arr): return np.all(arr > 0) if arr[0] > 0 else np.all(arr < 0)

def inside_quad(rect1, pt1):
    pts = np.array(rect1)
    a =  pts - pt1
    d = np.zeros((4,2))
    d[0,:] = pts[1,:]-pts[0,:]
    d[1,:] = pts[2,:]-pts[1,:]
    d[2,:] = pts[3,:]-pts[2,:]
    d[3,:] = pts[0,:]-pts[3,:]
    res = np.cross(a,d)
    return same_sign(res), res

print inside_quad(rect1, np.array([10,10]))
print inside_quad(rect1, np.array([10,16]))
```

```text
(True, array([  4.,  10.,  32.,  24.]))
(False, array([ 40.,  16., -10.,  24.]))
```















