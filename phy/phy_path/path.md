
```python
from mpl_toolkits.mplot3d import Axes3D
import matplotlib.pyplot as plt
from matplotlib import cm
import numpy as np
import matplotlib.pyplot as plt
import numpy.linalg as lin
from scipy.spatial.distance import cdist
```

```python
D = 50
gamma = 2.0

x = np.linspace(36,37,D)
y = np.linspace(32,33,D)

xx,yy = np.meshgrid(x,y)

xr = np.array([[36.06122449],
               [36.71428571]])
yr = np.array([[32.67346939],
               [32.32653061]])
X = np.hstack((xr,yr))
print (xr)
print (yr)

Phi = np.exp(-gamma*cdist(X,X,metric='euclid'))

w = np.array([[0.5],[0.5]])

xxx = xx.reshape(D*D)
yyy = yy.reshape(D*D)

a = np.vstack((xxx,yyy))
d = cdist(X,a.T)
d = np.exp(-gamma * d)
dd = np.dot(w.T,d)
znew = dd.reshape(D,D)

fig = plt.figure()
ax = fig.gca(projection='3d')
surf = ax.plot_surface(xx, yy, znew, cmap=cm.coolwarm,linewidth=0, antialiased=False)
fig.colorbar(surf, shrink=0.5, aspect=5)
plt.savefig('rbf1.png')
```

```text
[[36.06122449]
 [36.71428571]]
[[32.67346939]
 [32.32653061]]
```
