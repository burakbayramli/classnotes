#+LaTeX_HEADER: \newcommand{\ud}{\,\mathrm{d}}
#+LaTeX_HEADER: \newcommand{\mlabel}[1]{\quad \text{(#1)}\quad}
#+LaTeX_HEADER: \usepackage{palatino,eulervm}
#+LaTeX_HEADER: \usepackage{cancel}

$$
f(x,y) = 0.5 \exp (-\gamma || x - m_1 ||^2) + 0.5 \exp (-\gamma || x - m_2 ||^2 )
$$


```python
t = np.linspace(0,1.0,100)

a1,a2,a3 = -0.5, 4.0, 3.0
b1,b2,b3 = 2.5, -3.3, -3.0

sx,sy=(1.0,1.0)
ex,ey=(4.0,4.0)

a4 = ex - sx - (a1+a2+a3)
b4 = ey - sy - (b1+b2+b3)

x = 1.0 + a1*t + a2*t**2 + a3*t**3 + a4*t**4 
y = 1.0 + b1*t + b2*t**2 + b3*t**3 + b4*t**4

plt.xlim(0,5.0)
plt.ylim(0,5.0)
plt.plot(x,y)
plt.savefig('out6.png')
```

```python
t = np.linspace(0,1.0,100)

a1,a2,a3 = 5.5, 1.0, 1.0
b1,b2,b3 = -9.5, 1.3, 1.0

sx,sy=(1.0,1.0)
ex,ey=(-4.0,-4.0)

a4 = ex - sx - (a1+a2+a3)
b4 = ey - sy - (b1+b2+b3)

x = 1.0 + a1*t + a2*t**2 + a3*t**3 + a4*t**4 
y = 1.0 + b1*t + b2*t**2 + b3*t**3 + b4*t**4

plt.xlim(-5,5)
plt.ylim(-5,5)
plt.plot(x,y)
plt.savefig('out6.png')
```



```python
from mpl_toolkits.mplot3d import Axes3D
from scipy.spatial.distance import cdist
from matplotlib import cm

D = 50
gamma = 2.0

x = np.linspace(36,37,D)
y = np.linspace(32,33,D)

xx,yy = np.meshgrid(x,y)

m1 = np.array([[36.06122449,32.67346939]])
m2 = np.array([[36.71428571,32.32653061]])	       
X = np.vstack((m1,m2))
print (X)

w = np.array([[0.5],[0.5]])

xxx = xx.reshape(D*D)
yyy = yy.reshape(D*D)
a = np.vstack((xxx,yyy))

dd = np.dot(w.T, np.exp(-gamma * cdist(X,a.T)))
znew = dd.reshape(D,D)

fig = plt.figure()
ax = fig.gca(projection='3d')
surf = ax.plot_wireframe(xx, yy, znew)


def f(x_test):
    w1 = 0.5; w2 = 0.5
    d1 = np.sqrt((x_test[0]-m1[0][0])**2 + (x_test[1]-m1[0][1])**2)
    d2 = np.sqrt((x_test[0]-m2[0][0])**2 + (x_test[1]-m2[0][1])**2)
    y_new = w1*np.exp(-gamma * d1) + w2*np.exp(-gamma * d2)
    return y_new

x_test = [36.2, 32.2]
print (f(x_test))
ax.plot3D([x_test[0]], [x_test[1]], f(x_test),'rd')

plt.savefig('out1.png')
```

```text
[[36.06122449 32.67346939]
 [36.71428571 32.32653061]]
0.3597470320869875
```





















