

```python
def func(x, y):
    s1 = 2.2; x1 = 2.0; y1 = 2.0
    g1 = np.exp( -4 *np.log(2) * ((x-x1)**2+(y-y1)**2) / s1**2)
    return g1 

gamma = 2.0
D = 50

x = np.linspace(0,5,D)
y = np.linspace(0,5,D)

xx,yy = np.meshgrid(x,y)
zz = func(xx,yy)

fig = plt.figure()
ax = fig.gca(projection='3d')
surf = ax.plot_wireframe(xx, yy, zz)

plt.savefig('out1.png')
```
