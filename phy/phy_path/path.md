#+LaTeX_HEADER: \newcommand{\ud}{\,\mathrm{d}}
#+LaTeX_HEADER: \newcommand{\mlabel}[1]{\quad \text{(#1)}\quad}
#+LaTeX_HEADER: \usepackage{palatino,eulervm}
#+LaTeX_HEADER: \usepackage{cancel}

```python
plt.xlim(-10,10)
plt.ylim(-10,10)
plt.quiver(0,0,-4,4)
plt.savefig('out.png')
```

```python
from scipy.integrate import odeint

V = 1.0
h = 1.0
def wave(z, t):
    x,y,theta = z
    tmp1 = V*np.cos(theta) + -V*(y/h)
    tmp2 = V*np.sin(theta) 
    tmp3 = -np.cos(theta)**2*(-V/h)
    return [tmp1, tmp2, tmp3]

t = np.linspace(0, 10, 50)

z0 = [3.66,-1.86,np.deg2rad(105)]
sol = odeint(wave, z0, t)
idx = 27
thetas = sol[0:idx,2]
print (sol[idx,:])
print (np.rad2deg(sol[idx,2]))
print (t[idx])
print (np.rad2deg(thetas))
```

```text
[-0.04096147 -0.0363544   4.20008988]
240.64742384230138
5.510204081632653
[105.         105.82533902 106.74404915 107.77235927 108.93033001
 110.24297839 111.74181792 113.46692122 115.46973744 117.81688271
 120.59516753 123.91792862 127.932163   132.82416518 138.81708905
 146.14527764 154.97946458 165.28299453 176.64741416 188.27850714
 199.26873739 208.97172389 217.15287301 223.88529836 229.3803128
 233.87309753 237.57241404]
```

```python
plt.xlim(-0,5)
plt.ylim(-2,2)
for i in range(idx):
    x,y,theta = sol[i]
    plt.quiver(x,y,np.cos(theta),np.sin(theta))

plt.savefig('phy_path_02.png')
```
