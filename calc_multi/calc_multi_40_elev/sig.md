
```python
from mpl_toolkits.mplot3d import Axes3D
from scipy.spatial.distance import cdist
from matplotlib import cm

rho = 7.0
def sig(x,a):
   return (x-a)*1/(1+np.exp(-rho*(x-a)))


def gfunc(x, y):
    s1 = 2.2; x1 = 2.0; y1 = 2.0
    g1 = np.exp( -4 *np.log(2) * ((x-x1)**2+(y-y1)**2) / s1**2)
    return g1 * 10.0

def plot_surf_path(a1,a2,a3,a4,b1,b2,b3,b4):

    D = 50
    x = np.linspace(0,5,D)
    y = np.linspace(0,5,D)
    xx,yy = np.meshgrid(x,y)
    zz = gfunc(xx,yy)

    fig = plt.figure()
    ax = fig.gca(projection='3d')
    ax.set_xlim(0,5)
    ax.set_ylim(0,5)
    surf = ax.plot_wireframe(xx, yy, zz,rstride=10, cstride=10)

    t = np.linspace(0,5.0,100)

    def sigx(t):
        t = t[0]
        x = a1 + \
            a2*sig(t,1) + \
            a3*sig(t,2) + \
            a4*sig(t,3)
        return x
    
    def sigy(t):
        t = t[0]
        y = b1 + \
            b2*sig(t,1) + \
            b3*sig(t,2) + \
            b4*sig(t,3)
        return y


    xs = np.array([sigx([tt]) for tt in t])
    ys = np.array([sigy([tt]) for tt in t])

    #print (xs)
    #print (ys)
    
    ax.plot3D(xs, ys, gfunc(xs,ys),'r.')


#a1,a2,a3,a4,b1,b2,b3,b4=(0.58860821, -0.41960781,  0.58726455,  1.6640147,   4.90109649,  0.24970064, -0.06625096, -0.85057308)
a1,a2,a3,a4,b1,b2,b3,b4=-0.16110801,  2.29555687, -0.44309516, -1.84591699,  2.99303621,  2.99581171, -2.67317591, -2.97837766
plot_surf_path(a1,a2,a3,a4,b1,b2,b3,b4)
plt.savefig('/tmp/out.png')
```
