# AABB, Üçgen Kesişmesi, Hızlı Çarpışma Saptaması



```python
from stl import mesh
from mpl_toolkits import mplot3d
figure = plt.figure()
ax = mplot3d.Axes3D(figure)
your_mesh = mesh.Mesh.from_file('../../2020/08/shapes/Prism_hexagon.stl')
obj = mplot3d.art3d.Poly3DCollection(your_mesh.vectors)
obj.set_edgecolor('k')
ax.add_collection3d(obj)
scale = your_mesh.points.flatten()
ax.auto_scale_xyz(scale, scale, scale)
ax.set_xlim(40,60);ax.set_ylim(-5,15); ax.set_zlim(-4,20)
ax.view_init(elev=21, azim=40)
plt.savefig('coll_01.jpg')
```

```python
import pickle
print (u'dış üçgen sayısı =',len(your_mesh.vectors))
print (your_mesh.vectors.shape)
```

```text
dış üçgen sayısı = 20
(20, 3, 3)
```

![](coll_01.jpg)

```python
import sys; sys.path.append("randall")
import mpl_toolkits.mplot3d as a3
from stl import mesh
import AABB
        
class STLObj(AABB.IAABB):
    def __init__(self,offset):
        self.offset = offset
        self.init_triangles()

    def __repr__(self):
        return f"Obje offset {self.offset}"

    def init_triangles(self):
        m = mesh.Mesh.from_file('../../2020/08/shapes/Prism_hexagon.stl')
        self.triangles = m.vectors + self.offset
        
    def set_offset(self,offset):
        self.offset = offset
        self.init_triangles()

    def plot(self,ax):
        for x in self.triangles: 
            tri = a3.art3d.Poly3DCollection([x])
            tri.set_color('blue')
            tri.set_linestyle('dotted')
            tri.set_alpha(0.1)
            ax.add_collection3d(tri)

    def plot_aabb(self,axx):
        aabb = self.get_aabb()
        util.plot_box_imp(aabb.min_x, aabb.min_y, aabb.min_z,
                          aabb.max_x, aabb.max_y, aabb.max_z,axx)

    def get_aabb(self):
        tmp = np.vstack(self.triangles)
        mins = np.min(tmp,axis=0)
        maxs = np.max(tmp,axis=0)
        x,y,z,w,h,d = list(mins) + list(maxs)
        return AABB.AABB(x,y,z,w,h,d)
```

```python
o1 = STLObj(offset=np.array([0,0,0]))
o2 = STLObj(offset=np.array([5,-7,0]))
o3 = STLObj(offset=np.array([20,-5,15]))

ax = a3.Axes3D(plt.figure())        
o1.plot(ax)
o2.plot(ax)
o3.plot(ax)
ax.set_xlim(30,70);ax.set_ylim(-20,20); ax.set_zlim(-20,30)
ax.set_xlabel("x axis");ax.set_ylabel("y axis");ax.set_zlabel("z axis")
ax.view_init(elev=21, azim=200)
plt.savefig('coll_02.jpg')
```

![](coll_02.jpg)

```python
import util
ax = a3.Axes3D(plt.figure())        
o1.plot(ax); o1.plot_aabb(ax)
o2.plot(ax); o2.plot_aabb(ax)
o3.plot(ax); o3.plot_aabb(ax)
ax.set_xlim(30,70);ax.set_ylim(-20,20); ax.set_zlim(-20,30)
ax.set_xlabel("x axis");ax.set_ylabel("y axis");ax.set_zlabel("z axis")
ax.view_init(elev=21, azim=200)

plt.savefig('coll_03.jpg')
```


```python
tree = AABB.AABBTree(initial_size=10)
tree.insert_object(o1)
tree.insert_object(o2)
tree.insert_object(o3)
```

```python
print (o1, u'çarpışma testi')
overlaps = tree.query_overlaps(o1)
print (u'Sonuçlar:')
for obj in overlaps: print(f"  - Çakışma {obj} ile")
```

```text
Obje offset [0 0 0] çarpışma testi
Sonuçlar:
  - Çakışma Obje offset [ 5 -7  0] ile
```










[devam edecek]

Kaynaklar

[1] Bayramlı, 
    <a href="https://burakbayramli.github.io/dersblog/calc_multi/calc_multi_75_app/green_in_teorisi_duzlem_kesismeleri_egriler.html">
    Green'in Teorisi, Düzlem Kesişmeleri, Eğriler
    </a>

[2] <a href="aabb-randall-tr.html">Randall, AABB Ağaçları ile Çarpışma Saptamasına Giriş</a>

[4] Bayramlı, <a href="../../2020/08/stl-3d-cad.html">3D Baskıya Hazır CAD Tasarım Formatı, STL</a>

[5] Bayramli, 
    <a href="../../2000/10/nesnesel-programlama.html">Nesnesel Progralama</a>