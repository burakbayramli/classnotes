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
pickle.dump(your_mesh.vectors, open('prismhex.pkl', 'wb'))
```

```text
dış üçgen sayısı = 20
(20, 3, 3)
```

![](coll_01.jpg)










[devam edecek]

Kaynaklar

[1] Bayramlı, 
    <a href="https://burakbayramli.github.io/dersblog/calc_multi/calc_multi_75_app/green_in_teorisi_duzlem_kesismeleri_egriler.html">
    Green'in Teorisi, Düzlem Kesişmeleri, Eğriler
    </a>

[2] <a href="aabb-randall-tr.html">Randall, AABB Ağaçları ile Çarpışma Saptamasına Giriş</a>

[4] Bayramlı, <a href="../../2020/08/stl-3d-cad.html">3D Baskıya Hazır CAD Tasarım Formatı, STL</a>
