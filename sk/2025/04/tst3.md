
```python
from stl import mesh
from mpl_toolkits import mplot3d

# Create a new plot
figure = plt.figure()
axes = mplot3d.Axes3D(figure)

# Load the STL files and add the vectors to the plot
your_mesh = mesh.Mesh.from_file('Prism_triangle.stl')
your_mesh = mesh.Mesh.from_file('../../2020/08/shapes/Prism_hexagon.stl')
axes.add_collection3d(mplot3d.art3d.Poly3DCollection(your_mesh.vectors))

# Auto scale to the mesh size
scale = your_mesh.points.flatten()
axes.auto_scale_xyz(scale, scale, scale)

# Show the plot to the screen
plt.savefig('/tmp/out1.jpg')
```

```python
import pickle
print (len(your_mesh.vectors))
#print (your_mesh.vectors)
pickle.dump(your_mesh.vectors, open('prismhex.pkl', 'wb'))
```

```text
20
```









