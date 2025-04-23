import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D

fig = plt.figure(figsize=(10, 8))
ax = fig.add_subplot(111, projection='3d')

# min=(-0.03, 0.97, -0.23), max=(0.43, 1.43, 0.23))

x_min, y_min, z_min = (-0.03, 0.97, -0.23)
x_max, y_max, z_max = (0.43, 1.43, 0.23)

def plot_box(x_min, y_min, z_min, x_max, y_max, z_max, axx):
        
    axx.plot3D([x_min, x_max], [y_min, y_min], [z_min, z_min], 'y--')
    axx.plot3D([x_min, x_max], [y_max, y_max], [z_min, z_min], 'y--')
    axx.plot3D([x_min, x_max], [y_min, y_min], [z_max, z_max], 'y--')
    axx.plot3D([x_min, x_max], [y_max, y_max], [z_max, z_max], 'y--')
    axx.plot3D([x_min, x_min], [y_min, y_max], [z_min, z_min], 'y--')
    axx.plot3D([x_max, x_max], [y_min, y_max], [z_min, z_min], 'y--')
    axx.plot3D([x_min, x_min], [y_min, y_max], [z_max, z_max], 'y--')
    axx.plot3D([x_max, x_max], [y_min, y_max], [z_max, z_max], 'y--')
    axx.plot3D([x_min, x_min], [y_min, y_min], [z_min, z_max], 'y--')
    axx.plot3D([x_max, x_max], [y_min, y_min], [z_min, z_max], 'y--')
    axx.plot3D([x_min, x_min], [y_max, y_max], [z_min, z_max], 'y--')
    axx.plot3D([x_max, x_max], [y_max, y_max], [z_min, z_max], 'y--')

plot_box(x_min, y_min, z_min, x_max, y_max, z_max, ax)
    
plt.show()
