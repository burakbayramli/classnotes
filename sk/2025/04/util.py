import numpy as np, itertools

def pairwise(iterable):
    a, b = itertools.tee(iterable)
    return itertools.zip_longest(a, b, fillvalue=next(b, None))

def plot_box_imp(x_min, y_min, z_min, x_max, y_max, z_max, axx):        
    axx.plot3D([x_min, x_max], [y_min, y_min], [z_min, z_min], 'y')
    axx.plot3D([x_min, x_max], [y_max, y_max], [z_min, z_min], 'y')
    axx.plot3D([x_min, x_max], [y_min, y_min], [z_max, z_max], 'y')
    axx.plot3D([x_min, x_max], [y_max, y_max], [z_max, z_max], 'y')
    axx.plot3D([x_min, x_min], [y_min, y_max], [z_min, z_min], 'y')
    axx.plot3D([x_max, x_max], [y_min, y_max], [z_min, z_min], 'y')
    axx.plot3D([x_min, x_min], [y_min, y_max], [z_max, z_max], 'y')
    axx.plot3D([x_max, x_max], [y_min, y_max], [z_max, z_max], 'y')
    axx.plot3D([x_min, x_min], [y_min, y_min], [z_min, z_max], 'y')
    axx.plot3D([x_max, x_max], [y_min, y_min], [z_min, z_max], 'y')
    axx.plot3D([x_min, x_min], [y_max, y_max], [z_min, z_max], 'y')
    axx.plot3D([x_max, x_max], [y_max, y_max], [z_min, z_max], 'y')
