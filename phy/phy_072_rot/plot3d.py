from mpl_toolkits.mplot3d import axes3d
from matplotlib.patches import Circle, PathPatch
import matplotlib.pyplot as plt
from matplotlib.transforms import Affine2D
from mpl_toolkits.mplot3d import art3d
import numpy as np

def plot_vector(ax, orig, v, color='blue'):
   orig = np.array(orig); v=np.array(v)
   ax.quiver(orig[0], orig[1], orig[2], v[0], v[1], v[2],color=color)
   ax.set_xlim(0,10);ax.set_ylim(0,10);ax.set_zlim(0,10)

def rotation_matrix(d):
    sin_angle = np.linalg.norm(d)
    if sin_angle == 0:return np.identity(3)
    d /= sin_angle
    eye = np.eye(3)
    ddt = np.outer(d, d)
    skew = np.array([[    0,  d[2],  -d[1]],
                  [-d[2],     0,  d[0]],
                  [d[1], -d[0],    0]], dtype=np.float64)

    M = ddt + np.sqrt(1 - sin_angle**2) * (eye - ddt) + sin_angle * skew
    return M

def pathpatch_2d_to_3d(pathpatch, z, normal):
    if type(normal) is str: #Translate strings to normal vectors
        index = "xyz".index(normal)
        normal = np.roll((1.0,0,0), index)

    normal /= np.linalg.norm(normal) #Make sure the vector is normalised
    path = pathpatch.get_path() #Get the path and the associated transform
    trans = pathpatch.get_patch_transform()

    path = trans.transform_path(path) #Apply the transform

    new_pathpatch = art3d.PathPatch3D(
        path,
        zs=z,  # Pass the z-coordinate directly as 'zs' for all vertices
        zdir='z',
        facecolor=pathpatch.get_facecolor(),
        edgecolor=pathpatch.get_edgecolor(),
        fill=pathpatch.get_fill()
    )
    
    # Copy attributes from the original pathpatch to the new 3D one
    for prop in ['_facecolor3d', '_edgecolor3d', '_linewidth', '_alpha']:
        if hasattr(pathpatch, prop):
            setattr(new_pathpatch, prop, getattr(pathpatch, prop))
    
    # The _segment3d is now handled by PathPatch3D's internal z-coordinate
    # but the rotation needs to be applied to the vertices
    # Get the vertices in 2D after transformation
    verts = path.vertices 

    d = np.cross(normal, (0, 0, 1)) #Obtain the rotation vector    
    M = rotation_matrix(d) #Get the rotation matrix

    # Apply the rotation to the 2D vertices (with their Z=0 plane initial position)
    # The z-translation is applied separately by pathpatch_translate
    new_pathpatch._segment3d = np.array([np.dot(M, (x, y, 0)) for x, y in verts])
    
    return new_pathpatch

def pathpatch_translate(pathpatch, delta):
    # This translation will now be applied to the already rotated 3D segment
    pathpatch._segment3d += delta

def plot_plane(ax, point, normal, size=10, color='y'):    
    p = Circle((0, 0), size, facecolor = color, alpha = .2)
    # The 'z=0' here will be passed as the initial zs for the PathPatch3D
    p3d = pathpatch_2d_to_3d(p, z=0, normal=normal) 
    ax.add_patch(p3d)
    # The actual position of the plane (point) is handled by pathpatch_translate
    pathpatch_translate(p3d, (point[0], point[1], point[2]))
