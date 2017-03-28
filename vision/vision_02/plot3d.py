from mpl_toolkits.mplot3d import axes3d
import matplotlib.pyplot as plt
import numpy as np

def plot_vector(fig, orig, v, color='blue'):
   ax = fig.gca(projection='3d')
   orig = np.array(orig); v=np.array(v)
   length = np.sqrt((v**2).sum())
   # quiver kodu ikinci uclu kordinati vektorun 'ucu' olarak aliyor, 
   # biz de burada baslangic noktasi ve vektor parametrelerini ona 
   # gore degistiriyoruz.
   tip = orig + v
   ax.quiver(tip[0], tip[1], tip[2], v[0], v[1], v[2],length=length,color=color)
   ax.set_xlim(0,10);ax.set_ylim(0,10);ax.set_zlim(0,10)
   ax = fig.gca(projection='3d')  
   return fig


def plot_plane(fig, point, normal, color='yellow'):
    point = np.array(point)
    normal = np.array(normal)
    # duzlem a*x+b*y+c*z+d=0 olarak temsil edilir, duzlemin normali
    # [a,b,c]. O yuzden d'yi hesaplamak lazim.
    d = -point.dot(normal)
    xx, yy = np.meshgrid(range(10), range(10))
    # x,y lere tekabul eden z'leri hesapla
    z = (-normal[0] * xx - normal[1] * yy - d) * 1. /normal[2]
    # yuzeyi grafikle
    plt3d = fig.gca(projection='3d')
    plt3d.plot_surface(xx, yy, z, alpha=0.3,color='yellow')
