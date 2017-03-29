from numpy import *

K = array([[700., 0., 300.],
           [0., 700., 330.],
           [0., 0., 1.]])

def proj_board(im, xl, yl, z):
    h,w = im.shape[:2]
    for x in arange(xl-9, xl+9, 0.5):
        for y in arange(yl-9, yl+9, 0.5):
            X = array([x, y, z])
            q = dot(K, X)
            q = [int(q[0]/q[2]), int(q[1]/q[2])]           
            if q[0] >= w: return
            if h-q[1] >= h: return
            if h-q[1] < 0: return
            im[h-q[1], q[0]] = 255
