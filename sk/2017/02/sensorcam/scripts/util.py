from PIL import Image, ImageFilter
from mpl_toolkits.mplot3d import axes3d
import pandas as pd
import time, io, cv2
import numpy as np, itertools
from scipy import linalg
import matplotlib.pyplot as plt
from PIL import Image, ImageDraw
import os, glob, re

def get_frame(dir, frame, hsv=True):
    data = np.fromfile(dir + "cam.bin", dtype=np.uint8)
    df = pd.read_csv(dir + "sizes.txt",header=None)
    dfgps = pd.read_csv(dir + "gps.txt",header=None,sep=",",names=['lat','lon','speed','acc','alt'])
    df['cum'] = df.cumsum()
    df['cum2'] = df.cum.shift(-1)
    df.columns = ['x','fr','to']    
    arr = data[int(df.ix[frame]['fr']) : int(df.ix[frame]['to'])]
    barr = io.BytesIO(arr)
    im = Image.open(barr)
    if hsv: return im.convert('HSV')
    return im

def same_sign(arr): return np.all(arr > 0) if arr[0] > 0 else np.all(arr < 0)

def inside_quad(pts, pt):
    a =  pts - pt
    d = np.zeros((4,2))
    d[0,:] = pts[1,:]-pts[0,:]
    d[1,:] = pts[2,:]-pts[1,:]
    d[2,:] = pts[3,:]-pts[2,:]
    d[3,:] = pts[0,:]-pts[3,:]
    res = np.cross(a,d)
    return same_sign(res), res

def plot_quad(cc, h, color):
    plot_line([cc[0][0],h-cc[0][1]],[cc[1][0],h-cc[1][1]], color) 
    plot_line([cc[1][0],h-cc[1][1]],[cc[2][0],h-cc[2][1]], color) 
    plot_line([cc[2][0],h-cc[2][1]],[cc[3][0],h-cc[3][1]], color) 
    plot_line([cc[3][0],h-cc[3][1]],[cc[0][0],h-cc[0][1]], color) 

def plot_line(pt1,pt2,color):
    plt.plot(np.array([pt1[0],pt2[0]]),np.array([pt1[1],pt2[1]]),color=color)
    
class VS:
    def __init__(self):
        self.last_T = 0.
        self.alpha = 0.9
        self.sx = 0; self.sy = 0; self.sa = 0
        self.x = 0; self.y = 0; self.a = 0
        self.prev = None
        self.prev_gray = None

    def stabilize(self, cur):
        if not np.any(self.prev):
            self.prev = cur
            self.prev_gray = cv2.cvtColor(self.prev, cv2.COLOR_RGB2GRAY)
            return cur
        (h,w) = self.prev.shape[:2]
        cur_gray = cv2.cvtColor(cur, cv2.COLOR_RGB2GRAY)
        prev_corner = cv2.goodFeaturesToTrack(self.prev_gray,
                                              maxCorners = 200,
                                              qualityLevel = 0.01,
                                              minDistance = 30.0,
                                              blockSize = 3)

        cur_corner, status, err = cv2.calcOpticalFlowPyrLK(self.prev_gray,
                                                           cur_gray,
                                                           prev_corner,
                                                           None)
        prev_corner2 = []
        cur_corner2 = []
        for i,st in enumerate(status):
            if st==1:
                prev_corner2.append(prev_corner[i])
                cur_corner2.append(cur_corner[i])
        prev_corner2 = np.array(prev_corner2)
        cur_corner2 = np.array(cur_corner2)
        T = cv2.estimateRigidTransform(prev_corner2, cur_corner2, False);
        last_T = T[:]
        dx = T[0,2];
        dy = T[1,2];
        da = np.arctan2(T[1,0], T[0,0])
        prev = cur[:]
        self.prev_gray = cur_gray[:]

        self.x += dx; self.y += dy; self.a += da

        self.sx = self.alpha*self.sx + (1-self.alpha)*self.x
        self.sy = self.alpha*self.sy + (1-self.alpha)*self.y
        self.sa = self.alpha*self.sa + (1-self.alpha)*self.a

        T[0,0] = np.cos(self.sa-self.a);
        T[0,1] = -np.sin(self.sa-self.a);
        T[1,0] = np.sin(self.sa-self.a);
        T[1,1] = np.cos(self.sa-self.a);
        T[0,2] = self.sx-self.x;
        T[1,2] = self.sy-self.y;
        cur2 = cv2.warpAffine(cur, T, (w,h));
        return cur2
    

    
