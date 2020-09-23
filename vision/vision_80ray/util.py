import pandas as pd, io
from PIL import Image
import os, glob, re, zipfile, cv2
import pandas as pd, pickle
import numpy as np, itertools
import matplotlib.pyplot as plt

def plot_line(pt1,pt2,color):
    plt.plot(np.array([pt1[0],pt2[0]]),np.array([pt1[1],pt2[1]]),color=color)
    
def inside_quad(pts, pt):
    a =  pts - pt
    d = np.zeros((4,2))
    d[0,:] = pts[1,:]-pts[0,:]
    d[1,:] = pts[2,:]-pts[1,:]
    d[2,:] = pts[3,:]-pts[2,:]
    d[3,:] = pts[0,:]-pts[3,:]
    res = np.cross(a,d)
    return same_sign(res), res

def same_sign(arr): return np.all(arr > 0) if arr[0] > 0 else np.all(arr < 0)

def plot_quad(cc, h, color):
    plot_line([cc[0][0],h-cc[0][1]],[cc[1][0],h-cc[1][1]], color) 
    plot_line([cc[1][0],h-cc[1][1]],[cc[2][0],h-cc[2][1]], color) 
    plot_line([cc[2][0],h-cc[2][1]],[cc[3][0],h-cc[3][1]], color) 
    plot_line([cc[3][0],h-cc[3][1]],[cc[0][0],h-cc[0][1]], color) 
