import sys
import time, io, cv2
import numpy as np
from PIL import Image, ImageDraw

class VS:
    def __init__(self):
        self.last_T = 0
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
    
vs = VS()

if len(sys.argv) < 2:
    print "Usage: vs.py [input file]"
    exit()
fin = sys.argv[1]
cap = cv2.VideoCapture(fin)

while True:
    status, im = cap.read()
    im2 = vs.stabilize(im)
    cv2.imshow('frame',im2)
    k = cv2.waitKey(10)
        
