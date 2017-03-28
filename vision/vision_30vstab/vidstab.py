#!/usr/bin/env python
import cv2, sys
import numpy as np
import pandas as pd

if len(sys.argv) < 2:
    print "Usage: vs.py [input file]"
    exit()
fin = sys.argv[1]
cap = cv2.VideoCapture(fin)
N = int(cap.get(cv2.CAP_PROP_FRAME_COUNT))
fps = int(cap.get(cv2.CAP_PROP_FPS))

status, prev = cap.read()
prev_gray = cv2.cvtColor(prev, cv2.COLOR_BGR2GRAY)
(h,w) = prev.shape[:2]

last_T = None
prev_to_cur_transform = []
for k in range(N-1):
    status, cur = cap.read()
    cur_gray = cv2.cvtColor(cur, cv2.COLOR_BGR2GRAY)
    prev_corner = cv2.goodFeaturesToTrack(prev_gray,
                                          maxCorners = 200,
                                          qualityLevel = 0.01,
                                          minDistance = 30.0,
                                          blockSize = 3)
    cur_corner, status, err = cv2.calcOpticalFlowPyrLK(prev_gray,
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
    prev_to_cur_transform.append([dx, dy, da])
    prev = cur[:]
    prev_gray = cur_gray[:]


prev_to_cur_transform = np.array(prev_to_cur_transform)
trajectory = np.cumsum(prev_to_cur_transform, axis=0)
trajectory = pd.DataFrame(trajectory)
smoothed_trajectory = pd.rolling_mean(trajectory,window=30)
smoothed_trajectory = smoothed_trajectory.fillna(method='bfill')
new_prev_to_cur_transform = prev_to_cur_transform + \
                            (smoothed_trajectory - trajectory)
new_prev_to_cur_transform = np.array(new_prev_to_cur_transform)

T = np.zeros((2,3))
cap = cv2.VideoCapture(fin)
out = cv2.VideoWriter('out.avi', cv2.VideoWriter_fourcc('P','I','M','1'),
                      fps, (w, h), True)

for k in range(N-1):
    status, cur = cap.read()
    T[0,0] = np.cos(new_prev_to_cur_transform[k][2]);
    T[0,1] = -np.sin(new_prev_to_cur_transform[k][2]);
    T[1,0] = np.sin(new_prev_to_cur_transform[k][2]);
    T[1,1] = np.cos(new_prev_to_cur_transform[k][2]);
    T[0,2] = new_prev_to_cur_transform[k][0];
    T[1,2] = new_prev_to_cur_transform[k][1];
    cur2 = cv2.warpAffine(cur, T, (w,h));
    out.write(cur2);
    cv2.waitKey(20);
