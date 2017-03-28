import cv2
import sys; sys.path.append('../../tser/tser_kf')
from kalman_3d import *
from K import *

def proj_board(im, xl, yl, z):
    h,w = im.shape[:2]
    for x in arange(xl-9, xl+9, 0.5):
        for y in arange(yl-9, yl+9, 0.5):
            X = array([x, y, z])
            q = dot(K, X)
            q = [int(q[0]/q[2]), int(q[1]/q[2])]
            im[h-q[1], q[0]] = 255

dim = 3
if __name__ == "__main__":    
    fin = sys.argv[1]
    cap = cv2.VideoCapture(fin)
    kalman = Kalman(K, mu_init=array([1., 1., 165., 2.]))

    while (True):
        ret, frame = cap.read()
        h,w = frame.shape[:2]
        #proj_board(frame, 1, 1, 160)
        gray = cv2.cvtColor(frame, cv2.COLOR_BGR2GRAY)
        status, corners = cv2.findChessboardCorners( gray, (dim,dim))
        is_x = []; is_y = []
        if status: 
            cv2.drawChessboardCorners( gray, (dim,dim), corners, status)
            for p in corners:
                is_x.append(p[0][0])
                is_y.append(p[0][1])

        if len(is_x) > 0 : 
            kalman.update(array([is_x[5], h-is_y[5], 1.]))
            print 'projecting'
            proj_board(gray, 
                       kalman.mu_hat[0], 
                       kalman.mu_hat[1], 
                       kalman.mu_hat[2])
                
        cv2.imshow('frame',gray)
        if cv2.waitKey(20) & 0xFF == ord('q'):
            break        
    
       
