import sys; sys.path.append('../../tser/tser_083_kf')
import util
from kalman_3d import *
import cv2

dim = 3
if __name__ == "__main__":    
    fin = sys.argv[1]
    cap = cv2.VideoCapture(fin)
    N = int(cap.get(cv2.CAP_PROP_FRAME_COUNT))
    fps = int(cap.get(cv2.CAP_PROP_FPS))
    kalman = Kalman(util.K, mu_init=array([1., 1., 165., 0.5]))
    
    for i in range(N):
        ret, frame = cap.read()
        h,w = frame.shape[:2]
        #proj_board(frame, 1, 1, 160) # basla
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
            util.proj_board(gray, 
                            kalman.mu_hat[0], 
                            kalman.mu_hat[1], 
                            kalman.mu_hat[2])
        if i % 10 == 0: 
            cv2.imwrite('/tmp/kf-out-%d.jpg' % i, gray)
        cv2.imshow('frame',gray)
        cv2.waitKey(20)
    
