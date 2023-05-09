import sys; sys.path.append('../../tser/tser_085_pf')
import cv2
import util
from PF import *

dim = 3
if __name__ == "__main__":    

    fin = sys.argv[1]
    cap = cv2.VideoCapture(fin)
    N = int(cap.get(cv2.CAP_PROP_FRAME_COUNT))

    pf = PF(util.K, 200)

    for i in range(N):
        ret, frame = cap.read()
        h,w = frame.shape[:2]
        gray = cv2.cvtColor(frame, cv2.COLOR_BGR2GRAY)
        status, corners = cv2.findChessboardCorners( gray, (dim,dim))
        is_x = []; is_y = []
        if status: 
            cv2.drawChessboardCorners( gray, (dim,dim), corners, status)
            for p in corners:
                is_x.append(p[0][0])
                is_y.append(p[0][1])

        if len(is_x) > 0: 
            pf.update(array([is_x[5], h-is_y[5], 1.]))
            mu_x = pf.average()
            util.proj_board(gray, mu_x[0], mu_x[1], mu_x[2])

        cv2.imshow('frame',gray)
        if cv2.waitKey(20) & 0xFF == ord('q'):
            break        
    
      
