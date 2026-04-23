import cv2
import sys
import numpy as np

class KalmanXZ:
    def __init__(self, dt):
        self.dt = dt

        # State: [X, Z, dX, dZ]
        self.x = np.zeros((4, 1))
        self.P = np.eye(4) * 10.0

        self.F = np.array([
            [1, 0, dt, 0],
            [0, 1, 0, dt],
            [0, 0, 1,  0],
            [0, 0, 0,  1]
        ])

        self.H = np.array([
            [1, 0, 0, 0],
            [0, 1, 0, 0]
        ])

        self.Q = np.eye(4) * 0.05
        self.R = np.eye(2) * 2.0
        self.I = np.eye(4)

    def predict(self):
        self.x = self.F @ self.x
        self.P = self.F @ self.P @ self.F.T + self.Q

    def update(self, z):
        z = z.reshape(2, 1)
        y = z - self.H @ self.x
        S = self.H @ self.P @ self.H.T + self.R
        K = self.P @ self.H.T @ np.linalg.inv(S)

        self.x = self.x + K @ y
        self.P = (self.I - K @ self.H) @ self.P

def kf_track(fin):

    cap = cv2.VideoCapture(fin)
    fps = cap.get(cv2.CAP_PROP_FPS)
    dt = 1.0 / fps

    # ---- Camera intrinsics ----
    K = np.array([
        [700.,   0., 300.],
        [  0., 700., 330.],
        [  0.,   0.,   1.]
    ])
    dist = np.zeros((5, 1))
    board_size = (3, 3)
    square_size = 1.0

    objp = np.zeros((board_size[0] * board_size[1], 3), np.float32)
    objp[:, :2] = np.mgrid[0:3, 0:3].T.reshape(-1, 2)
    objp *= square_size

    axis = np.float32([
        [0, 0, 0],
        [2, 0, 0],
        [0, 2, 0],
        [0, 0, -2]
    ])

    kf = KalmanXZ(dt)
    initialized = False

    raw_trace = []
    kf_trace = []

    frame_idx = 0

    while True:
        ret, frame = cap.read()
        if not ret:
            break

        gray = cv2.cvtColor(frame, cv2.COLOR_BGR2GRAY)
        found, corners = cv2.findChessboardCorners(gray, board_size)

        cv2.putText(frame,
                    f"Frame {frame_idx} | found={found}",
                    (20, 30),
                    cv2.FONT_HERSHEY_SIMPLEX,
                    0.8,
                    (0, 255, 0) if found else (0, 0, 255),
                    2)

        if found:
            corners = cv2.cornerSubPix(
                gray, corners, (5, 5), (-1, -1),
                (cv2.TERM_CRITERIA_EPS +
                 cv2.TERM_CRITERIA_MAX_ITER, 30, 0.01)
            )

            ok, rvec, tvec = cv2.solvePnP(objp, corners, K, dist)

            if ok:
                X, Z = float(tvec[0]), float(tvec[2])

                raw_trace.append((X, Z))

                if not initialized:
                    kf.x[0, 0] = X
                    kf.x[1, 0] = Z
                    initialized = True

                kf.predict()
                kf.update(np.array([X, Z]))

                kf_trace.append((kf.x[0, 0], kf.x[1, 0]))

                cv2.drawChessboardCorners(frame, board_size, corners, found)

                imgpts, _ = cv2.projectPoints(axis, rvec, tvec, K, dist)
                imgpts = imgpts.astype(int)
                o = tuple(imgpts[0].ravel())

                cv2.line(frame, o, tuple(imgpts[1].ravel()), (0, 0, 255), 3)
                cv2.line(frame, o, tuple(imgpts[2].ravel()), (0, 255, 0), 3)
                cv2.line(frame, o, tuple(imgpts[3].ravel()), (255, 0, 0), 3)

        cv2.imshow("Chessboard KF Tracking", frame)
        if cv2.waitKey(20) & 0xFF == 27:
            break

        frame_idx += 1

    cap.release()
    cv2.destroyAllWindows()

    with open("trajectory.csv", "w") as f:
        f.write("frame,raw_X,raw_Z,kf_X,kf_Z\n")
        for i, ((rx, rz), (kx, kz)) in enumerate(zip(raw_trace, kf_trace)):
            f.write(f"{i},{rx},{rz},{kx},{kz}\n")

    print(f"\nSaved trajectory.csv with {len(kf_trace)} samples")
    
if __name__ == "__main__":
    # "/opt/Downloads/skdata/chessb-left.avi"
    fin = sys.argv[1]
    kf_track(fin)
