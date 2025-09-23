import cv2, numpy as np

# --- CONFIG ---
K = 3
lambda_forget = 0.995
min_variance = 15.0
video_path = '/opt/Downloads/skdata/campus_vibe_video4.mp4'
resize_width = 640  # for speed; set None to keep original size
thresh_val = 30     # threshold for foreground detection
min_area = 500      # minimum area for contour to be considered an object

# --- Open video ---
cap = cv2.VideoCapture(video_path)
ret, frame = cap.read()
if not ret:
    print("Failed to read video")
    cap.release()
    exit()

if resize_width is not None:
    h0, w0 = frame.shape[:2]
    scale = resize_width / float(w0)
    frame = cv2.resize(frame, (resize_width, int(h0 * scale)))

H, W, C = frame.shape

# --- Initialize GMM state ---
means = np.zeros((K, H, W, C), dtype=np.float32)
vars_ = np.ones((K, H, W, C), dtype=np.float32) * 225.0
weights = np.ones((K, H, W), dtype=np.float32) / K
N_g = np.ones((K, H, W), dtype=np.float32)

M1 = np.zeros((K, H, W, C), dtype=np.float32)
M2 = np.zeros((K, H, W, C), dtype=np.float32)
for kk in range(K):
    noise = np.random.normal(scale=4.0*(kk+1), size=(H,W,C)).astype(np.float32)
    means[kk] = frame.astype(np.float32) + noise
    M1[kk] = N_g[kk,:,:,None] * means[kk]
    M2[kk] = N_g[kk,:,:,None] * (vars_[kk] + means[kk]**2)

print("Initialized GMM from first frame")

def diag_gauss_pdf(x, mean, var):
    eps = 1e-6
    denom = np.sqrt(2*np.pi*(var+eps))
    exponent = -0.5*((x-mean)**2)/(var+eps)
    return np.prod(np.exp(exponent)/denom, axis=-1)

while True:
    ret, frame = cap.read()
    if not ret:
        break
    if resize_width is not None:
        frame = cv2.resize(frame, (resize_width, int(frame.shape[0]*resize_width/frame.shape[1])))
    frame_f = frame.astype(np.float32)

    # Expand for broadcasting
    xk = np.expand_dims(frame_f, 0)

    # Likelihoods per component
    px_given_k = diag_gauss_pdf(xk, means, vars_)  # (K,H,W)
    numer = weights * px_given_k
    denom = np.sum(numer, axis=0) + 1e-12
    responsibilities = numer / denom

    # Forgetting updates
    lam = lambda_forget
    rkc = responsibilities[...,None]
    N_g = lam*N_g + responsibilities
    M1 = lam*M1 + rkc*xk
    M2 = lam*M2 + rkc*(xk*xk)

    N_safe = np.maximum(N_g[...,None], 1e-6)
    means = M1 / N_safe
    vars_ = (M2 / N_safe) - means**2
    vars_ = np.maximum(vars_, min_variance)

    total = np.sum(N_g, axis=0, keepdims=True) + 1e-12
    weights = N_g / total

    # Argmax background
    k_star = np.argmax(weights, axis=0)
    rows, cols = np.indices((H, W))
    background = means[k_star, rows, cols].astype(np.uint8)

    # Foreground mask
    fgmask = cv2.absdiff(frame, background)
    fgmask = cv2.cvtColor(fgmask, cv2.COLOR_BGR2GRAY)
    _, fgmask = cv2.threshold(fgmask, thresh_val, 255, cv2.THRESH_BINARY)

    # Morphological cleanup
    k = cv2.getStructuringElement(cv2.MORPH_ELLIPSE, (3,3))
    fgmask = cv2.morphologyEx(fgmask, cv2.MORPH_OPEN, k, iterations=2)

    # Find contours
    contours, _ = cv2.findContours(fgmask, cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_SIMPLE)
    for cnt in contours:
        if cv2.contourArea(cnt) < min_area:
            continue
        x,y,w,h = cv2.boundingRect(cnt)
        cv2.rectangle(frame, (x,y), (x+w, y+h), (0,0,255), 2)

    # Show live results
    cv2.imshow("Foreground Mask", fgmask)
    cv2.imshow("Detections", frame)

    key = cv2.waitKey(1) & 0xFF
    if key == ord('q'):
        break

cap.release()
cv2.destroyAllWindows()
