import cv2
import numpy as np

# --- CONFIG ---
K = 3
lambda_forget = 0.1
min_variance = 15.0
video_path = '/opt/Downloads/skdata/campus_vibe_video4.mp4'
resize_width = 640  # for speed; set None to keep original size
thresh_val = 30     # threshold for foreground detection
min_area = 500      # minimum area for contour to be considered an object

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

m = 1  # sample count as in paper

pi_g = np.ones((K, H, W), dtype=np.float32) / K

means = np.zeros((K, H, W, C), dtype=np.float32)
for k in range(K):
    noise = np.random.normal(scale=4.0*(k+1), size=(H,W,C)).astype(np.float32)
    means[k] = frame.astype(np.float32) + noise

covars = np.ones((K, H, W, C), dtype=np.float32) * 225.0

inv_covars = 1.0 / np.maximum(covars, min_variance)
det_covars = np.prod(covars, axis=-1, keepdims=True)

print("Initialized RGMM from first frame")

def diag_gauss_pdf(x, mean, inv_covar, det_covar):
    """Calculate Gaussian PDF with diagonal covariance"""
    eps = 1e-6
    exponent = -0.5 * np.sum((x - mean)**2 * inv_covar, axis=-1)
    denom = np.sqrt((2*np.pi)**C * np.maximum(det_covar.squeeze(-1), eps))
    return np.exp(exponent) / np.maximum(denom, eps)

eps = 1e-12

while True:
    ret, frame = cap.read()
    if not ret:
        break
        
    if resize_width is not None:
        frame = cv2.resize(frame, (resize_width, int(frame.shape[0]*resize_width/frame.shape[1])))
    
    frame_f = frame.astype(np.float32)


    # --- compute likelihoods and responsibilities (using current params) ---
    likelihoods = np.zeros((K, H, W), dtype=np.float32)
    for k in range(K):
        likelihoods[k] = diag_gauss_pdf(frame_f, means[k], inv_covars[k], det_covars[k])

    numerator = pi_g * likelihoods  # shape (K,H,W)
    denominator = np.sum(numerator, axis=0, keepdims=True) + eps
    responsibilities = numerator / denominator  # shape (K,H,W), r_g(x_m)

    # --- 1) update mixing probabilities (Zheng eq.13) ---
    # pi_g_new = pi_g + lambda * (r - pi_g)
    pi_g = pi_g + lambda_forget * (responsibilities - pi_g)
    # numerical guard and normalization
    pi_sum = np.sum(pi_g, axis=0, keepdims=True) + eps
    pi_g = pi_g / pi_sum  # now sum_g pi_g == 1 at each pixel

    # --- 2) update means and covariances using updated pi_g (Zheng eqs.14-15) ---
    for k in range(K):
        r_k = responsibilities[k]                # shape (H,W)
        pi_k = pi_g[k]                           # updated mixing prob (H,W)
        # avoid division by zero: denom = max(pi_k, eps)
        denom = np.maximum(pi_k, eps)            # (H,W)
        ratio = (r_k / denom)[..., None]         # shape (H,W,1) to broadcast over channels

        # mean update: mu <- mu + lambda * (r / pi) * (x - mu)
        delta = frame_f - means[k]               # shape (H,W,C)
        means[k] = means[k] + lambda_forget * ratio * delta

        # covariance update (diagonal): Σ <- Σ + λ * (r/π) * [ (x - μ)(x - μ) - Σ ]
        delta_sq = delta * delta                 # elementwise squared (H,W,C)
        covars[k] = covars[k] + lambda_forget * ratio * (delta_sq - covars[k])

        # enforce minimum variance (per channel)
        covars[k] = np.maximum(covars[k], min_variance)

        # update inverses and determinants used to compute likelihoods next frame
        inv_covars[k] = 1.0 / covars[k]
        det_covars[k] = np.prod(covars[k], axis=-1, keepdims=True)

    
    k_bg = np.argmax(pi_g, axis=0)
    rows, cols = np.indices((H, W))
    background = means[k_bg, rows, cols].astype(np.uint8)
    
    fgmask = cv2.absdiff(frame, background)
    fgmask = cv2.cvtColor(fgmask, cv2.COLOR_BGR2GRAY)
    _, fgmask = cv2.threshold(fgmask, thresh_val, 255, cv2.THRESH_BINARY)
    
    kernel = cv2.getStructuringElement(cv2.MORPH_ELLIPSE, (3, 3))
    fgmask = cv2.morphologyEx(fgmask, cv2.MORPH_OPEN, kernel, iterations=2)
    
    contours, _ = cv2.findContours(fgmask, cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_SIMPLE)
    for cnt in contours:
        if cv2.contourArea(cnt) < min_area:
            continue
        x, y, w, h = cv2.boundingRect(cnt)
        cv2.rectangle(frame, (x, y), (x + w, y + h), (0, 0, 255), 2)
    
    cv2.imshow("Foreground Mask", fgmask)
    cv2.imshow("Detections", frame)
    
    key = cv2.waitKey(1) & 0xFF
    if key == ord('q'):
        break

cap.release()
cv2.destroyAllWindows()
