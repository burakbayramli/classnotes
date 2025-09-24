import cv2
import numpy as np

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

# --- Initialize GMM state according to paper ---
# Initialize with first frame data
m = 1  # sample count as in paper

# Mixing probabilities - uniform initialization
pi_g = np.ones((K, H, W), dtype=np.float32) / K

# Means - initialize with first frame + small noise
means = np.zeros((K, H, W, C), dtype=np.float32)
for k in range(K):
    noise = np.random.normal(scale=4.0*(k+1), size=(H,W,C)).astype(np.float32)
    means[k] = frame.astype(np.float32) + noise

# Covariances - initialize with identity-like matrix
# Using diagonal covariance for efficiency as in your original code
covars = np.ones((K, H, W, C), dtype=np.float32) * 225.0

# Precompute inverse covariances and determinants for efficiency
inv_covars = 1.0 / np.maximum(covars, min_variance)
det_covars = np.prod(covars, axis=-1, keepdims=True)

print("Initialized RGMM from first frame")

def diag_gauss_pdf(x, mean, inv_covar, det_covar):
    """Calculate Gaussian PDF with diagonal covariance"""
    eps = 1e-6
    exponent = -0.5 * np.sum((x - mean)**2 * inv_covar, axis=-1)
    denom = np.sqrt((2*np.pi)**C * np.maximum(det_covar.squeeze(-1), eps))
    return np.exp(exponent) / np.maximum(denom, eps)

while True:
    ret, frame = cap.read()
    if not ret:
        break
        
    if resize_width is not None:
        frame = cv2.resize(frame, (resize_width, int(frame.shape[0]*resize_width/frame.shape[1])))
    
    frame_f = frame.astype(np.float32)
    m += 1  # Increment sample count
    
    # Step 1: Calculate responsibilities (posterior probabilities)
    # Equation 16 from paper
    likelihoods = np.zeros((K, H, W))
    for k in range(K):
        likelihoods[k] = diag_gauss_pdf(frame_f, means[k], inv_covars[k], det_covars[k])
    
    numerator = pi_g * likelihoods
    denominator = np.sum(numerator, axis=0) + 1e-12
    responsibilities = numerator / denominator  # p(C_g | x_m)
    
    # Step 2: Update model parameters with forgetting factor
    # Equations 13-15 from paper (adapted for diagonal covariance)
    for k in range(K):
        # Update mixing probabilities (Equation 13)
        r_k = responsibilities[k]
        pi_g[k] = pi_g[k] + (1.0/m) * (r_k - pi_g[k])
    
    # Normalize mixing probabilities (Equation 10)
    pi_sum = np.sum(pi_g, axis=0) + 1e-12
    pi_g = pi_g / pi_sum
    
    for k in range(K):
        r_k = responsibilities[k]
        
        # Update means (Equation 14)
        delta = frame_f - means[k]
        weight_factor = r_k[..., None] / np.maximum(pi_g[k][..., None], 1e-12)
        means[k] = means[k] + (weight_factor * delta) / m
        
        # Update covariances with forgetting factor (Equation 15 adapted for diagonal)
        # For diagonal covariance, we only update the variance terms
        beta = (lambda_forget * r_k / np.maximum(pi_g[k], 1e-12))[..., None]
        
        # Element-wise variance update (diagonal approximation)
        delta_sq = delta ** 2
        covars[k] = (1 - beta) * covars[k] + beta * delta_sq
        
        # Ensure minimum variance
        covars[k] = np.maximum(covars[k], min_variance)
        
        # Update inverse covariances and determinants
        inv_covars[k] = 1.0 / covars[k]
        det_covars[k] = np.prod(covars[k], axis=-1, keepdims=True)
    
    # Step 3: Background extraction and foreground detection
    # Use component with highest weight as background
    k_bg = np.argmax(pi_g, axis=0)
    rows, cols = np.indices((H, W))
    background = means[k_bg, rows, cols].astype(np.uint8)
    
    # Foreground mask
    fgmask = cv2.absdiff(frame, background)
    fgmask = cv2.cvtColor(fgmask, cv2.COLOR_BGR2GRAY)
    _, fgmask = cv2.threshold(fgmask, thresh_val, 255, cv2.THRESH_BINARY)
    
    # Morphological cleanup
    kernel = cv2.getStructuringElement(cv2.MORPH_ELLIPSE, (3, 3))
    fgmask = cv2.morphologyEx(fgmask, cv2.MORPH_OPEN, kernel, iterations=2)
    
    # Find contours and draw bounding boxes
    contours, _ = cv2.findContours(fgmask, cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_SIMPLE)
    for cnt in contours:
        if cv2.contourArea(cnt) < min_area:
            continue
        x, y, w, h = cv2.boundingRect(cnt)
        cv2.rectangle(frame, (x, y), (x + w, y + h), (0, 0, 255), 2)
    
    # Display results
    cv2.imshow("Foreground Mask", fgmask)
    cv2.imshow("Detections", frame)
    
    key = cv2.waitKey(1) & 0xFF
    if key == ord('q'):
        break

cap.release()
cv2.destroyAllWindows()
