import cv2
import numpy as np

# --- CONFIG ---
K = 3
lambda_forget = 0.005
min_variance = 15.0
video_path = '/opt/Downloads/skdata/campus_vibe_video4.mp4'
resize_width = 640  
thresh_val = 30     
min_area = 500      

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

pi_g = np.ones((K, H, W), dtype=np.float32) / K

means = np.zeros((K, H, W, C), dtype=np.float32)
for k in range(K):
    noise = np.random.normal(scale=4.0*(k+1), size=(H,W,C)).astype(np.float32)
    means[k] = frame.astype(np.float32) + noise

covars = np.ones((K, H, W, C), dtype=np.float32) * 225.0

inv_covars = 1.0 / np.maximum(covars, min_variance)
det_covars = np.prod(covars, axis=-1, keepdims=True)

def diag_gauss_pdf(x, mean, inv_covar, det_covar):
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

    likelihoods = np.zeros((K, H, W), dtype=np.float32)
    for k in range(K):
        likelihoods[k] = diag_gauss_pdf(frame_f, means[k], inv_covars[k], det_covars[k])

    numerator = pi_g * likelihoods
    denominator = np.sum(numerator, axis=0, keepdims=True) + eps
    responsibilities = numerator / denominator  

    pi_g = pi_g + lambda_forget * (responsibilities - pi_g)
    pi_sum = np.sum(pi_g, axis=0, keepdims=True) + eps
    pi_g = pi_g / pi_sum

    for k in range(K):
        r_k = responsibilities[k]                
        pi_k = pi_g[k]                           
        denom = np.maximum(pi_k, eps)            
        ratio = (r_k / denom)[..., None]         
        delta = frame_f - means[k]               
        means[k] = means[k] + lambda_forget * ratio * delta
        delta_sq = delta * delta                 
        covars[k] = covars[k] + lambda_forget * ratio * (delta_sq - covars[k])
        covars[k] = np.maximum(covars[k], min_variance)
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
