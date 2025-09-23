import matplotlib.pyplot as plt
import numpy as np, cv2
import time, datetime

# --- CONFIG ---
K = 3                    # number of GMM components per pixel
lambda_forget = 0.995    # forgetting factor (1.0 -> no forgetting)
min_variance = 15.0      # floor for variance
snapshot_frames = [220, 1200, 1900, 3500]  # frames where we snapshot background

# --- Open video ---
cap = cv2.VideoCapture('/opt/Downloads/skdata/campus_vibe_video4.mp4')
fps = int(cap.get(cv2.CAP_PROP_FPS))
print(f"Frame rate: {fps} FPS")

frame_index = 0
fig, axes = plt.subplots(nrows=4, ncols=2, figsize=(6,8))
g_row = 0

# GMM state
gmm_initialized = False
means = vars_ = weights = N_g = M1 = M2 = None

for k in range(3600):
    ret, frame = cap.read()
    if not ret:
        break
    frame = frame.astype(np.float32)
    H, W, C = frame.shape

    if not gmm_initialized:
        # Initialize
        means = np.zeros((K, H, W, C), dtype=np.float32)
        vars_ = np.ones((K, H, W, C), dtype=np.float32) * 225.0
        weights = np.ones((K, H, W), dtype=np.float32) / K
        N_g = np.ones((K, H, W), dtype=np.float32)

        M1 = np.zeros((K, H, W, C), dtype=np.float32)
        M2 = np.zeros((K, H, W, C), dtype=np.float32)
        for kk in range(K):
            noise = np.random.normal(scale=4.0*(kk+1), size=(H,W,C)).astype(np.float32)
            means[kk] = frame + noise
            M1[kk] = N_g[kk,:,:,None] * means[kk]
            M2[kk] = N_g[kk,:,:,None] * (vars_[kk] + means[kk]**2)
        gmm_initialized = True
        print("Initialized GMM from first frame")

    # Expand dims for broadcasting
    xk = np.expand_dims(frame, 0)  # (1,H,W,C)

    # Likelihoods per component (diagonal Gaussians)
    def diag_gauss_pdf(x, mean, var):
        eps = 1e-6
        denom = np.sqrt(2*np.pi*(var+eps))
        exponent = -0.5*((x-mean)**2)/(var+eps)
        return np.prod(np.exp(exponent)/denom, axis=-1)

    px_given_k = diag_gauss_pdf(xk, means, vars_)  # (K,H,W)
    numer = weights * px_given_k
    denom = np.sum(numer, axis=0) + 1e-12
    responsibilities = numer / denom  # (K,H,W)

    # Forgetting update
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

    # Background estimate = weighted mean
    background = np.sum(means * weights[...,None], axis=0).astype(np.uint8)
    frame_uint8 = frame.astype(np.uint8)

    if frame_index in snapshot_frames:
        t = datetime.datetime.now()
        print(f"Frame {frame_index}, Time {t}: saving background snapshot")
        axes[g_row, 0].imshow(cv2.cvtColor(frame_uint8, cv2.COLOR_BGR2RGB))
        axes[g_row, 1].imshow(cv2.cvtColor(background, cv2.COLOR_BGR2RGB))
        g_row += 1

    frame_index += 1

plt.tight_layout(pad=0, w_pad=0, h_pad=0)
plt.savefig('/tmp/vision_gmm_bg.jpg')
cap.release()
cv2.destroyAllWindows()
