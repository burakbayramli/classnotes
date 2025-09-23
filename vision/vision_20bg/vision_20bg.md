# Arkaplan (Background) Tespiti











```python
from PIL import Image
import numpy as np, cv2
import time, datetime

N = 400                    # "hafiza" faktoru (daha yuksek = daha yavas guncelleme)
bandwidth = 40.0           # Gaussian bant genisligi
num_bins = 32              # PDF temsil etmek icin kac tane nokta secelim
bin_centers = np.linspace(0, 255, num_bins).astype(np.float32)
alpha = 1/N

cap = cv2.VideoCapture('/opt/Downloads/skdata/campus_vibe_video4.mp4')
fps = int(cap.get(cv2.CAP_PROP_FPS))
print(f"Frame rate: {fps} FPS")

pdf_model = None
frame_index = 0
fig, axes = plt.subplots(nrows=4, ncols=2, figsize=(5,7))
g_row = 0
for k in range(3600):
    ret, frame = cap.read()
    gray_frame = cv2.cvtColor(frame, cv2.COLOR_BGR2GRAY).astype(np.float32)
    H, W = gray_frame.shape
    if pdf_model is None:
        pdf_model = np.ones((H, W, num_bins), dtype=np.float32) / num_bins

    diffs = gray_frame[..., None] - bin_centers[None, None, :]
    new_pdf = np.exp(-0.5 * (diffs / bandwidth) ** 2)
    new_pdf /= (new_pdf.sum(axis=-1, keepdims=True) + 1e-8)  # normalize
    pdf_model = (1 - alpha) * pdf_model + alpha * new_pdf
    if frame_index in [220,1200,1900,3500]:
        background_bins = pdf_model.argmax(axis=-1)  # index of most likely bin
        background = bin_centers[background_bins].astype(np.uint8)
        t = datetime.datetime.now()
        print(f"Frame {frame_index}, Time {t}: saving background snapshot")
        background_bins = pdf_model.argmax(axis=-1)  # index of most likely bin
        background = bin_centers[background_bins].astype(np.uint8)
        axes[g_row, 0].imshow(gray_frame, cmap='gray')
        axes[g_row, 1].imshow(background, cmap='gray')
        g_row = g_row + 1        

    frame_index += 1
    
plt.tight_layout(pad=0, w_pad=0, h_pad=0)
plt.savefig('vision_20bg_01.jpg')
cap.release()
cv2.destroyAllWindows()
```

![](vision_20bg_01.jpg)






[devam edecek]

Kaynaklar

[1] Evangelidis, *Parametric Image Alignment Using Enhanced Correlation Coefficient Maximization*

[2] [Video 1](https://www.dropbox.com/scl/fi/oczbpoicx243wd857doti/campus_vibe_video4.mp4?rlkey=0h026033fbwho59frsq3ewuu1&st=7axsjn6x&raw=1)

[3] Bayramli, *Istatistik, Parametresiz İstatistik (Nonparametric Statistics)*

