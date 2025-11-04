import numpy as np, random, librosa, zipfile, io, os, pickle
from script1 import extract_label, extract_frames

from scipy.special import logsumexp

# Config
SR = 16000
ZFILE_PATH = "/opt/Downloads/gvoice/train.zip"
UBM_PICKLE = "ubm_gmm.pkl"
OUT_DIR = "models"
LABELS = ['no','nine','down','three']
MAX_PER_LABEL = 3000
#TAU = 16.0   # relevance factor for MAP
TAU = 8.0   # relevance factor for MAP
SEED = 0
random.seed(SEED); np.random.seed(SEED)


def compute_posteriors(frames, ubm):
    """Compute responsibilities r[t,k] for all frames."""
    w, mu, cov = ubm['weights'], ubm['means'], ubm['covs']
    T, D = frames.shape
    K = w.shape[0]
    log_prob = np.zeros((T, K))
    for k in range(K):
        diff = frames - mu[k]
        log_det = np.sum(np.log(cov[k]))
        exp_term = -0.5 * np.sum((diff**2) / cov[k], axis=1)
        log_prob[:, k] = np.log(w[k] + 1e-12) - 0.5 * (D * np.log(2*np.pi)) - 0.5 * log_det + exp_term
    log_norm = logsumexp(log_prob, axis=1, keepdims=True)
    log_resp = log_prob - log_norm
    return np.exp(log_resp)  # shape (T, K)


def map_adapt_class(frames_list, ubm, tau=16.0):
    """Perform MAP mean-only adaptation for one class."""
    w, mu, cov = ubm['weights'], ubm['means'], ubm['covs']
    K, D = mu.shape

    N_k = np.zeros(K)
    F_k = np.zeros((K, D))

    for frames in frames_list:
        r = compute_posteriors(frames, ubm)
        N_k += r.sum(axis=0)
        F_k += r.T @ frames

    adapted_means = np.zeros_like(mu)
    for k in range(K):
        Nk = N_k[k]
        if Nk > 0:
            xbar_k = F_k[k] / Nk
            adapted_means[k] = (Nk * xbar_k + tau * mu[k]) / (Nk + tau)
        else:
            adapted_means[k] = mu[k].copy()

    return {
        'weights': w.copy(),
        'means': adapted_means,
        'covs': cov.copy(),
        'n_components': K,
        'feature_dim': D
    }


def main():
    # Load UBM
    with open(UBM_PICKLE, "rb") as f:
        ubm = pickle.load(f)

    # Collect adaptation frames per label
    frames_dict = {lab: [] for lab in LABELS}
    counts = {lab: 0 for lab in LABELS}
    processed = 0

    with zipfile.ZipFile(ZFILE_PATH, 'r') as z:
        files = [f for f in z.namelist() if f.endswith(".wav")]
        random.shuffle(files)
        files = files[0:int(len(files)*0.90)]
        for fpath in files:
            if os.path.basename(os.path.dirname(fpath)).startswith("_"): 
                continue
            label = extract_label(fpath)
            if label not in LABELS: 
                continue
            if counts[label] >= MAX_PER_LABEL: 
                continue

            with z.open(fpath) as zf:
                audio, _ = librosa.load(io.BytesIO(zf.read()), sr=SR)
            frames = extract_frames(audio, SR)

            frames_dict[label].append(frames)
            counts[label] += 1
            processed += 1
            if processed % 200 == 0:
                print(f"Collected {processed} files")
                print(counts)

    # Perform MAP adaptation and save models
    os.makedirs(OUT_DIR, exist_ok=True)
    for lab in LABELS:
        model = map_adapt_class(frames_dict[lab], ubm, tau=TAU)
        with open(os.path.join(OUT_DIR, f"{lab}.pkl"), "wb") as f:
            pickle.dump(model, f)
        print(f"Saved MAP-adapted model for {lab}")

if __name__ == "__main__":
    main()
