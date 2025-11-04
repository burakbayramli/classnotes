import numpy as np, random, librosa, zipfile, io, os, pickle
from scipy.stats import multivariate_normal

# Config
SR = 16000
N_MFCC = 16
FEATURE_DIM = N_MFCC * 3   # MFCC + delta + delta2 = 39
N_COMPONENTS = 64
ALPHA = 1.0
ZFILE_PATH = "/opt/Downloads/gvoice/train.zip"
UBM_PICKLE = "ubm_gmm.pkl"
LABELS = ['no','nine','down','three']
MAX_PER_LABEL = 700
SEED = 0
random.seed(SEED); np.random.seed(SEED)

def extract_label(path_in_zip):
    return os.path.basename(os.path.dirname(path_in_zip))

def extract_frames(audio, sr):
    mfcc = librosa.feature.mfcc(y=audio, sr=sr, n_mfcc=N_MFCC)
    d1 = librosa.feature.delta(mfcc)
    d2 = librosa.feature.delta(mfcc, order=2)
    feats = np.vstack([mfcc, d1, d2]).T   # (T, 39)
    # CMVN
    mean = feats.mean(axis=0)
    std = feats.std(axis=0) + 1e-12
    feats = (feats - mean) / std
    return feats


def responsibilities(x, weights, means, covs):
    K = len(weights)
    p = np.zeros(K)
    for k in range(K):
        try:
            cov_diag = covs[k]
            diff = x - means[k]
            log_det = np.sum(np.log(cov_diag))
            exp_term = -0.5 * np.sum((diff**2) / cov_diag)
            p[k] = weights[k] * np.exp(-0.5 * FEATURE_DIM * np.log(2*np.pi) - 0.5*log_det + exp_term)
        except:
            p[k] = 1e-10
    denom = p.sum()
    if denom == 0:
        return np.ones(K) / K
    return p / denom


def initialize_gmm(K, D):
    weights = np.ones(K) / K
    means = np.random.randn(K, D) * 0.1
    covs = np.ones((K, D))  # diagonal covariance, start as 1
    return weights, means, covs


def update_gmm_incremental(x, weights, means, covs, m_total, alpha=1.0):
    K, D = means.shape
    r = responsibilities(x, weights, means, covs)
    m_total = alpha * m_total + 1.0
    for k in range(K):
        # weights
        weights[k] = (alpha * weights[k] * m_total + r[k]) / m_total
        # means
        mu_old = means[k].copy()
        lr = r[k] / (weights[k] * m_total + 1e-12)
        means[k] = mu_old + lr * (x - mu_old)
        # diag cov update
        diff = (x - mu_old)
        covs[k] = covs[k] + lr * ((diff**2) - covs[k])
        covs[k] = np.maximum(covs[k], 1e-6)  # avoid collapse
    weights /= weights.sum()
    return weights, means, covs, m_total

def update_gmm_incremental2(x, weights, means, covs, m_total, alpha=1.0):
    K, D = means.shape
    r = responsibilities(x, weights, means, covs)
    m_total = alpha * m_total + 1.0
    for k in range(K):
        mu_old = means[k].copy()
        lr = r[k] / (weights[k] * m_total + 1e-12)
        means[k] = mu_old + lr * (x - mu_old)

    weights /= weights.sum()
    return weights, means, covs, m_total


def main():
    weights, means, covs = initialize_gmm(N_COMPONENTS, FEATURE_DIM)
    m_total = 1.0
    counts = {l:0 for l in LABELS}
    processed = 0

    with zipfile.ZipFile(ZFILE_PATH, 'r') as z:
        files = [f for f in z.namelist() if f.endswith(".wav")]
        random.shuffle(files)
        files = files[0:int(len(files)*0.90)]
        for f in files:
            if os.path.basename(os.path.dirname(f)).startswith("_"): continue
            label = extract_label(f)
            if label not in LABELS: continue
            if counts[label] >= MAX_PER_LABEL: continue
            with z.open(f) as zf:
                audio, _ = librosa.load(io.BytesIO(zf.read()), sr=SR)
            frames = extract_frames(audio, SR)
            for x in frames:
                weights, means, covs, m_total = update_gmm_incremental(x, weights, means, covs, m_total, ALPHA)
            counts[label] += 1
            processed += 1
            if processed % 200 == 0:
                print(f"Processed {processed} files")
                print (counts)

    ubm = {'weights':weights, 'means':means, 'covs':covs,
           'n_components':N_COMPONENTS, 'feature_dim':FEATURE_DIM}
    with open(UBM_PICKLE, "wb") as f: pickle.dump(ubm, f)
    print("UBM saved:", UBM_PICKLE)


if __name__ == "__main__":
    main()
