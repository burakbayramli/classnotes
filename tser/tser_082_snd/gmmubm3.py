import numpy as np, librosa, zipfile, random, io, os, pickle
from scipy.special import logsumexp
from script1 import extract_frames

# Config
SR = 16000
N_MFCC = 16
USE_DELTAS = True
TAKE_LABELS = ['no', 'nine', 'down', 'three']
MODELS_DIR = "models/"
ZFILE_PATH = "/opt/Downloads/gvoice/train.zip"  # later: test.zip
SEED = 0

random.seed(SEED)
np.random.seed(SEED)


def extract_label(path_in_zip):
    return os.path.basename(os.path.dirname(path_in_zip))

def log_likelihood_utterance(frames, model):
    """Compute total log-likelihood of utterance under GMM"""
    w, m, c = model['weights'], model['means'], model['covs']
    K, D = m.shape
    T = frames.shape[0]
    log_probs = np.zeros((T, K))
    for k in range(K):
        diff = frames - m[k]
        log_det = np.sum(np.log(c[k]))
        exp_term = -0.5 * np.sum((diff**2) / c[k], axis=1)
        log_probs[:, k] = np.log(w[k] + 1e-12) \
                          - 0.5 * (D * np.log(2*np.pi)) \
                          - 0.5 * log_det + exp_term
    # log-sum-exp over components, then sum over frames
    return np.sum(logsumexp(log_probs, axis=1))


def load_models():
    models = {}
    for label in TAKE_LABELS:
        path = os.path.join(MODELS_DIR, f"{label}.pkl")
        with open(path, "rb") as f:
            models[label] = pickle.load(f)
        print(f"Loaded model for '{label}'")
    return models


def main():
    models = load_models()
    correct, total = 0, 0

    with zipfile.ZipFile(ZFILE_PATH, 'r') as z:
        files = [f for f in z.namelist() if f.endswith(".wav")]
        random.shuffle(files)
        files = files[int(len(files)*0.90):-1]
        for path in files:
            true_label = extract_label(path)
            if true_label not in TAKE_LABELS:
                continue
            with z.open(path) as zf:
                audio, _ = librosa.load(io.BytesIO(zf.read()), sr=SR)
            frames = extract_frames(audio, SR)

            # score against all models
            scores = {lab: log_likelihood_utterance(frames, models[lab]) for lab in TAKE_LABELS}
            pred_label = max(scores.items(), key=lambda x: x[1])[0]

            total += 1
            if pred_label == true_label:
                correct += 1

            if total % 50 == 0:
                print(f"Evaluated {total} files... current acc={correct/total:.3f}")

            if total >= 1000:  # quick test limit
                break

    acc = correct / total if total > 0 else 0
    print(f"\n=== FINAL EVALUATION ===")
    print(f"Accuracy: {acc:.4f} ({acc*100:.2f}%) on {total} files")


if __name__ == "__main__":
    main()
