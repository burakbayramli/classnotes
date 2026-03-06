import csv, json, time, os
import numpy as np, sys
from scipy.special import gammaln
from scipy.sparse import lil_matrix, diags
import umap

csv.field_size_limit(sys.maxsize)

#d = "/opt/Downloads/ml-latest-small"
d = "/opt/Downloads/ml-32m"
USER_MOVIE_FILE = d + "/user_movie.txt"
N_CLUSTERS      = 10                  # K  (try 4-10)
N_ITER          = 400                # Gibbs sweeps
BURN_IN         = 150                # discard first N sweeps
SEED            = 42
MIN_RATINGS     = 5                  # drop users with fewer ratings
np.random.seed(SEED)

from scipy.sparse import lil_matrix, csr_matrix
from scipy.sparse.linalg import norm as sparse_norm

# ── Pass 1: collect vocabulary (users + movies), no ratings kept in memory ───
print("Pass 1: scanning vocabulary …")
user_rating_counts = {}
all_movies_set = set()
with open(USER_MOVIE_FILE, newline="") as csvfile:
    rd = csv.reader(csvfile, delimiter="|")
    for row in rd:
        uid = int(row[0])
        ratings = json.loads(row[1])
        if len(ratings) >= MIN_RATINGS:
            user_rating_counts[uid] = len(ratings)
            all_movies_set.update(int(k) for k in ratings)

users     = sorted(user_rating_counts.keys())
user_idx  = {u: i for i, u in enumerate(users)}
all_movies = sorted(all_movies_set)
movie_idx  = {m: i for i, m in enumerate(all_movies)}
del all_movies_set, user_rating_counts

N = len(users)
M = len(all_movies)
print(f"  {N} users, {M} movies")

# ── Pass 2: fill sparse matrix row by row ────────────────────────────────────
print("Pass 2: building sparse rating matrix …")
R_sparse = lil_matrix((N, M), dtype=np.float32)
with open(USER_MOVIE_FILE, newline="") as csvfile:
    rd = csv.reader(csvfile, delimiter="|")
    for row in rd:
        uid = int(row[0])
        if uid not in user_idx:
            continue
        i = user_idx[uid]
        ratings = json.loads(row[1])
        for mid, v in ratings.items():
            mid = int(mid)
            if mid in movie_idx:
                R_sparse[i, movie_idx[mid]] = float(v)

# Convert to CSR and normalize rows (sparse-safe)
print("  Converting to CSR and normalizing …")
R_sparse = R_sparse.tocsr()
row_norms = np.asarray(R_sparse.power(2).sum(axis=1)).squeeze() ** 0.5
row_norms[row_norms == 0] = 1.0
# Divide each row by its norm via diagonal scaling
from scipy.sparse import diags
R_norm = diags(1.0 / row_norms) @ R_sparse   # stays sparse
del R_sparse

print(f"  Rating matrix: {N} users × {M} movies  "
      f"(nnz={R_norm.nnz}, density={R_norm.nnz/(N*M)*100:.3f}%)")

print("Embedding via UMAP …")

reducer = umap.UMAP(
    metric="cosine",
    n_components=2,
    n_neighbors=30,       # reduce from 50, still fine for 200k
    n_epochs=200,         # default is 500 for large datasets, halving saves ~40%
    low_memory=True,      # trades some speed for memory, but avoids swapping
    random_state=SEED
)

UMAP_CACHE = d + "/umap_embedding.npy"
if os.path.exists(UMAP_CACHE):
    print("Loading cached UMAP embedding …")
    X = np.load(UMAP_CACHE)
else:
    print("Embedding via UMAP …")
    X = reducer.fit_transform(R_norm)
    np.save(UMAP_CACHE, X)


K      = N_CLUSTERS
D      = 2
alpha0 = 1.0 / K
m0     = np.zeros(D)
kappa0 = 0.01
nu0    = D + 2.0
Psi0   = np.eye(D) * 0.5

from scipy.stats import invwishart, dirichlet

# ── Initialise parameters ─────────────────────────────────────────────────────
idx0  = np.random.choice(N, K, replace=False)
mu    = X[idx0].copy()
Sigma = np.array([np.eye(D) * 0.5] * K)
pi    = np.ones(K) / K

dists0 = np.array([np.sum((X - mu[k])**2, axis=1) for k in range(K)]).T
z      = dists0.argmin(axis=1)

def mvn_logpdf_batch(X, mu_k, Sigma_k):
    """Log-pdf of N(mu_k, Sigma_k) for all N points. Returns (N,)."""
    diff         = X - mu_k
    sign, logdet = np.linalg.slogdet(Sigma_k)
    inv_S        = np.linalg.inv(Sigma_k)
    maha         = np.einsum('nd,dd,nd->n', diff, inv_S, diff)
    return -0.5 * (D * np.log(2 * np.pi) + logdet + maha)

print(f"\nRunning Gibbs sampler: K={K}, {N_ITER} iterations, burn-in={BURN_IN} …")
t0 = time.time()

z_samples = []

for it in range(N_ITER):

    # ── Step 1: sample z for ALL users at once (vectorized) ──────────────────
    log_probs = np.column_stack([
        np.log(pi[k] + 1e-300) + mvn_logpdf_batch(X, mu[k], Sigma[k])
        for k in range(K)
    ])                                           # (N, K)
    log_probs -= log_probs.max(axis=1, keepdims=True)
    probs      = np.exp(log_probs)
    probs     /= probs.sum(axis=1, keepdims=True)

    cumprobs = probs.cumsum(axis=1)
    u        = np.random.rand(N, 1)
    z        = np.clip((u > cumprobs).sum(axis=1), 0, K - 1)

    # ── Step 2: sample pi, mu_k, Sigma_k ─────────────────────────────────────
    nk   = np.array([(z == k).sum() for k in range(K)], dtype=float)
    xbar = np.array([X[z == k].mean(axis=0) if nk[k] > 0 else m0
                     for k in range(K)])
    pi   = dirichlet.rvs(nk + alpha0)[0]

    for k in range(K):
        if nk[k] == 0:
            Sigma[k] = invwishart.rvs(df=nu0, scale=Psi0)
            mu[k]    = np.random.multivariate_normal(m0, Sigma[k] / kappa0)
            continue
        Xk        = X[z == k]
        kappa_n   = kappa0 + nk[k]
        nu_n      = nu0 + nk[k]
        diff_mean = xbar[k] - m0
        S_k       = (Xk - xbar[k]).T @ (Xk - xbar[k])
        Psi_n     = (Psi0 + S_k
                     + (kappa0 * nk[k]) / kappa_n * np.outer(diff_mean, diff_mean))
        Sigma[k]  = invwishart.rvs(df=nu_n, scale=Psi_n)
        mu_n      = (kappa0 * m0 + nk[k] * xbar[k]) / kappa_n
        mu[k]     = np.random.multivariate_normal(mu_n, Sigma[k] / kappa_n)

    if it >= BURN_IN:
        z_samples.append(z.copy())

    if (it + 1) % 10 == 0:
        elapsed = time.time() - t0
        counts  = np.bincount(z, minlength=K)
        print(f"  iter {it+1:4d}/{N_ITER}  sizes={counts.tolist()}  ({elapsed:.1f}s)")

print(f"Sampling done in {time.time()-t0:.1f}s")

# ── 5. POSTERIOR ANALYSIS ─────────────────────────────────────────────────────
z_samples_arr = np.array(z_samples)          # (n_samples, N)

# MAP assignment: mode across post-burn-in samples
z_map = np.apply_along_axis(
    lambda col: np.bincount(col, minlength=K).argmax(),
    axis=0, arr=z_samples_arr)

probs_per_user = np.zeros((N, K))
for zs in z_samples_arr:
    probs_per_user[np.arange(N), zs] += 1
probs_per_user /= len(z_samples_arr)
entropy = -(probs_per_user * np.log(probs_per_user + 1e-12)).sum(axis=1)

import pickle

to_save = {
    "R_norm":    R_norm,       # (N, M) sparse normalized rating matrix
    "X":         X,            # (N, 2) UMAP embeddings
    "z_map":     z_map,        # (N,)   MAP cluster per user
    "users":     users,        # list of user ids (index alignment)
    "movie_idx": movie_idx,    # {movie_id -> col index}
    "K":         K,
    "reducer":   reducer,      # fitted UMAP (for inference)
}

# cluster_params: use final sampled mu/Sigma (post burn-in mean would be better
# but final sample is a valid point estimate and avoids storing all samples)
cluster_params = [{"mu": mu[k], "Sigma": Sigma[k]} for k in range(K)]
to_save["cluster_params"] = cluster_params

with open(d + "/gmm_model.pkl", "wb") as f:
    pickle.dump(to_save, f)
