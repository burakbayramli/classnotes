import numpy as np
import matplotlib.pyplot as plt
from scipy.stats import multivariate_normal
import importlib.util

# === Load your generator ===
spec = importlib.util.spec_from_file_location("tst4", "tst4.py")

# === True GMM params (for generating synthetic data) ===
weights_true = [0.7, 0.3]
means_true = [[0, 5], [5, -2]]
covs_true = [[[2, 1.5], [1.5, 3]], [[10, -3], [-3, 5]]]
n_points = 1000

def generate_gmm_data(weights, means, covs, n_samples):
    n_components = len(weights)
    data = np.zeros((n_samples, 2))    
    component_choices = np.random.choice(n_components, size=n_samples, p=weights)
    for i in range(n_components):
        indices = np.where(component_choices == i)[0]
        n_points_to_sample = len(indices)
        if n_points_to_sample > 0:
            data[indices, :] = np.random.multivariate_normal(
                mean=means[i], 
                cov=covs[i], 
                size=n_points_to_sample
            )

    shuffle_indices = np.arange(len(data))
    np.random.shuffle(shuffle_indices)
    res_data = data[shuffle_indices]
    return res_data

data = generate_gmm_data(weights_true, means_true, covs_true, n_points)

# === Initial batch ===
initial_batch = 200
X0 = data[:initial_batch]

n_components = 2
d = data.shape[1]
lambda_forget = 0.995   # forgetting factor (λ=1 → no forgetting)

# === Helper: log-likelihood of dataset under a GMM ===
def log_likelihood(X, weights, means, covs):
    ll = 0.0
    for x in X:
        px = sum(weights[k] *
                 multivariate_normal(mean=means[k], cov=covs[k]).pdf(x)
                 for k in range(len(weights)))
        ll += np.log(px + 1e-12)
    return ll

weights = np.ones(n_components) / n_components
means = np.random.randn(n_components, d) * 5
covs = np.array([np.eye(d) for _ in range(n_components)])

# === Effective counts and moments ===
N_g = weights * initial_batch  # effective counts
M1 = np.zeros_like(means)      # weighted sum of x
M2 = np.zeros((n_components, d, d))  # weighted sum of xx^T
for k in range(n_components):
    M1[k] = N_g[k] * means[k]
    M2[k] = N_g[k] * (covs[k] + np.outer(means[k], means[k]))

# === Responsibilities function ===
def responsibilities(x, weights, means, covs):
    p = np.zeros(len(weights))
    for k in range(len(weights)):
        p[k] = weights[k] * multivariate_normal(mean=means[k], cov=covs[k]).pdf(x)
    s = p.sum()
    if s == 0:
        return np.ones_like(p) / len(p)
    return p / s

# === Incremental loop with forgetting ===
for idx in range(initial_batch, n_points):
    x = data[idx]
    r = responsibilities(x, weights, means, covs)
    for k in range(n_components):
        r_k = r[k]
        N_new = lambda_forget * N_g[k] + r_k
        M1_new = lambda_forget * M1[k] + r_k * x
        M2_new = lambda_forget * M2[k] + r_k * np.outer(x, x)

        N_g[k], M1[k], M2[k] = N_new, M1_new, M2_new
        means[k] = M1_new / (N_new + 1e-12)
        covs[k] = (M2_new / (N_new + 1e-12)) - np.outer(means[k], means[k])

    total = N_g.sum()
    weights = N_g / total if total > 0 else np.ones_like(weights) / n_components

# === Evaluate PDFs on grid ===
xs = np.linspace(-10, 15, 120)
ys = np.linspace(-10, 15, 120)
Xg, Yg = np.meshgrid(xs, ys)
Z_true = np.zeros_like(Xg)
Z_learned = np.zeros_like(Xg)

for i in range(Xg.shape[0]):
    for j in range(Xg.shape[1]):
        pt = np.array([Xg[i, j], Yg[i, j]])
        Z_true[i, j] = sum(weights_true[k] *
                           multivariate_normal(mean=means_true[k], cov=covs_true[k]).pdf(pt)
                           for k in range(len(weights_true)))
        Z_learned[i, j] = sum(weights[k] *
                              multivariate_normal(mean=means[k], cov=covs[k]).pdf(pt)
                              for k in range(n_components))

print (weights)        
print (means)
print (covs)
        
