import numpy as np
import matplotlib.pyplot as plt

from scipy.stats import multivariate_normal

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

def gmm_pdf(x, y, weights, means, covs):
    """(x,y) noktasinda GMM olasilik yogunlugunu hesapla"""
    z = 0
    for i in range(len(weights)):
        rv = multivariate_normal(means[i], covs[i])
        z += weights[i] * rv.pdf([x, y])
    return z

def responsibilities(x, weights, means, covs):
    p = np.zeros(len(weights))
    for k in range(len(weights)):
        rv = multivariate_normal(mean=means[k], cov=covs[k])
        p[k] = weights[k] * rv.pdf(x)
    denom = np.sum(p)
    if denom == 0:
        return np.ones_like(p) / len(p)
    return p / denom

def log_likelihood(X, weights, means, covs):
    ll = 0.0
    for x in X:
        px = sum(weights[k] *
                 multivariate_normal(mean=means[k], cov=covs[k]).pdf(x)
                 for k in range(len(weights)))
        ll += np.log(px + 1e-12)  # guard against log(0)
    return ll

weights = [0.7, 0.3]
means = [
    [0, 5],
    [5, -2]
]
covs = [
    [[2, 1.5], [1.5, 3]],
    [[10, -3], [-3, 5]]
]

n_points = 1000

# Generate data points
gmm_data = generate_gmm_data(weights, means, covs, n_points)

# Number of Gaussian components and data dimension
n_components = 2
d = gmm_data.shape[1]

# Initialize model parameters
weights_tmp = np.ones(n_components) / n_components
means_tmp = np.random.randn(n_components, d) * 5
covs_tmp = np.array([np.eye(d) for _ in range(n_components)])

# === Introduction of the lambda variable ===
LAMBDA = 0.01  # Forgetting factor (lambda)
# Note: For lambda = 1/(m+1), the recursive formula is recovered.

# Main recursive update loop
for idx in range(len(gmm_data)):
    x = gmm_data[idx]
    
    # Compute responsibilities
    r = responsibilities(x, weights_tmp, means_tmp, covs_tmp)
    
    # === Update formulas with forgetting factor (lambda) ===
    for k in range(n_components):
        # Update mixture weights (Equation 13)
        weights_tmp[k] = weights_tmp[k] + LAMBDA * (r[k] - weights_tmp[k])
        
        # Update mean (Equation 14)
        mu_old = means_tmp[k].copy()
        if weights_tmp[k] != 0:
            means_tmp[k] = mu_old + LAMBDA * (r[k] / weights_tmp[k]) * (x - mu_old)
        
        # Update covariance (Equation 15)
        diff = np.array([x - mu_old])
        cov_old = covs_tmp[k].copy()
        if weights_tmp[k] != 0:
            covs_tmp[k] = cov_old + LAMBDA * (r[k] / weights_tmp[k]) * (np.dot(diff.T, diff) - cov_old)
        
    # Normalize weights
    weights_tmp = weights_tmp / np.sum(weights_tmp)
    
    # Print likelihood every 100 iterations
    if (idx + 1) % 100 == 0:
        ll = log_likelihood(gmm_data[:idx+1], weights_tmp, means_tmp, covs_tmp)
        print(f"Iteration {idx + 1}: Log Likelihood = {ll}")

print("\nFinal Parameters:")
print("Weights:", weights_tmp)
print("Means:", means_tmp)
print("Covs:", covs_tmp)
