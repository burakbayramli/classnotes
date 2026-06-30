import numpy as np
import pandas as pd
import scipy.stats as stats
from scipy.special import expit
import matplotlib.pyplot as plt

# =====================================================================
# 1. LOAD RAW DATA
# =====================================================================
# Load your real dataset
df = pd.read_csv('../../compscieng/compscieng_app20cfit/cave.csv')

# Use raw, unstandardized values
X = df['Temp'].values
Y = df['C'].values
N = len(df)
time_axis = np.arange(N)

print(f"Loaded {N} data points from cave.csv.")

# Define number of blocks dynamically
NUM_BLOCKS = 3  # Change this to 2, 4, 5, etc. to test different architectures
NUM_TAUS = NUM_BLOCKS - 1

# =====================================================================
# 2. GENERALIZED CONTINUOUS MODEL DEFINITION (WITH INTERCEPTS)
# =====================================================================
def sigmoid(t, tau, k=2.0):
    """Logistic function to smoothly transition between blocks (Numerically Stable)."""
    return expit(k * (t - tau))

def log_joint_posterior(taus, alphas, betas, sigmas):
    # 1. Order and boundary constraints on change-points
    # Ensure taus are sorted and maintain a buffer from edges/each other
    if np.any(np.diff(taus) <= 15) or taus[0] < 15 or taus[-1] > N - 15:
        return -np.inf
        
    # 2. Parameter constraints (Removed tight -1 to 1 bounds since data is unstandardized)
    if np.any(sigmas <= 0) or np.any(sigmas > 50):
        return -np.inf
    if np.any(alphas < -500) or np.any(alphas > 500) or np.any(betas < -50) or np.any(betas > 50):
        return -np.inf

    # 3. Calculate generalized weights via the telescoping difference method
    num_blocks = len(betas)
    sigmoids = np.array([sigmoid(time_axis, tau) for tau in taus]) # Shape: (NUM_TAUS, N)
    
    weights = np.zeros((num_blocks, N))
    weights[0] = 1.0 - sigmoids[0]
    for j in range(1, num_blocks - 1):
        weights[j] = sigmoids[j-1] - sigmoids[j]
    weights[-1] = sigmoids[-1]
    
    # 4. Blended predictions and variances using intercepts and slopes
    predicted_mu = np.zeros(N)
    blended_sigma_sq = np.zeros(N)
    for b in range(num_blocks):
        # Explicit intercept-aware prediction formulation: alpha + beta * X
        predicted_mu += weights[b] * (alphas[b] + betas[b] * X)
        blended_sigma_sq += weights[b] * (sigmas[b] ** 2)
    blended_sigma = np.sqrt(blended_sigma_sq)
    
    # 5. Evaluate Log-Likelihood and Log-Prior
    log_lik = np.sum(stats.norm.logpdf(Y, loc=predicted_mu, scale=blended_sigma))
    
    # Weakly informative normal priors for parameters to stabilize the sampler
    log_prior = np.sum(stats.norm.logpdf(alphas, loc=np.mean(Y), scale=100.0)) + \
                np.sum(stats.norm.logpdf(betas, loc=0.0, scale=10.0)) + \
                np.sum(stats.halfnorm.logpdf(sigmas, scale=10.0))
    
    return log_lik + log_prior

# =====================================================================
# 3. METROPOLIS SAMPLER FOR VARIABLE DIMENSIONS
# =====================================================================
num_samples = 40000
burn_in = 15000

# Dynamically initialize parameters based on the number of blocks
curr_taus = np.linspace(30, N - 30, NUM_TAUS)
curr_alphas = np.ones(NUM_BLOCKS) * np.mean(Y)  # Initialize near the baseline of data
curr_betas = np.zeros(NUM_BLOCKS)
curr_sigmas = np.ones(NUM_BLOCKS) * np.std(Y)   # Initialize near global residual noise

# Traces
trace_taus = []
trace_alphas = []
trace_betas = []
trace_sigmas = []

accepted_count = 0

print(f"Running intercept-aware Metropolis sampler for {NUM_BLOCKS} blocks...")
for step in range(num_samples):
    # Propose shifts using symmetric Gaussian random walk steps
    prop_taus = curr_taus + np.random.normal(0, 0.6, size=NUM_TAUS)
    prop_alphas = curr_alphas + np.random.normal(0, 0.4, size=NUM_BLOCKS)
    prop_betas = curr_betas + np.random.normal(0, 0.04, size=NUM_BLOCKS)
    prop_sigmas = curr_sigmas + np.random.normal(0, 0.1, size=NUM_BLOCKS)
    
    log_post_curr = log_joint_posterior(curr_taus, curr_alphas, curr_betas, curr_sigmas)
    log_post_prop = log_joint_posterior(prop_taus, prop_alphas, prop_betas, prop_sigmas)
    
    if np.log(np.random.rand()) < (log_post_prop - log_post_curr):
        curr_taus = prop_taus
        curr_alphas = prop_alphas
        curr_betas = prop_betas
        curr_sigmas = prop_sigmas
        if step >= burn_in:
            accepted_count += 1
        
    if step >= burn_in:
        trace_taus.append(curr_taus.copy())
        trace_alphas.append(curr_alphas.copy())
        trace_betas.append(curr_betas.copy())
        trace_sigmas.append(curr_sigmas.copy())

trace_taus = np.array(trace_taus)
trace_alphas = np.array(trace_alphas)
trace_betas = np.array(trace_betas)
trace_sigmas = np.array(trace_sigmas)

# =====================================================================
# 4. RESULTS & GOODNESS-OF-FIT
# =====================================================================
print(f"Post-Burn-in Acceptance Rate: {accepted_count / (num_samples - burn_in) * 100:.2f}%")

print("\n--- INFERENCE RESULTS ---")
for i in range(NUM_TAUS):
    print(f"Estimated Tau {i+1} (Timeline Index): {np.mean(trace_taus[:, i]):.2f} ± {np.std(trace_taus[:, i]):.2f}")
print("")
for i in range(NUM_BLOCKS):
    print(f"Block {i+1} -> Alpha (Intercept): {np.mean(trace_alphas[:, i]):.3f} | Beta (Slope): {np.mean(trace_betas[:, i]):.3f} | Sigma (Noise): {np.mean(trace_sigmas[:, i]):.3f}")

print("\n--- GOODNESS-OF-FIT METRICS ---")
# Extract best parameter estimates via posterior means
est_taus = np.mean(trace_taus, axis=0)
est_alphas = np.mean(trace_alphas, axis=0)
est_betas = np.mean(trace_betas, axis=0)
est_sigmas = np.mean(trace_sigmas, axis=0)

# Compute maximized log-likelihood under the intercept-aware paradigm
def get_max_log_lik(taus, alphas, betas, sigmas):
    sigmoids = np.array([sigmoid(time_axis, tau) for tau in taus])
    weights = np.zeros((NUM_BLOCKS, N))
    weights[0] = 1.0 - sigmoids[0]
    for j in range(1, NUM_BLOCKS - 1):
        weights[j] = sigmoids[j-1] - sigmoids[j]
    weights[-1] = sigmoids[-1]
    
    predicted_mu = np.zeros(N)
    blended_sigma_sq = np.zeros(N)
    for b in range(NUM_BLOCKS):
        predicted_mu += weights[b] * (alphas[b] + betas[b] * X)
        blended_sigma_sq += weights[b] * (sigmas[b] ** 2)
    
    return np.sum(stats.norm.logpdf(Y, loc=predicted_mu, scale=np.sqrt(blended_sigma_sq)))

max_log_lik = get_max_log_lik(est_taus, est_alphas, est_betas, est_sigmas)

# Calculate Information Criteria accounting for the extra alpha parameters
# k = (M - 1) [taus] + M [alphas] + M [betas] + M [sigmas] = 4M - 1
num_params = (NUM_BLOCKS - 1) + NUM_BLOCKS + NUM_BLOCKS + NUM_BLOCKS
aic = 2 * num_params - 2 * max_log_lik
bic = num_params * np.log(N) - 2 * max_log_lik

print(f"Maximized Log-Likelihood: {max_log_lik:.2f}")
print(f"Total Parameters (k):     {num_params}  (Expanded to 4M - 1 due to intercepts)")
print(f"AIC Score:                {aic:.2f}")
print(f"BIC Score:                {bic:.2f} <-- Best for identifying true block count")
