import numpy as np
import pandas as pd
import scipy.stats as stats
from scipy.special import expit
import matplotlib.pyplot as plt

# =====================================================================
# 1. LOAD RAW DATA
# =====================================================================
df = pd.read_csv('../../compscieng/compscieng_app20cfit/cave.csv')

X = df['Temp'].values
Y = df['C'].values
N = len(df)
time_axis = np.arange(N)

np.random.seed(42)

print(f"Loaded {N} data points from cave.csv.")

NUM_BLOCKS = 3  
NUM_TAUS = NUM_BLOCKS - 1

def sigmoid(t, tau, k=2.0):
    """Logistic function to smoothly transition between blocks (Numerically Stable)."""
    return expit(k * (t - tau))

def log_likelihood(taus, alphas, betas, sigmas, X, Y):
    # Calculate generalized weights via the telescoping difference method
    num_blocks = len(betas)
    sigmoids = np.array([sigmoid(time_axis, tau) for tau in taus]) # Shape: (NUM_TAUS, N)
    
    weights = np.zeros((num_blocks, N))
    weights[0] = 1.0 - sigmoids[0]
    for j in range(1, num_blocks - 1):
        weights[j] = sigmoids[j-1] - sigmoids[j]
    weights[-1] = sigmoids[-1]
    
    # Blended predictions and variances using intercepts and slopes
    predicted_mu = np.zeros(N)
    blended_sigma_sq = np.zeros(N)
    for b in range(num_blocks):
        predicted_mu += weights[b] * (alphas[b] + betas[b] * X)
        blended_sigma_sq += weights[b] * (sigmas[b] ** 2)
    blended_sigma = np.sqrt(blended_sigma_sq)
    
    return np.sum(stats.norm.logpdf(Y, loc=predicted_mu, scale=blended_sigma))

def log_prior(taus, alphas, betas, sigmas, Y):
    # 1. Order and boundary constraints on change-points
    if np.any(np.diff(taus) <= 15) or taus[0] < 15 or taus[-1] > N - 15:
        return -np.inf
        
    # 2. Parameter parameter constraints
    if np.any(sigmas <= 0) or np.any(sigmas > 50):
        return -np.inf
    if np.any(alphas < -500) or np.any(alphas > 500) or np.any(betas < -50) or np.any(betas > 50):
        return -np.inf
    
    # Weakly informative normal priors for parameters to stabilize the sampler
    return np.sum(stats.norm.logpdf(alphas, loc=np.mean(Y), scale=100.0)) + \
           np.sum(stats.norm.logpdf(betas, loc=0.0, scale=10.0)) + \
           np.sum(stats.halfnorm.logpdf(sigmas, scale=10.0))

def log_posterior(taus, alphas, betas, sigmas, X, Y):
    log_p = log_prior(taus, alphas, betas, sigmas, Y)
    if log_p == -np.inf:
        return -np.inf
    return log_p + log_likelihood(taus, alphas, betas, sigmas, X, Y)

def metropolis_sampler(X, Y, iterations=40000, burn_in=15000, 
                       proposal_width_tau=0.6, proposal_width_alpha=0.4, 
                       proposal_width_beta=0.04, proposal_width_sigma=0.1):
    
    # Dynamically initialize parameters based on the number of blocks
    curr_taus = np.linspace(30, N - 30, NUM_TAUS)
    curr_alphas = np.ones(NUM_BLOCKS) * np.mean(Y)  
    curr_betas = np.zeros(NUM_BLOCKS)
    curr_sigmas = np.ones(NUM_BLOCKS) * np.std(Y)   
    
    # Traces
    trace_taus = []
    trace_alphas = []
    trace_betas = []
    trace_sigmas = []
    
    accepted_count = 0
    
    print(f"Running intercept-aware Metropolis sampler for {NUM_BLOCKS} blocks...")
    for step in range(iterations):
        # Propose shifts using symmetric Gaussian random walk steps
        prop_taus = curr_taus + np.random.normal(0, proposal_width_tau, size=NUM_TAUS)
        prop_alphas = curr_alphas + np.random.normal(0, proposal_width_alpha, size=NUM_BLOCKS)
        prop_betas = curr_betas + np.random.normal(0, proposal_width_beta, size=NUM_BLOCKS)
        prop_sigmas = curr_sigmas + np.random.normal(0, proposal_width_sigma, size=NUM_BLOCKS)
        
        log_post_curr = log_posterior(curr_taus, curr_alphas, curr_betas, curr_sigmas, X, Y)
        log_post_prop = log_posterior(prop_taus, prop_alphas, prop_betas, prop_sigmas, X, Y)
        
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
            
    print(f"Post-Burn-in Acceptance Rate: {accepted_count / (iterations - burn_in) * 100:.2f}%")
    
    return np.array(trace_taus), np.array(trace_alphas), np.array(trace_betas), np.array(trace_sigmas)


# Execution call to run the restructured sampler
trace_taus, trace_alphas, trace_betas, trace_sigmas = metropolis_sampler(
    X, Y, 
    iterations=50000, 
    burn_in=20000, 
    proposal_width_tau=0.3,    # Decreased from 0.6
    proposal_width_alpha=0.15,  # Decreased from 0.4
    proposal_width_beta=0.01,   # Decreased from 0.04
    proposal_width_sigma=0.05   # Decreased from 0.1
)
# =====================================================================
# 4. RESULTS & GOODNESS-OF-FIT
# =====================================================================
print("\n--- INFERENCE RESULTS ---")
for i in range(NUM_TAUS):
    print(f"Estimated Tau {i+1} (Timeline Index): {np.mean(trace_taus[:, i]):.2f} ± {np.std(trace_taus[:, i]):.2f}")
print("")
for i in range(NUM_BLOCKS):
    print(f"Block {i+1} -> Alpha (Intercept): {np.mean(trace_alphas[:, i]):.3f} | Beta (Slope): {np.mean(trace_betas[:, i]):.3f} | Sigma (Noise): {np.mean(trace_sigmas[:, i]):.3f}")

print("\n--- GOODNESS-OF-FIT METRICS ---")
est_taus = np.mean(trace_taus, axis=0)
est_alphas = np.mean(trace_alphas, axis=0)
est_betas = np.mean(trace_betas, axis=0)
est_sigmas = np.mean(trace_sigmas, axis=0)

# Re-use our new modular log_likelihood function for the final calculations!
max_log_lik = log_likelihood(est_taus, est_alphas, est_betas, est_sigmas, X, Y)

num_params = (NUM_BLOCKS - 1) + NUM_BLOCKS + NUM_BLOCKS + NUM_BLOCKS
aic = 2 * num_params - 2 * max_log_lik
bic = num_params * np.log(N) - 2 * max_log_lik

print(f"Maximized Log-Likelihood: {max_log_lik:.2f}")
print(f"Total Parameters (k):     {num_params}  (Expanded to 4M - 1 due to intercepts)")
print(f"AIC Score:                {aic:.2f}")
print(f"BIC Score:                {bic:.2f} <-- Best for identifying true block count")
