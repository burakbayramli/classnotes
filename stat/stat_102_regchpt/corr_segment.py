import numpy as np
import pandas as pd
import scipy.stats as stats
from scipy.special import expit
import matplotlib.pyplot as plt

# =====================================================================
# 1. LOAD AND PREPROCESS DATA
# =====================================================================
df = pd.read_csv('gold_ir.csv', index_col=0)
df['Gold_Return'] = np.log(df['Gold_Price'] / df['Gold_Price'].shift(1))

# Calculate daily absolute changes for the Interest Rate (already a %)
df['IR_Change'] = df['Calculated_Real_IR'].diff()

# Drop the first row which contains NaN from the diffs
df_clean = df.dropna().copy()

# Normalize these stationary changes for your model
X = (df_clean.Gold_Return - np.mean(df_clean.Gold_Return)) / np.std(df_clean.Gold_Return)
Y = (df_clean.IR_Change - np.mean(df_clean.IR_Change)) / np.std(df_clean.IR_Change)

N = len(df_clean)
time_axis = np.arange(N)

np.random.seed(42)

# Define number of blocks dynamically
NUM_BLOCKS = 6  
NUM_TAUS = NUM_BLOCKS - 1

# =====================================================================
# 2. GENERALIZED CONTINUOUS MODEL DEFINITION
# =====================================================================
def sigmoid(t, tau, k=2.0):
    """Logistic function to smoothly transition between blocks (Numerically Stable)."""
    return expit(k * (t - tau))

def log_likelihood(taus, betas, sigmas, X, Y):
    # Calculate generalized weights via the telescoping difference method
    num_blocks = len(betas)
    sigmoids = np.array([sigmoid(time_axis, tau) for tau in taus]) # Shape: (NUM_TAUS, N)
    
    weights = np.zeros((num_blocks, N))
    weights[0] = 1.0 - sigmoids[0]
    for j in range(1, num_blocks - 1):
        weights[j] = sigmoids[j-1] - sigmoids[j]
    weights[-1] = sigmoids[-1]
    
    # Blended predictions and variances using slopes (no intercept)
    predicted_mu = np.zeros(N)
    blended_sigma_sq = np.zeros(N)
    for b in range(num_blocks):
        predicted_mu += weights[b] * betas[b] * X
        blended_sigma_sq += weights[b] * (sigmas[b] ** 2)
    blended_sigma = np.sqrt(blended_sigma_sq)
    
    return np.sum(stats.norm.logpdf(Y, loc=predicted_mu, scale=blended_sigma))

def log_prior(taus, betas, sigmas):
    # 1. Order and boundary constraints on change-points
    if np.any(np.diff(taus) <= 15) or taus[0] < 15 or taus[-1] > N - 15:
        return -np.inf
        
    # 2. Parameter constraints
    if np.any(betas < -1) or np.any(betas > 1) or np.any(sigmas <= 0):
        return -np.inf

    # Half-normal prior for noise scales
    return np.sum(stats.halfnorm.logpdf(sigmas, scale=1.0))

def log_posterior(taus, betas, sigmas, X, Y):
    log_p = log_prior(taus, betas, sigmas)
    if log_p == -np.inf:
        return -np.inf
    return log_p + log_likelihood(taus, betas, sigmas, X, Y)

# =====================================================================
# 3. METROPOLIS SAMPLER FOR VARIABLE DIMENSIONS
# =====================================================================
def metropolis_sampler(X, Y, iterations=30000, burn_in=10000,
                       proposal_width_tau=0.7, proposal_width_beta=0.02, 
                       proposal_width_sigma=0.02):
    
    # Dynamically initialize parameters based on the number of blocks
    curr_taus = np.linspace(30, N - 30, NUM_TAUS)
    curr_betas = np.zeros(NUM_BLOCKS)
    curr_sigmas = np.ones(NUM_BLOCKS)
    
    # Traces
    trace_taus = []
    trace_betas = []
    trace_sigmas = [] # Reconstructing full sigmas trace for strict completeness
    
    accepted_count = 0
    
    print(f"Running continuous Metropolis sampler for {NUM_BLOCKS} blocks...")
    for step in range(iterations):
        # Propose shifts using symmetric Gaussian random walk steps
        prop_taus = curr_taus + np.random.normal(0, proposal_width_tau, size=NUM_TAUS)
        prop_betas = curr_betas + np.random.normal(0, proposal_width_beta, size=NUM_BLOCKS)
        prop_sigmas = curr_sigmas + np.random.normal(0, proposal_width_sigma, size=NUM_BLOCKS)
        
        log_post_curr = log_posterior(curr_taus, curr_betas, curr_sigmas, X, Y)
        log_post_prop = log_posterior(prop_taus, prop_betas, prop_sigmas, X, Y)
        
        if np.log(np.random.rand()) < (log_post_prop - log_post_curr):
            curr_taus = prop_taus
            curr_betas = prop_betas
            curr_sigmas = prop_sigmas
            if step >= burn_in:
                accepted_count += 1
                
        if step >= burn_in:
            trace_taus.append(curr_taus.copy())
            trace_betas.append(curr_betas.copy())
            trace_sigmas.append(curr_sigmas.copy())
            
    print(f"Post-Burn-in Acceptance Rate: {accepted_count / (iterations - burn_in) * 100:.2f}%")
    
    return np.array(trace_taus), np.array(trace_betas), np.array(trace_sigmas)


# Execution call to run the restructured sampler
trace_taus, trace_betas, trace_sigmas = metropolis_sampler(
    X, Y, 
    iterations=30000, 
    burn_in=10000,
    proposal_width_tau=0.7,
    proposal_width_beta=0.02,
    proposal_width_sigma=0.02
)

# =====================================================================
# 4. RESULTS & GOODNESS-OF-FIT
# =====================================================================
dates = df_clean.index.values
print("\n--- INFERENCE RESULTS ---")
for i in range(NUM_TAUS):
    tau_mean = np.mean(trace_taus[:, i])
    tau_std = np.std(trace_taus[:, i])
    
    # Map back to the calendar date using integer casting
    mapped_index = int(round(tau_mean))
    mapped_index = max(0, min(mapped_index, len(dates) - 1))
    breakpoint_date = dates[mapped_index]
    
    print(f"Regime Shift {i+1}: Index {tau_mean:.1f} (±{tau_std:.2f}) -> Date: {breakpoint_date}")

print("")
for i in range(NUM_BLOCKS):
    print(f"Block {i+1} -> Beta (Slope/Correlation): {np.mean(trace_betas[:, i]):.3f} | Sigma (Noise): {np.mean(trace_sigmas[:, i]):.3f}")

print("\n--- GOODNESS-OF-FIT METRICS ---")
est_taus = np.mean(trace_taus, axis=0)
est_betas = np.mean(trace_betas, axis=0)
est_sigmas = np.mean(trace_sigmas, axis=0)

# Re-use our restructured modular log_likelihood function
max_log_lik = log_likelihood(est_taus, est_betas, est_sigmas, X, Y)

num_params = (NUM_BLOCKS - 1) + NUM_BLOCKS + NUM_BLOCKS  # taus + betas + sigmas
aic = 2 * num_params - 2 * max_log_lik
bic = num_params * np.log(N) - 2 * max_log_lik

print(f"Maximized Log-Likelihood: {max_log_lik:.2f} (Higher/Less Negative is better)")
print(f"Total Parameters (k):     {num_params}")
print(f"AIC Score:                {aic:.2f} (Lower is better)")
print(f"BIC Score:                {bic:.2f} <-- Best for identifying true block count")
