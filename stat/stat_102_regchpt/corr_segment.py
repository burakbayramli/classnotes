import numpy as np
import scipy.stats as stats
from scipy.special import expit

def sigmoid(t, tau, k=2.0):
    """Logistic function to smoothly transition between blocks (Numerically Stable)."""
    return expit(k * (t - tau))

def log_likelihood(taus, betas, sigmas, X, Y):
    """Calculates log-likelihood without relying on global variables."""
    num_blocks = len(betas)
    N = len(Y)
    time_axis = np.arange(N)
    
    # Calculate generalized weights via the telescoping difference method
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

def log_prior(taus, betas, sigmas, Y):
    """Calculates log-prior using data dimensions explicitly passed."""
    N = len(Y)
    
    # 1. Order and boundary constraints on change-points
    if np.any(np.diff(taus) <= 15) or taus[0] < 15 or taus[-1] > N - 15:
        return -np.inf
        
    # 2. Parameter constraints
    if np.any(betas < -1) or np.any(betas > 1) or np.any(sigmas <= 0):
        return -np.inf

    # Half-normal prior for noise scales
    return np.sum(stats.halfnorm.logpdf(sigmas, scale=1.0))

def log_posterior(taus, betas, sigmas, X, Y):
    log_p = log_prior(taus, betas, sigmas, Y)
    if log_p == -np.inf:
        return -np.inf
    return log_p + log_likelihood(taus, betas, sigmas, X, Y)

def metropolis_sampler(X, Y, num_blocks=6, iterations=30000, burn_in=10000,
                       proposal_width_tau=0.7, proposal_width_beta=0.02, 
                       proposal_width_sigma=0.02):
    """
    Runs continuous Metropolis sampler. 
    num_blocks is passed explicitly to isolate logic from environment scope.
    """
    N = len(Y)
    num_taus = num_blocks - 1
    
    # Dynamically initialize parameters based on the number of blocks
    curr_taus = np.linspace(30, N - 30, num_taus)
    curr_betas = np.zeros(num_blocks)
    curr_sigmas = np.ones(num_blocks)
    
    # Traces
    trace_taus = []
    trace_betas = []
    trace_sigmas = [] 
    
    accepted_count = 0
    
    print(f"Running continuous Metropolis sampler for {num_blocks} blocks...")
    for step in range(iterations):
        # Propose shifts using symmetric Gaussian random walk steps
        prop_taus = curr_taus + np.random.normal(0, proposal_width_tau, size=num_taus)
        prop_betas = curr_betas + np.random.normal(0, proposal_width_beta, size=num_blocks)
        prop_sigmas = curr_sigmas + np.random.normal(0, proposal_width_sigma, size=num_blocks)
        
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
