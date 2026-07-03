import numpy as np
import scipy.stats as stats
from scipy.special import expit

def sigmoid(t, tau, k=0.1):
    """Logistic function to smoothly transition between blocks (Numerically Stable)."""
    return expit(k * (t - tau))

def log_likelihood(taus, alphas, betas, sigmas, X, Y):
    """Calculates log-likelihood without relying on global time_axis."""
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
    
    # Blended predictions and variances using intercepts and slopes
    predicted_mu = np.zeros(N)
    blended_sigma_sq = np.zeros(N)
    for b in range(num_blocks):
        predicted_mu += weights[b] * (alphas[b] + betas[b] * X)
        blended_sigma_sq += weights[b] * (sigmas[b] ** 2)
    blended_sigma = np.sqrt(blended_sigma_sq)
    
    return np.sum(stats.norm.logpdf(Y, loc=predicted_mu, scale=blended_sigma))

def log_prior(taus, alphas, betas, sigmas, Y):
    """Calculates log-prior without relying on global N."""
    N = len(Y)
    
    # 1. Order and boundary constraints on change-points
    if np.any(np.diff(taus) <= 15) or taus[0] < 15 or taus[-1] > N - 15:
        return -np.inf
        
    # 2. Parameter constraints
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

def metropolis_sampler(X, Y, num_blocks=3, iterations=40000, burn_in=15000, 
                       proposal_width_tau=0.6, proposal_width_alpha=0.4, 
                       proposal_width_beta=0.04, proposal_width_sigma=0.1):
    """
    Runs the intercept-aware Metropolis sampler using a component-wise update strategy
    to maintain a healthy acceptance rate.
    """
    N = len(Y)
    num_taus = num_blocks - 1
    
    # Dynamically initialize parameters based on the number of blocks
    curr_taus = np.linspace(30, N - 30, num_taus)
    curr_alphas = np.ones(num_blocks) * np.mean(Y) 
    curr_betas = np.zeros(num_blocks)
    curr_sigmas = np.ones(num_blocks) * np.std(Y)  
    
    # Traces
    trace_taus = []
    trace_alphas = []
    trace_betas = []
    trace_sigmas = []
    
    accepted_count = 0
    
    print(f"Running component-wise Metropolis sampler for {num_blocks} blocks...")
    for step in range(iterations):
        # Create baseline copies for the proposal
        prop_taus = curr_taus.copy()
        prop_alphas = curr_alphas.copy()
        prop_betas = curr_betas.copy()
        prop_sigmas = curr_sigmas.copy()
        
        # Randomly select exactly one block of parameters to mutate
        update_choice = np.random.choice(['tau', 'alpha', 'beta', 'sigma'])
        
        if update_choice == 'tau':
            prop_taus += np.random.normal(0, proposal_width_tau, size=num_taus)
        elif update_choice == 'alpha':
            prop_alphas += np.random.normal(0, proposal_width_alpha, size=num_blocks)
        elif update_choice == 'beta':
            prop_betas += np.random.normal(0, proposal_width_beta, size=num_blocks)
        elif update_choice == 'sigma':
            prop_sigmas += np.random.normal(0, proposal_width_sigma, size=num_blocks)
        
        # Evaluate posterior densities
        log_post_curr = log_posterior(curr_taus, curr_alphas, curr_betas, curr_sigmas, X, Y)
        log_post_prop = log_posterior(prop_taus, prop_alphas, prop_betas, prop_sigmas, X, Y)
        
        # Metropolis acceptance step
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
