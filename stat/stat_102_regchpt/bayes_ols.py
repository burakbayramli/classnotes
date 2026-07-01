import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

def log_likelihood(alpha, beta, sigma, x, y):
    if sigma <= 0:
        return -np.inf

    mu = alpha + beta * x
    
    term1 = -0.5 * np.log(2 * np.pi)
    term2 = -np.log(sigma)
    term3 = -((y - mu) ** 2) / (2 * (sigma ** 2))
    return np.sum(term1 + term2 + term3)

def log_prior(alpha, beta, sigma):
    if sigma <= 0 or sigma > 50: 
        return -np.inf
    if not (-100 < alpha < 100): 
        return -np.inf
    if not (-10 < beta < 10):    
        return -np.inf
    return 0.0

def log_posterior(alpha, beta, sigma, x, y):
    return log_prior(alpha, beta, sigma) + log_likelihood(alpha, beta, sigma, x, y)

def metropolis_sampler(x, y, iterations=25000, proposal_width_alpha=1.0, proposal_width_beta=0.05, proposal_width_sigma=0.5):
    current_alpha = 0.0
    current_beta = 0.0
    current_sigma = 1.0
    
    alpha_trace = np.zeros(iterations)
    beta_trace = np.zeros(iterations)
    sigma_trace = np.zeros(iterations)
    
    accepted_count = 0
    
    for i in range(iterations):
        proposed_alpha = np.random.normal(current_alpha, proposal_width_alpha)
        proposed_beta = np.random.normal(current_beta, proposal_width_beta)
        proposed_sigma = np.random.normal(current_sigma, proposal_width_sigma)
        
        log_post_current = log_posterior(current_alpha, current_beta, current_sigma, x, y)
        log_post_proposed = log_posterior(proposed_alpha, proposed_beta, proposed_sigma, x, y)
        
        log_acceptance_ratio = log_post_proposed - log_post_current
        
        if np.log(np.random.uniform(0, 1)) < log_acceptance_ratio:
            current_alpha = proposed_alpha
            current_beta = proposed_beta
            current_sigma = proposed_sigma
            accepted_count += 1
            
        alpha_trace[i] = current_alpha
        beta_trace[i] = current_beta
        sigma_trace[i] = current_sigma
        
    print(f"Acceptance Rate: {accepted_count / iterations * 100:.2f}%")
    return alpha_trace, beta_trace, sigma_trace
