import numpy as np
import pandas as pd
import scipy.stats as st

np.random.seed(42)

df = pd.read_csv('../stat_102_regchpt/gold_ir.csv', index_col=0)
df_clean = df.dropna().copy()

N = len(df)

raw_gold = df.Gold_Price
raw_rate = df.Calculated_Real_IR
diff_gold = np.diff(raw_gold)
diff_rate = np.diff(raw_rate)
data = np.column_stack((diff_gold, diff_rate))

def log_prior(theta):
    mu_x, mu_y, sigma_x, sigma_y, rho = theta
    if sigma_x <= 0 or sigma_y <= 0 or not (-1 < rho < 1):
        return -np.inf
    return 0.0

def log_likelihood(theta, data, nu=3.0):
    mu_x, mu_y, sigma_x, sigma_y, rho = theta    
    cov = rho * sigma_x * sigma_y
    Sigma = np.array([[sigma_x**2, cov], [cov, sigma_y**2]])
    
    inv_Sigma = np.linalg.inv(Sigma)
    det_Sigma = np.linalg.det(Sigma)
        
    if det_Sigma <= 0: return -np.inf

    X = data - np.array([mu_x, mu_y])    
    quad_form = np.sum(X @ inv_Sigma * X, axis=1)
    p = 2.0
    log_const = np.log(st.gamma.pdf((nu + p)/2, 1)) - np.log(st.gamma.pdf(nu/2, 1)) - (p/2)*np.log(np.pi * nu) - 0.5 * np.log(det_Sigma)
    
    log_lik = len(data) * log_const - ((nu + p) / 2.0) * np.sum(np.log(1.0 + quad_form / nu))
    return log_lik

def log_posterior(theta, data):
    lp = log_prior(theta)
    if not np.isfinite(lp):
        return -np.inf
    return lp + log_likelihood(theta, data)

def metropolis_sampler(data, num_samples=20000, proposal_std=0.01):
    current_theta = np.array([np.mean(data[:,0]), np.mean(data[:,1]), np.std(data[:,0]), np.std(data[:,1]), 0.0])
    current_log_post = log_posterior(current_theta, data)
    
    trace = np.zeros((num_samples, 5))
    accepted = 0
    
    std_x = np.std(data[:, 0])
    std_y = np.std(data[:, 1])
    
    tuning_factor = 0.0004
    
    prop_scales = np.array([
        std_x * tuning_factor, 
        std_y * tuning_factor, 
        std_x * tuning_factor, 
        std_y * tuning_factor, 
        proposal_std * 0.1 
    ])
    
    for i in range(num_samples):
        proposal = current_theta + np.random.normal(0, 1, size=5) * prop_scales
        proposal_log_post = log_posterior(proposal, data)
        
        if proposal_log_post > current_log_post:
            accept = True
        else:
            accept_prob = np.exp(np.clip(proposal_log_post - current_log_post, -20, 0))
            accept = np.random.uniform(0, 1) < accept_prob
            
        if accept:
            current_theta = proposal
            current_log_post = proposal_log_post
            accepted += 1
            
        trace[i, :] = current_theta
        
    print(f"True Sampler Acceptance Rate: {accepted / num_samples:.2%}")
    return trace

trace = metropolis_sampler(data, num_samples=50000)
burn_in = 5000
rho_samples = trace[burn_in:, 4]

r_raw, p_raw = st.pearsonr(raw_gold, raw_rate)

r_diff, p_diff = st.pearsonr(diff_gold, diff_rate)

bayes_rho_mean = np.mean(rho_samples)
hdi_95 = np.percentile(rho_samples, [2.5, 97.5])

print(f"\nPREPROCESSED DATA (First Differences):")
print(f"  - Frequentist: Pearson r = {r_diff:.4f}, p-value = {p_diff:.4f}")
print(f"  - Custom Bayes Student-t: Posterior Mean ρ = {bayes_rho_mean:.4f}")
print(f"  - 95% Bayesian Credible Interval: [{hdi_95[0]:.4f}, {hdi_95[1]:.4f}]")


