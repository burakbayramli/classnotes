import pymc as pm
import numpy as np
import matplotlib.pyplot as plt

# Your data setup
N = 250
mu_A_true, std_A_true = 30, 4
mu_B_true, std_B_true = 26, 7
durations_A = np.random.normal(mu_A_true, std_A_true, size=N)
durations_B = np.random.normal(mu_B_true, std_B_true, size=N)

# Pool the data for priors
pooled_mean = np.r_[durations_A, durations_B].mean()
pooled_std = np.r_[durations_A, durations_B].std()

# Build the model in modern PyMC
with pm.Model() as model:
    # Priors
    mu_A = pm.Normal("mu_A", mu=pooled_mean, sigma=1000*pooled_std)
    mu_B = pm.Normal("mu_B", mu=pooled_mean, sigma=1000*pooled_std)
    
    std_A = pm.Uniform("std_A", lower=pooled_std/1000., upper=1000.*pooled_std)
    std_B = pm.Uniform("std_B", lower=pooled_std/1000., upper=1000.*pooled_std)
    
    nu_minus_1 = pm.Exponential("nu_minus_1", lam=1./29)
    nu = pm.Deterministic("nu", nu_minus_1 + 1)
    
    # Likelihood - using StudentT instead of NoncentralT
    obs_A = pm.StudentT("obs_A", nu=nu, mu=mu_A, sigma=std_A, observed=durations_A)
    obs_B = pm.StudentT("obs_B", nu=nu, mu=mu_B, sigma=std_B, observed=durations_B)
    
    # Derived quantities (deterministic nodes)
    diff_of_means = pm.Deterministic("diff_of_means", mu_A - mu_B)
    diff_of_stds = pm.Deterministic("diff_of_stds", std_A - std_B)
    effect_size = pm.Deterministic("effect_size", 
                                    (mu_A - mu_B) / pm.math.sqrt((std_A**2 + std_B**2) / 2))
    
    # Sample
    trace = pm.sample(2000, tune=1000, return_inferencedata=False, cores=1)

# Extract traces
diff_means_trace = trace['diff_of_means']
diff_stds_trace = trace['diff_of_stds']
effect_size_trace = trace['effect_size']

# Plot the difference of means (Kruschke style)
plt.figure(figsize=(10, 4))
plt.hist(diff_means_trace, bins=50, histtype='stepfilled', alpha=0.85, density=True)
plt.axvline(0, color='red', linestyle='--', label='Null hypothesis')
plt.xlabel('μ_A - μ_B')
plt.ylabel('Density')
plt.title('Posterior Distribution of Difference of Means')

# Calculate percentage > 0
pct_greater = (diff_means_trace > 0).sum() / len(diff_means_trace) * 100
pct_less = (diff_means_trace < 0).sum() / len(diff_means_trace) * 100
plt.text(0.7, 0.9, f'{pct_less:.1f}% < 0 < {pct_greater:.1f}%', 
         transform=plt.gca().transAxes, fontsize=12)

plt.legend()
plt.tight_layout()
plt.savefig('/tmp/out1.jpg')


# Calculate and print 95% HDI
def hdi(trace, credible_mass=0.95):
    """Calculate highest density interval"""
    sorted_trace = np.sort(trace)
    n = len(sorted_trace)
    interval_size = int(np.floor(credible_mass * n))
    n_intervals = n - interval_size
    interval_width = sorted_trace[interval_size:] - sorted_trace[:n_intervals]
    min_idx = np.argmin(interval_width)
    return sorted_trace[min_idx], sorted_trace[min_idx + interval_size]

lower, upper = hdi(diff_means_trace)
mean_diff = diff_means_trace.mean()
print(f"Mean difference: {mean_diff:.2f}")
print(f"95% HDI: [{lower:.2f}, {upper:.2f}]")
print(f"{pct_less:.1f}% < 0 < {pct_greater:.1f}%")
