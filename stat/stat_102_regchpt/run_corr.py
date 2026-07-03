import numpy as np
import pandas as pd
import corr_segment
import scipy.stats

def p_corr(df1, df2):
    corr = df1.corr(df2)
    N = np.sum(df1.notnull())
    t = corr*np.sqrt((N-2)/(1-corr**2))
    p = 1-scipy.stats.t.cdf(abs(t),N-2)  # one-tailed
    return corr, t, p

NUM_BLOCKS = 5
NUM_TAUS = NUM_BLOCKS - 1
np.random.seed(42)

df = pd.read_csv('gold_ir.csv', index_col=0)
df_clean = df.dropna().copy()
X = (df_clean.Gold_Price - np.mean(df_clean.Gold_Price)) / np.std(df_clean.Gold_Price)
Y = (df_clean.Calculated_Real_IR - np.mean(df_clean.Calculated_Real_IR)) / np.std(df_clean.Calculated_Real_IR)
N = len(df_clean)

trace_taus, trace_alphas, trace_betas, trace_sigmas = corr_segment.metropolis_sampler(
    X, Y, 
    num_blocks=NUM_BLOCKS,
    iterations=40000, 
#    iterations=3000, 
    burn_in=1000, 
    proposal_width_tau=20,    
    proposal_width_alpha=0.15,  
    proposal_width_beta=0.01,   
    proposal_width_sigma=0.05   
)

print("\n--- INFERENCE RESULTS ---")
for i in range(NUM_TAUS):
    print(f"Estimated Tau {i+1} (Timeline Index): {np.mean(trace_taus[:, i]):.2f} ± {np.std(trace_taus[:, i]):.2f}")
print("")
for i in range(NUM_BLOCKS):
    print(f"Block {i+1} -> Alpha (Intercept): {np.mean(trace_alphas[:, i]):.3f} | Beta (Slope): {np.mean(trace_betas[:, i]):.3f} | Sigma (Noise): {np.mean(trace_sigmas[:, i]):.3f}")

dates = df_clean.index.values
print("\n--- INFERENCE RESULTS ---")
block_edges = [0]
for i in range(NUM_TAUS):
    tau_mean = np.mean(trace_taus[:, i])
    tau_std = np.std(trace_taus[:, i])
    # Map back to the calendar date using integer casting
    mapped_index = int(round(tau_mean))
    block_edges.append(mapped_index)
    mapped_index = max(0, min(mapped_index, len(dates) - 1))
    breakpoint_date = dates[mapped_index]    
    print(f"Regime Shift {i+1}: Index {tau_mean:.1f} (±{tau_std:.2f}) -> Date: {breakpoint_date}")

block_edges.append(len(df_clean))    
    
print("--- FINAL BLOCK CORRELATIONS ---")
for i in range(len(block_edges) - 1):
    start_idx = block_edges[i]
    end_idx = block_edges[i+1]
    block_data = df_clean.iloc[start_idx:end_idx]    
    correlation,t,p = p_corr(block_data['Gold_Price'],block_data['Calculated_Real_IR'])    
    start_date = df_clean.index[start_idx]
    end_date = df_clean.index[min(end_idx, len(df_clean)-1)]    
    print(f"Block {i+1} ({start_date} to {end_date}): Correlation r = {correlation:.3f} pval = {p:.3f}")
    
print("\n--- GOODNESS-OF-FIT METRICS ---")
est_taus = np.mean(trace_taus, axis=0)
est_alphas = np.mean(trace_alphas, axis=0)
est_betas = np.mean(trace_betas, axis=0)
est_sigmas = np.mean(trace_sigmas, axis=0)

max_log_lik = corr_segment.log_likelihood(est_taus, est_alphas, est_betas, est_sigmas, X, Y)

num_params = (NUM_BLOCKS - 1) + NUM_BLOCKS + NUM_BLOCKS + NUM_BLOCKS
aic = 2 * num_params - 2 * max_log_lik
bic = num_params * np.log(N) - 2 * max_log_lik

print(f"Maximized Log-Likelihood: {max_log_lik:.2f}")
print(f"Total Parameters (k):     {num_params} ")
print(f"AIC Score:                {aic:.2f}")
print(f"BIC Score:                {bic:.2f} ")
