import numpy as np
import pandas as pd
import corr_segment

NUM_BLOCKS = 6
NUM_TAUS = NUM_BLOCKS - 1
np.random.seed(42)

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

trace_taus, trace_betas, trace_sigmas = corr_segment.metropolis_sampler(
    X, Y, 
    num_blocks=NUM_BLOCKS,
    iterations=30000, 
    burn_in=10000,
    proposal_width_tau=0.7,
    proposal_width_beta=0.02,
    proposal_width_sigma=0.02
)

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

# Call modular log_likelihood from the routine package
max_log_lik = corr_segment.log_likelihood(est_taus, est_betas, est_sigmas, X, Y)

num_params = (NUM_BLOCKS - 1) + NUM_BLOCKS + NUM_BLOCKS  # taus + betas + sigmas
aic = 2 * num_params - 2 * max_log_lik
bic = num_params * np.log(N) - 2 * max_log_lik

print(f"Maximized Log-Likelihood: {max_log_lik:.2f} (Higher/Less Negative is better)")
print(f"Total Parameters (k):     {num_params}")
print(f"AIC Score:                {aic:.2f} (Lower is better)")
print(f"BIC Score:                {bic:.2f} <-- Best for identifying true block count")
