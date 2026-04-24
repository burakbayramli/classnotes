import tensorflow as tf
import numpy as np
import time

# --- CONFIGURATION ---
D = 100               # 100 Dimensions
N = 1_000_000         # 1 Million Particles (GPU fodder)
STEPS = 50            # Temperature steps
MCMC_STEPS = 5        # Jiggle steps per temperature
T_START = 50.0        # Higher start temp for the Product Peak "desert"
T_END = 0.01

# Product Peak Parameters (The "Needle")
# a_coeffs control sharpness (higher = sharper)
a_coeffs = tf.constant(np.random.uniform(5.0, 10.0, D), dtype=tf.float32)
# w_offsets is the "hidden" secret location of the peak
w_offsets = tf.constant(np.random.uniform(0.2, 0.8, D), dtype=tf.float32)

@tf.function
def genz_product_peak(particles, a_coeffs, w_offsets):
    # f(x) = product( (a_i^-2 + (x_i - w_i)^2 )^-1 )
    dist_sq = tf.square(particles - w_offsets)
    terms = (tf.math.pow(a_coeffs, -2.0) + dist_sq)
    # Using log-sum to avoid numerical underflow in 100D
    return tf.reduce_sum(tf.math.log(terms), axis=-1)

# Initialize particles [0, 1]
particles = tf.Variable(tf.random.uniform([N, D], 0, 1))
temperatures = np.geomspace(T_START, T_END, STEPS)

print(f"Starting SMC for {D}D Genz Product Peak with {N} particles...")

for i, T_curr in enumerate(temperatures):
    start_time = time.time()
    
    # 1. Evaluate Likelihood (Energy)
    # Corrected: Passing all required arguments
    energies = genz_product_peak(particles, a_coeffs, w_offsets)
    
    # 2. Resample (Memory Efficient Systematic Resampling)
    weights = tf.exp(-(energies - tf.reduce_min(energies)) / T_curr)
    weights = weights / tf.reduce_sum(weights) 
    
    cumsum = tf.cumsum(weights)
    u = (tf.random.uniform([N]) + tf.cast(tf.range(N), tf.float32)) / tf.cast(N, tf.float32)
    
    # Clipping prevents the "index 1000000 out of bounds" error
    indices = tf.searchsorted(cumsum, u)
    indices = tf.clip_by_value(indices, 0, N - 1) 
    
    particles.assign(tf.gather(particles, indices))
    total_accepted = 0.0
    # 3. MCMC "Jiggle" (Vectorized Random Walk)
    for _ in range(MCMC_STEPS):
        noise = tf.random.normal([N, D], 0, 0.01)
        proposal = particles + noise
        proposal = tf.clip_by_value(proposal, 0.0, 1.0)
        
        # Evaluation calls corrected here as well
        p_energy = genz_product_peak(proposal, a_coeffs, w_offsets)
        curr_energy = genz_product_peak(particles, a_coeffs, w_offsets)
        
        accept_prob = tf.exp(-(p_energy - curr_energy) / T_curr)
        accepted_bool = tf.random.uniform([N]) < accept_prob
        indices_to_replace = tf.random.uniform([N]) < accept_prob

        total_accepted += tf.reduce_mean(tf.cast(accepted_bool, tf.float32))
        
        particles.assign(tf.where(tf.expand_dims(indices_to_replace, -1), proposal, particles))

    elapsed = time.time() - start_time
    best_val = tf.reduce_min(energies).numpy()

    avg_acceptance = (total_accepted / MCMC_STEPS).numpy()
    print(f"Step {i+1:3d} | Temp: {T_curr:7.4f} | Best: {best_val:10.4f} | Accept: {avg_acceptance:6.2%} | Time: {elapsed:.2f}s")
    

# --- POST-PROCESSING ---
final_energies = genz_product_peak(particles, a_coeffs, w_offsets)
best_idx = tf.argmin(final_energies)
best_x_coords = particles[best_idx].numpy()
actual_x = w_offsets.numpy()

print("\n" + "="*30)

print(f"Final Best Energy: {tf.reduce_min(final_energies).numpy():.12f}")
print(f"Accuracy Check (Found vs Actual for first 5 dims):")
for i in range(5):
    print(f"Dim {i}: Found {best_x_coords[i]:.6f} | Actual {actual_x[i]:.6f}")
print("="*30)
