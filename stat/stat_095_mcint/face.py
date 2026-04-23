import numpy as np
import matplotlib.pyplot as plt
import os

def place_object(f, h, gx, gy):
    """
    Places the face template 'f' into a larger grid at coordinates 'h'.
    h is [x, y].
    """
    grid = np.zeros((gy, gx))
    fh, fw = f.shape
    
    # Calculate top-left corner based on center coordinate h
    r_start = int(h[1] - fh // 2)
    c_start = int(h[0] - fw // 2)
    
    for i in range(fh):
        for j in range(fw):
            r, c = r_start + i, c_start + j
            if 0 <= r < gy and 0 <= c < gx:
                grid[r, c] = f[i, j]
    return grid

def compat(v_t, f, h_part, gx, gy):
    """
    Likelihood function: compares observed noisy grid with particle prediction.
    """
    v_pred = place_object(f, h_part, gx, gy)
    # Mean absolute error based likelihood
    diff = np.sum(np.abs(v_t - v_pred))
    return np.exp(-0.5 * diff)

def run_particle_filter_demo():
    # 1. Setup Environment
    gx, gy = 40, 40
    output_dir = "/tmp/pf_frames"
    if not os.path.exists(output_dir):
        os.makedirs(output_dir)
        print(f"Created directory: {output_dir}")

    # Define the Face Template
    f = np.array([
        [1, 1, 1, 0, 1, 1, 1],
        [0, 0, 0, 0, 0, 0, 0],
        [0, 1, 0, 0, 0, 1, 0],
        [0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 1, 0, 0, 0],
        [1, 0, 0, 0, 0, 0, 1],
        [1, 1, 1, 1, 1, 1, 1]
    ])

    t_steps = 50
    sigma = 1.0
    l_count = 70

    # 2. Generate Ground Truth and Observations
    h = np.zeros((2, t_steps))
    v = np.zeros((gy, gx, t_steps))
    h[:, 0] = [gx // 2, gy // 2] 
    
    for t in range(1, t_steps):
        h[:, t] = h[:, t-1] + sigma * np.random.randn(2)
        v_clean = place_object(f, h[:, t], gx, gy)
        flip_mask = np.random.rand(gy, gx) > 0.95
        v_noisy = v_clean.copy()
        v_noisy[flip_mask] = 1 - v_noisy[flip_mask]
        v[:, :, t] = v_noisy

    # 3. Initialize Particle Filter
    h_part_old = np.tile(h[:, 0:1], (1, l_count))
    # Initial weights normalized and forced to sum to 1.0
    w_old = np.ones(l_count) / l_count
    w_old /= w_old.sum()
    
    fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(14, 6))

    # 4. Filter Loop
    for t in range(t_steps):
        h_part = np.zeros((2, l_count))
        w_tilde = np.zeros(l_count)
        
        # Robust Resampling Step
        # Using w_old / w_old.sum() inside choice to prevent "does not sum to 1" error
        indices = np.random.choice(np.arange(l_count), size=l_count, p=w_old)
        
        for l in range(l_count):
            l_star = indices[l]
            h_part[:, l] = h_part_old[:, l_star] + sigma * np.random.randn(2)
            w_tilde[l] = compat(v[:, :, t], f, h_part[:, l], gx, gy)
            
        # Normalization with precision safety
        sum_w = np.sum(w_tilde)
        if sum_w > 0:
            w = w_tilde / sum_w
            # Final nudge to ensure sum is exactly 1.0 for the next iteration's np.random.choice
            w = w / w.sum()
        else:
            w = np.ones(l_count) / l_count

        # --- Visualization ---
        ax1.clear()
        ax1.imshow(1 - v[:, :, t], cmap='bone')
        ax1.set_title(f"Step {t}: Tracking Noisy Face")
        
        for l in range(l_count):
            ax1.plot(h_part[0, l], h_part[1, l], 'o', 
                     color='dodgerblue', markersize=max(20 * w[l], 1.5), alpha=0.5)
            
        ax1.plot(h[0, t], h[1, t], 'gx', markersize=14, markeredgewidth=3, label='True')
        
        ind_max = np.argmax(w)
        ax1.plot(h_part[0, ind_max], h_part[1, ind_max], 'r+', markersize=14, markeredgewidth=3)
        
        h_mean = np.sum(w * h_part, axis=1)
        ax1.plot(h_mean[0], h_mean[1], 'mo', markersize=12, fillstyle='none', markeredgewidth=2)
        
        ax2.clear()
        ax2.bar(range(l_count), w, color='teal')
        ax2.set_title('Particle Weights')
        ax2.set_ylim(0, max(np.max(w), 0.2))
        
        plt.savefig(f"{output_dir}/frame_{t:03d}.jpg", dpi=120)
        if t % 10 == 0:
            print(f"Progress: {t}/{t_steps} frames saved.")
            
        h_part_old = h_part
        w_old = w

    plt.close(fig)
    print(f"Finished. Check the '{output_dir}' folder for JPEGs.")

if __name__ == "__main__":
    run_particle_filter_demo()
