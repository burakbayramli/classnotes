import numpy as np
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches

rng = np.random.default_rng(42)

N          = 300          # number of particles
T          = 80           # time steps
SIGMA_MOV  = 0.4          # std of transition noise (motion model)
SIGMA_OBS  = 1.2          # std of observation noise added to true distances
LAM        = 1.0 / (2 * SIGMA_OBS**2)   # precision in likelihood

# Known beacon positions (fixed, 2-D)
beacons = np.array([[0., 0.],
                    [10., 0.],
                    [10., 10.],
                    [0., 10.],
                    [5., 5.]])

def true_trajectory(T):
    t  = np.linspace(0, 2*np.pi, T)
    xs = 5 + 4 * np.cos(t)          # ellipse, centred in beacon grid
    ys = 5 + 3 * np.sin(t)
    return np.stack([xs, ys], axis=1)   # (T, 2)

true_path = true_trajectory(T)

def observe(pos):
    """Return noisy distances from pos to each beacon."""
    d = np.linalg.norm(beacons - pos, axis=1)          # true distances
    return d + rng.normal(0, SIGMA_OBS, size=len(beacons))

observations = np.array([observe(true_path[t]) for t in range(T)])  # (T, K)

def log_likelihood(particles, y_t):
    """
    particles : (N, 2)
    y_t       : (K,)  observed distances
    returns   : (N,)  unnormalised log-weights
    """
    # predicted distances from each particle to each beacon
    diff = particles[:, None, :] - beacons[None, :, :]   # (N, K, 2)
    pred = np.linalg.norm(diff, axis=2)                   # (N, K)
    eps2 = (pred - y_t[None, :])**2                       # (N, K)
    return -LAM * eps2.sum(axis=1)                        # (N,)

def systematic_resample(weights):
    """
    Lower-variance resampling.  weights must sum to 1.
    Returns indices of chosen particles.
    """
    N   = len(weights)
    cdf = np.cumsum(weights)
    u0  = rng.uniform(0, 1/N)
    us  = u0 + np.arange(N) / N
    return np.searchsorted(cdf, us)

def particle_filter(observations, beacons, N, sigma_mov):
    T = len(observations)

    # initialise particles uniformly over bounding box of beacons
    lo, hi  = beacons.min(axis=0) - 1, beacons.max(axis=0) + 1
    particles = rng.uniform(lo, hi, size=(N, 2))
    weights   = np.ones(N) / N

    estimates = np.zeros((T, 2))
    all_particles = np.zeros((T, N, 2))

    for t in range(T):
        particles += rng.normal(0, sigma_mov, size=particles.shape)

        log_w = log_likelihood(particles, observations[t])
        log_w -= log_w.max()               # numerical stability
        weights = np.exp(log_w)
        weights /= weights.sum()           # normalise

        # 3. ESTIMATE: weighted mean
        estimates[t] = (weights[:, None] * particles).sum(axis=0)
        all_particles[t] = particles.copy()

        # 4. RESAMPLE
        idx = systematic_resample(weights)
        particles = particles[idx]
        weights   = np.ones(N) / N

    return estimates, all_particles

estimates, all_particles = particle_filter(observations, beacons, N, SIGMA_MOV)

fig, axes = plt.subplots(1, 2, figsize=(14, 6))

ax = axes[0]
snap_steps = [0, 20, 40, 60, 79]
colors      = plt.cm.Blues(np.linspace(0.3, 0.9, len(snap_steps)))

for idx_s, (s, c) in enumerate(zip(snap_steps, colors)):
    ax.scatter(all_particles[s, :, 0], all_particles[s, :, 1],
               s=6, color=c, alpha=0.35, zorder=2)

ax.plot(true_path[:, 0], true_path[:, 1],
        'k-', lw=2, label='True path', zorder=4)
ax.plot(estimates[:, 0], estimates[:, 1],
        'r--', lw=1.8, label='PF estimate', zorder=5)

ax.scatter(beacons[:, 0], beacons[:, 1],
           marker='^', s=120, c='green', zorder=6, label='Beacons')
for i, b in enumerate(beacons):
    ax.annotate(f'$b_{i+1}$', b, textcoords='offset points',
                xytext=(6, 4), fontsize=9, color='green')

ax.set_title('Particle cloud snapshots + estimated path')
ax.set_xlabel('x'); ax.set_ylabel('y')
ax.legend(fontsize=9)
ax.set_aspect('equal')
ax.grid(True, alpha=0.3)

ax2 = axes[1]
err = np.linalg.norm(estimates - true_path, axis=1)
ax2.plot(err, color='steelblue', lw=1.8)
ax2.axhline(err.mean(), color='red', ls='--', lw=1.2,
            label=f'Mean error = {err.mean():.3f} m')
ax2.fill_between(range(T), 0, err, alpha=0.15, color='steelblue')
ax2.set_title('Euclidean error over time')
ax2.set_xlabel('Time step $t$')
ax2.set_ylabel('$\|\\hat{x}_t - x_t\|$')
ax2.legend(fontsize=9)
ax2.grid(True, alpha=0.3)

plt.suptitle(
    f'Particle Filter Localization   (N={N}, $\\sigma_{{mov}}$={SIGMA_MOV}, '
    f'$\\sigma_{{obs}}$={SIGMA_OBS}, $\\lambda$={LAM:.2f})',
    fontsize=12)
plt.tight_layout()
plt.savefig('tser_pf_01.jpg')
