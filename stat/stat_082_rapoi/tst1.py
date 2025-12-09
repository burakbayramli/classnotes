# Synthetic UFO-like data generator + PyMC fit example
# - Produces counts per year for two groups (near, far)
# - Shared year effects u_t (same for both groups)
# - Group effect beta (log-scale) constant across years
# - Option: Poisson or NegativeBinomial observation model
#
# Requirements:
#   pip install numpy pandas matplotlib pymc arviz
#
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import pymc as pm
import arviz as az

SEED = 123
np.random.seed(SEED)

def generate_synthetic(years=None,
                       alpha_log=3.5,      # baseline log-rate (log mu)
                       beta_log=0.05,      # log rate ratio (near vs far)
                       sigma_year=0.5,     # sd of year effects on log-scale
                       obs_model="poisson",# "poisson" or "nb"
                       nb_dispersion=2.0,  # for NB: alpha parameter (larger -> less overdispersion)
                       trend=None          # optional callable: trend(years) -> additive log-term
                      ):
    """
    Returns: dict containing years, near_counts, far_counts, true parameters
    """
    if years is None:
        years = np.arange(1950, 2011)  # similar span to UFO example

    n_years = len(years)

    # 1) Year effects: zero-centered fluctuations (shared)
    #    Drawn i.i.d. Normal(0, sigma_year). You can replace with RW1 for smoothness.
    year_effects = np.random.normal(0, sigma_year, size=n_years)

    # 2) Optional trend: additive on log scale (e.g. slowly increasing reporting)
    if trend is None:
        trend_vals = np.zeros(n_years)
    else:
        trend_vals = np.asarray(trend(years))
        assert trend_vals.shape[0] == n_years

    # 3) Linear predictors (log-scale) for far and near
    log_mu_far  = alpha_log + year_effects + trend_vals
    log_mu_near = alpha_log + beta_log + year_effects + trend_vals

    # 4) Convert to rates
    mu_far  = np.exp(log_mu_far)
    mu_near = np.exp(log_mu_near)

    # 5) Draw observations
    if obs_model == "poisson":
        y_far  = np.random.poisson(mu_far)
        y_near = np.random.poisson(mu_near)
    elif obs_model == "nb":
        # Parameterization: NB(mean=mu, alpha=nb_dispersion) where Var = mu + mu^2/alpha
        # PyMC uses alpha parameter matching this.
        alpha = nb_dispersion
        p_far = alpha / (alpha + mu_far)
        p_near = alpha / (alpha + mu_near)
        # Generate using Gamma-Poisson mixture via NegBin in numpy: use Gamma/Gamma?
        # Simpler: draw Gamma( alpha, scale=mu/alpha ) then Poisson; but we can use numpy's negative_binomial
        # numpy's negbin(n, p) returns number of failures; transform needed. Use Gamma-Poisson approach:
        gamma_shape = alpha
        gamma_scale_far = mu_far / alpha
        gamma_scale_near = mu_near / alpha
        lam_far = np.random.gamma(shape=gamma_shape, scale=gamma_scale_far)
        lam_near = np.random.gamma(shape=gamma_shape, scale=gamma_scale_near)
        y_far  = np.random.poisson(lam_far)
        y_near = np.random.poisson(lam_near)
    else:
        raise ValueError("obs_model must be 'poisson' or 'nb'")

    return {
        "years": years,
        "near": y_near,
        "far": y_far,
        "mu_near": mu_near,
        "mu_far": mu_far,
        "log_mu_near": log_mu_near,
        "log_mu_far": log_mu_far,
        "year_effects": year_effects,
        "alpha_log": alpha_log,
        "beta_log": beta_log,
        "sigma_year": sigma_year,
        "obs_model": obs_model
    }

# ----------------------------
# Example 1: Poisson synthetic data with gentle trend
# ----------------------------
years = np.arange(1950, 2011)

def slow_trend(yrs):
    # small logistic/increasing reporting effect to mimic internet-era rise
    return 0.02 * (yrs - yrs.mean()) / (yrs.max() - yrs.min()) * 12.0

sim = generate_synthetic(years=years,
                         alpha_log=3.2,
                         beta_log=np.log(1.05),   # ~5% higher near
                         sigma_year=0.4,
                         obs_model="poisson",
                         trend=slow_trend)

df = pd.DataFrame({
    "year": sim["years"],
    "near": sim["near"],
    "far": sim["far"]
})

df.to_csv("counts.csv",index=None)
exit()
# quick plot of generated counts
plt.figure(figsize=(10,4))
plt.plot(df.year, df.near, label="near", marker="o")
plt.plot(df.year, df.far, label="far", marker="x")
plt.xlabel("Year"); plt.ylabel("Counts")
plt.title("Synthetic counts (Poisson)")
plt.legend()
plt.tight_layout()
plt.show()

# ----------------------------
# Fit the hierarchical Poisson model to the synthetic data (recover beta)
# ----------------------------
# Build data arrays in the same stacked format you used earlier
def build_stacked_arrays(simdict):
    years = np.array(simdict["years"])
    near = np.array(simdict["near"])
    far  = np.array(simdict["far"])
    n_years = len(years)
    counts = np.concatenate([near, far])
    group = np.concatenate([np.ones(n_years, dtype=int), np.zeros(n_years, dtype=int)])
    year_idx = np.concatenate([np.arange(n_years), np.arange(n_years)])
    return years, counts, group, year_idx, near, far

years, counts_pois, group_pois, year_idx_pois, near_arr, far_arr = build_stacked_arrays(sim)
n_years = len(years)

with pm.Model() as model_synth:
    sigma_year = pm.HalfNormal("sigma_year", sigma=1.0)
    year_offset = pm.Normal("year_offset", 0.0, 1.0, shape=n_years)
    year_effect = pm.Deterministic("year_effect", year_offset * sigma_year)

    alpha = pm.Normal("alpha", 0.0, 2.0)
    beta  = pm.Normal("beta", 0.0, 1.0)

    log_mu = alpha + beta * group_pois + year_effect[year_idx_pois]
    mu = pm.math.exp(log_mu)

    obs = pm.Poisson("obs", mu=mu, observed=counts_pois)

    rate_ratio = pm.Deterministic("rate_ratio", pm.math.exp(beta))

    idata = pm.sample(1000, tune=1000, target_accept=0.9, return_inferencedata=True, random_seed=SEED)

print(az.summary(idata, var_names=["alpha", "beta", "sigma_year", "rate_ratio"], round_to=3))

# Plot posterior of rate ratio
rr_samples = idata.posterior["rate_ratio"].values.flatten()
plt.figure(figsize=(6,3))
az.plot_posterior(rr_samples, hdi_prob=0.95)
plt.axvline(np.exp(sim["beta_log"]), color="red", linestyle="--", label="true ratio")
plt.title("Recovered rate ratio (Poisson synthetic)")
plt.legend()
plt.tight_layout()
plt.savefig('out1.jpg')

