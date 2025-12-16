import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import pymc as pm
import arviz as az
import data

years = np.arange(1950, 2011)

sim = data.generate_synthetic(years=years,
                              alpha_log=3.2,
                              beta_log=np.log(1.05),   # ~5% higher near
                              sigma_year=0.4,
                              obs_model="poisson",
                              trend=data.slow_trend)

df = pd.DataFrame({
    "year": sim["years"],
    "near": sim["near"],
    "far": sim["far"]
})

SEED = 123
np.random.seed(SEED)

# quick plot of generated counts
plt.figure(figsize=(10,4))
plt.plot(df.year, df.near, label="near", marker="o")
plt.plot(df.year, df.far, label="far", marker="x")
plt.xlabel("Year"); plt.ylabel("Counts")
plt.title("Synthetic counts (Poisson)")
plt.legend()
plt.tight_layout()
plt.savefig('stat_082_rapoi_01.jpg')

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

    graphviz = pm.model_to_graphviz(model_synth)
    graphviz.graph_attr.update(dpi="100")
    graphviz.render("stat_082_rapoi_03", format="jpg")
    
print(az.summary(idata, var_names=["alpha", "beta", "sigma_year", "rate_ratio"], round_to=3))

# Plot posterior of rate ratio
rr_samples = idata.posterior["rate_ratio"].values.flatten()
plt.figure(figsize=(6,3))
az.plot_posterior(rr_samples, hdi_prob=0.95)
plt.axvline(np.exp(sim["beta_log"]), color="red", linestyle="--", label="true ratio")
plt.title("Recovered rate ratio (Poisson synthetic)")
plt.legend()
plt.tight_layout()
plt.savefig('stat_082_rapoi_02.jpg')

