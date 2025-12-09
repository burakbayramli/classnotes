import numpy as np
import pandas as pd

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

def slow_trend(yrs):
    # small logistic/increasing reporting effect to mimic internet-era rise
    return 0.02 * (yrs - yrs.mean()) / (yrs.max() - yrs.min()) * 12.0

