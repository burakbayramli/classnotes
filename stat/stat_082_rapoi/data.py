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
    log_mu_B  = alpha_log + year_effects + trend_vals
    log_mu_A = alpha_log + beta_log + year_effects + trend_vals

    # 4) Convert to rates
    mu_B  = np.exp(log_mu_B)
    mu_A = np.exp(log_mu_A)

    # 5) Draw observations
    if obs_model == "poisson":
        y_B  = np.random.poisson(mu_B)
        y_A = np.random.poisson(mu_A)
    elif obs_model == "nb":
        # Parameterization: NB(mean=mu, alpha=nb_dispersion) where Var = mu + mu^2/alpha
        # PyMC uses alpha parameter matching this.
        alpha = nb_dispersion
        p_B = alpha / (alpha + mu_B)
        p_A = alpha / (alpha + mu_A)
        # Generate using Gamma-Poisson mixture via NegBin in numpy: use Gamma/Gamma?
        # Simpler: draw Gamma( alpha, scale=mu/alpha ) then Poisson; but we can use numpy's negative_binomial
        # numpy's negbin(n, p) returns number of failures; transform needed. Use Gamma-Poisson approach:
        gamma_shape = alpha
        gamma_scale_B = mu_B / alpha
        gamma_scale_A = mu_A / alpha
        lam_B = np.random.gamma(shape=gamma_shape, scale=gamma_scale_B)
        lam_A = np.random.gamma(shape=gamma_shape, scale=gamma_scale_A)
        y_B  = np.random.poisson(lam_B)
        y_A = np.random.poisson(lam_A)
    else:
        raise ValueError("obs_model must be 'poisson' or 'nb'")

    return {
        "years": years,
        "A": y_A,
        "B": y_B,
        "mu_A": mu_A,
        "mu_B": mu_B,
        "log_mu_A": log_mu_A,
        "log_mu_B": log_mu_B,
        "year_effects": year_effects,
        "alpha_log": alpha_log,
        "beta_log": beta_log,
        "sigma_year": sigma_year,
        "obs_model": obs_model
    }

def slow_trend(yrs):
    # small logistic/increasing reporting effect to mimic internet-era rise
    return 0.02 * (yrs - yrs.mean()) / (yrs.max() - yrs.min()) * 12.0

