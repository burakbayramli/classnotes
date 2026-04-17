"""
Bayesian AFT Churn Model — Online Retail Dataset
Log-normal AFT, Metropolis sampler, C-index evaluation.

pip install pandas numpy scipy tqdm
"""

import numpy as np
import pandas as pd
from scipy.special import log_ndtr   # log(Phi(z)), numerically stable
from tqdm import tqdm

RNG = np.random.default_rng(42)

# ─── 1. Load & clean ─────────────────────────────────────────────────────────

def load_data(path: str) -> pd.DataFrame:
    df = pd.read_csv(
        path,
        encoding="ISO-8859-1",
        dtype={"CustomerID": str},
        parse_dates=["InvoiceDate"],
    )
    df = df.dropna(subset=["CustomerID"])
    # Drop cancellations (InvoiceNo starts with 'C') and bad prices/quantities
    df = df[~df["InvoiceNo"].astype(str).str.startswith("C")]
    df = df[(df["Quantity"] > 0) & (df["UnitPrice"] > 0)]
    df["revenue"] = df["Quantity"] * df["UnitPrice"]
    return df


# ─── 2. Aggregate to invoice level ───────────────────────────────────────────

def to_invoices(df: pd.DataFrame) -> pd.DataFrame:
    inv = (
        df.groupby(["CustomerID", "InvoiceNo", "InvoiceDate"])
        .agg(spend=("revenue", "sum"), n_items=("StockCode", "nunique"))
        .reset_index()
    )
    inv = inv.sort_values(["CustomerID", "InvoiceDate"]).reset_index(drop=True)
    return inv


# ─── 3. Build episode table ───────────────────────────────────────────────────
#
# For each customer with purchases at t_1 < t_2 < ... < t_K:
#   Episode k (k=1..K-1): gap = t_{k+1} - t_k,  delta = 1
#   Episode K            : gap = T_ref - t_K,     delta = 0  (censored)
#
# If the episode straddles the cutoff date (start < cutoff <= end):
#   - Training version : gap = cutoff - start,  delta = 0  (censored at cutoff)
#   - Test    version  : gap = actual end,       delta = 1 or 0 as normal
#   (We assign the episode to whichever split its START belongs to.)

def build_episodes(inv: pd.DataFrame, cutoff: pd.Timestamp, t_end: pd.Timestamp) -> pd.DataFrame:
    records = []

    for cid, grp in inv.groupby("CustomerID"):
        grp = grp.sort_values("InvoiceDate").reset_index(drop=True)
        dates  = grp["InvoiceDate"].tolist()
        spends = grp["spend"].tolist()
        items  = grp["n_items"].tolist()
        K = len(grp)

        cum_spend = 0.0

        for k in range(K):
            t_start = dates[k]
            cum_spend += spends[k]          # cumulative spend UP TO and including this invoice

            if k < K - 1:
                t_next = dates[k + 1]
            else:
                t_next = None               # last purchase: censored at reference time

            # ── Covariates (all known at time of invoice k) ──────────────────
            prev_gap = (dates[k] - dates[k-1]).days if k > 0 else 0.0
            days_since_first = (t_start - dates[0]).days
            purchase_index = k + 1          # 1-based

            X = np.array([
                1.0,                                    # intercept
                prev_gap,                               # previous inter-purchase gap (days)
                np.log1p(purchase_index),               # log(1 + purchase index)
                spends[k],                              # spend at this invoice
                cum_spend,                              # cumulative spend so far
                float(items[k]),                        # distinct items in this invoice
                float(days_since_first),                # tenure (days since first purchase)
            ], dtype=np.float64)

            # ── Determine split & gap ────────────────────────────────────────
            if t_next is not None:
                actual_gap = (t_next - t_start).days
                actual_delta = 1
            else:
                actual_gap = (t_end - t_start).days
                actual_delta = 0

            if actual_gap <= 0:
                continue                    # same-day repeat: skip

            # Assign to train or test based on where t_start falls
            if t_start < cutoff:
                # Training episode: if event occurs after cutoff, censor at cutoff
                if t_next is not None and t_next > cutoff:
                    gap   = max((cutoff - t_start).days, 1)
                    delta = 0
                else:
                    gap   = actual_gap
                    delta = actual_delta
                split = "train"
            else:
                gap   = actual_gap
                delta = actual_delta
                split = "test"

            records.append({
                "CustomerID":    cid,
                "episode_k":     k,
                "t_start":       t_start,
                "gap_days":      gap,
                "delta":         delta,
                "split":         split,
                **{f"x{i}": X[i] for i in range(len(X))},
            })

    ep = pd.DataFrame(records)
    return ep


FEATURE_NAMES = [
    "intercept",
    "prev_gap",
    "log_purchase_idx",
    "spend",
    "cum_spend",
    "n_items",
    "days_since_first",
]


# ─── 4. Standardise covariates (fit on train, apply to both) ──────────────────

def standardise(ep: pd.DataFrame):
    feat_cols = [f"x{i}" for i in range(len(FEATURE_NAMES))]
    train_mask = ep["split"] == "train"

    mu  = ep.loc[train_mask, feat_cols].mean()
    std = ep.loc[train_mask, feat_cols].std().replace(0, 1)

    # Do NOT standardise the intercept (x0)
    mu["x0"]  = 0.0
    std["x0"] = 1.0

    ep[feat_cols] = (ep[feat_cols] - mu) / std
    return ep, mu, std


# ─── 5. Log-normal AFT log-posterior ─────────────────────────────────────────
#
# Model:  log(tau_i) = X_i @ beta + sigma * eps_i,   eps_i ~ N(0,1)
#
# Likelihood contributions:
#   delta=1 : f(z) / (sigma * tau)  where z = (log tau - X@beta) / sigma
#   delta=0 : S(z) = Phi(-z)
#
# Parameterisation for sampler: theta = [beta_0, ..., beta_p, log_sigma]
# Priors: beta_j ~ N(0, prior_sd^2),  log_sigma ~ N(0, 1)

def log_posterior(theta: np.ndarray, log_tau: np.ndarray,
                  X: np.ndarray, delta: np.ndarray,
                  prior_sd: float = 10.0) -> float:
    log_sigma = theta[-1]
    sigma     = np.exp(log_sigma)
    beta      = theta[:-1]

    xb = X @ beta
    z  = (log_tau - xb) / sigma

    # Log-likelihood
    ll_event   = -0.5 * z**2 - 0.5 * np.log(2 * np.pi) - log_sigma - log_tau
    ll_censored = log_ndtr(-z)                        # log(Phi(-z))

    ll = np.where(delta == 1, ll_event, ll_censored)
    if not np.all(np.isfinite(ll)):
        return -np.inf

    lp_total = ll.sum()

    # Priors
    lp_total += -0.5 * np.sum((beta / prior_sd) ** 2)   # beta priors
    lp_total += -0.5 * log_sigma ** 2                    # log-sigma prior

    return float(lp_total)


# ─── 6. Metropolis sampler ────────────────────────────────────────────────────

def metropolis(log_post_fn, theta_init: np.ndarray,
               n_iter: int = 30_000, burnin: int = 10_000,
               prop_sd: float = 0.05, adapt_every: int = 500) -> dict:
    """
    Random-walk Metropolis with adaptive proposal variance.
    Targets ~23% acceptance in high dimensions.
    """
    d      = len(theta_init)
    theta  = theta_init.copy()
    lp     = log_post_fn(theta)
    chain  = np.zeros((n_iter, d))
    lp_chain = np.zeros(n_iter)
    n_accept = 0
    cov      = np.eye(d) * prop_sd**2

    for i in tqdm(range(n_iter), desc="Metropolis"):
        proposal = theta + RNG.multivariate_normal(np.zeros(d), cov)
        lp_prop  = log_post_fn(proposal)

        log_alpha = lp_prop - lp
        if np.log(RNG.uniform()) < log_alpha:
            theta    = proposal
            lp       = lp_prop
            n_accept += 1

        chain[i]    = theta
        lp_chain[i] = lp

        # Adapt proposal covariance during burn-in
        if i > 100 and i % adapt_every == 0 and i < burnin:
            recent = chain[max(0, i-2000):i]
            rate   = n_accept / (i + 1)
            scale  = 2.38**2 / d   # optimal scaling (Gelman et al.)
            cov    = scale * (np.cov(recent.T) + 1e-6 * np.eye(d))
            # nudge scale to keep acceptance near 23%
            if rate > 0.3:
                cov *= 1.2
            elif rate < 0.15:
                cov *= 0.8

    samples  = chain[burnin:]
    acc_rate = n_accept / n_iter
    print(f"\nAcceptance rate: {acc_rate:.3f}  (target ~0.23)")
    return {"samples": samples, "chain": chain, "lp_chain": lp_chain, "acc_rate": acc_rate}


# ─── 7. Posterior summaries ───────────────────────────────────────────────────

def summarise_posterior(samples: np.ndarray, param_names: list) -> pd.DataFrame:
    rows = []
    for j, name in enumerate(param_names):
        s = samples[:, j]
        rows.append({
            "param":  name,
            "mean":   s.mean(),
            "sd":     s.std(),
            "q025":   np.quantile(s, 0.025),
            "q50":    np.quantile(s, 0.50),
            "q975":   np.quantile(s, 0.975),
        })
    return pd.DataFrame(rows)


# ─── 8. Prediction: S(W | X) ─────────────────────────────────────────────────
#
# For a given window W (days), P(churn) = P(tau > W | X) = Phi(-z_W)
# where z_W = (log W - X @ beta) / sigma.
# We average over posterior samples (posterior predictive).

def predict_survival(X_test: np.ndarray, samples: np.ndarray, W: float) -> np.ndarray:
    """
    Returns P(tau > W | X) for each test episode, averaged over posterior samples.
    Shape: (n_test,)
    """
    from scipy.special import ndtr  # Phi(z)
    n_test  = X_test.shape[0]
    n_samp  = samples.shape[0]
    surv    = np.zeros(n_test)

    log_W = np.log(W)
    for s in range(n_samp):
        beta  = samples[s, :-1]
        sigma = np.exp(samples[s, -1])
        xb    = X_test @ beta
        z     = (log_W - xb) / sigma
        surv += ndtr(-z)            # Phi(-z) = S(W)

    return surv / n_samp


# ─── 9. C-index (concordance) ────────────────────────────────────────────────
#
# For pairs (i, j) where both are uncensored (or i is uncensored with tau_i < tau_j):
#   concordant if the model assigns lower predicted survival time to i than j.
# We use predicted median survival time = exp(X @ beta_hat) as the ranking score.

def concordance_index(gap: np.ndarray, delta: np.ndarray,
                      risk_score: np.ndarray) -> float:
    """
    risk_score: higher = predicted to churn sooner (shorter gap).
    Standard Harrell C-index with proper censoring handling.
    """
    n = len(gap)
    concordant = 0
    permissible = 0

    for i in range(n):
        for j in range(n):
            if i == j:
                continue
            # Pair (i,j) is permissible if i had the event first (or i had event, j censored later)
            if delta[i] == 1 and gap[i] < gap[j]:
                permissible += 1
                if risk_score[i] > risk_score[j]:   # model predicts i churns sooner
                    concordant += 1
                elif risk_score[i] == risk_score[j]:
                    concordant += 0.5

    return concordant / permissible if permissible > 0 else float("nan")


# ─── 10. Main pipeline ────────────────────────────────────────────────────────

def main(data_path: str = "online_retail.csv"):

    print("Loading data...")
    df  = load_data(data_path)
    inv = to_invoices(df)

    T_END    = inv["InvoiceDate"].max()
    CUTOFF   = pd.Timestamp("2011-10-01")   # ~85th percentile of date range
    print(f"Date range : {inv['InvoiceDate'].min().date()} -> {T_END.date()}")
    print(f"Cutoff     : {CUTOFF.date()}")

    print("\nBuilding episodes...")
    ep = build_episodes(inv, CUTOFF, T_END)
    print(f"Total episodes : {len(ep)}")
    print(ep.groupby(["split","delta"]).size().rename("count").to_string())

    print("\nStandardising covariates...")
    ep, feat_mu, feat_std = standardise(ep)

    feat_cols = [f"x{i}" for i in range(len(FEATURE_NAMES))]

    train = ep[ep["split"] == "train"].reset_index(drop=True)
    test  = ep[ep["split"] == "test" ].reset_index(drop=True)

    X_train   = train[feat_cols].values
    log_tau_tr = np.log(train["gap_days"].values.astype(float))
    delta_tr   = train["delta"].values.astype(float)

    X_test    = test[feat_cols].values
    log_tau_te = np.log(test["gap_days"].values.astype(float))
    delta_te   = test["delta"].values.astype(float)

    # ── Initialise at OLS estimate (fast warm start) ──────────────────────────
    p = X_train.shape[1]
    beta_init = np.linalg.lstsq(X_train, log_tau_tr, rcond=None)[0]
    resid     = log_tau_tr - X_train @ beta_init
    log_sigma_init = np.log(resid.std() + 1e-6)
    theta_init = np.append(beta_init, log_sigma_init)

    print(f"\nInitial log-posterior: {log_posterior(theta_init, log_tau_tr, X_train, delta_tr):.2f}")

    # ── Run sampler ───────────────────────────────────────────────────────────
    lp_fn = lambda th: log_posterior(th, log_tau_tr, X_train, delta_tr, prior_sd=10.0)

    result = metropolis(lp_fn, theta_init, n_iter=30_000, burnin=10_000, prop_sd=0.05)
    samples = result["samples"]   # shape (20_000, p+1)

    # ── Posterior summary ─────────────────────────────────────────────────────
    param_names = FEATURE_NAMES + ["log_sigma"]
    summary = summarise_posterior(samples, param_names)
    print("\nPosterior summary")
    print(summary.to_string(index=False, float_format="{:.4f}".format))

    # ── C-index on test set ───────────────────────────────────────────────────
    # Risk score: posterior mean of -X@beta  (higher = shorter predicted gap = sooner churn)
    beta_post_mean = samples[:, :-1].mean(axis=0)
    risk_score     = -X_test @ beta_post_mean   # higher risk → shorter gap

    c_idx = concordance_index(test["gap_days"].values, delta_te, risk_score)
    print(f"\nC-index (test set): {c_idx:.4f}")

    # ── Brier-style churn accuracy at W=90 days ───────────────────────────────
    W = 90   # churn window (days)
    p_churn = predict_survival(X_test, samples, W)   # P(tau > W) = P(no churn in W days)
    # Observed churn: 1 if gap > W (did NOT re-purchase within W days)
    y_churn = (test["gap_days"].values > W).astype(float)

    # Only evaluate on uncensored episodes where the outcome is unambiguous:
    # delta=1 means we observed the next purchase → outcome is definitive.
    # For censored (delta=0), gap < W means definitely churned; gap >= W is ambiguous.
    eval_mask = (delta_te == 1) | (test["gap_days"].values < W)
    brier = np.mean((p_churn[eval_mask] - y_churn[eval_mask])**2)
    print(f"Brier score (W={W}d, n={eval_mask.sum()}): {brier:.4f}")

    return {
        "samples":     samples,
        "summary":     summary,
        "c_index":     c_idx,
        "brier":       brier,
        "episodes":    ep,
        "feat_mu":     feat_mu,
        "feat_std":    feat_std,
    }


