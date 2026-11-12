import numpy as np
import pandas as pd
import statsmodels.api as sm
from sklearn.metrics import roc_auc_score

H, MIN_YEAR, MAX_YEAR = 1, 1950, 2019
TRAIN_END = 2010
FEATS = ["ca", "rer_dev", "exports", "output", "infl", "credit"]

dlog = lambda s: np.log(s.where(s > 0)).diff()

def construct(g, us_cpi):
    g = g.sort_values("year").set_index("year")
    g = g.reindex(range(int(g.index.min()), int(g.index.max()) + 1))
    g["ISO3"] = g["ISO3"].ffill().bfill()
    idx = g.index

    rer = np.log((g["USDfx"] * us_cpi.reindex(idx) / g["CPI"]).where(lambda s: s > 0))
    rer = rer.replace([np.inf, -np.inf], np.nan)    

    # Rolling 20-year linear trend, using only past/current data
    window = 20
    trend_values = pd.Series(index=idx, dtype=float)
    for i in range(len(rer)):
        sub = rer.iloc[max(0, i - window + 1):i + 1].dropna()
        if len(sub) >= 5:
            p = np.polyfit(sub.index, sub.values, 1)
            trend_values.iloc[i] = np.polyval(p, idx[i])
    rer_dev = rer - trend_values
    
    R = pd.DataFrame({
        "ca":      -g["CA_GDP"],
        "rer_dev": -rer_dev,
        "exports": -dlog(g["exports_USD"]),
        "output":  -dlog(g["rGDP"]),
        "infl":    g["infl"],
        "credit":  dlog(g["M2"]) - dlog(g["nGDP"]),
    }).replace([np.inf, -np.inf], np.nan)

    # Expanding-window standardization (no look-ahead)
    R_scaled = R.apply(
        lambda col: (col - col.expanding(min_periods=5).mean())
        / col.expanding(min_periods=5).std()
    )

    fwd = pd.concat([g["CurrencyCrisis"].shift(-k) for k in range(1, H + 1)], axis=1)
    R_scaled["target"] = fwd.max(axis=1).where(fwd.notna().any(axis=1))
    R_scaled["crisis"] = g["CurrencyCrisis"]
    R_scaled["ISO3"] = g["ISO3"]
    R_scaled["year"] = idx
    return R_scaled.reset_index(drop=True)

def tier(p, cut_amber, cut_red):
    return "RED" if p >= cut_red else ("AMBER" if p >= cut_amber else "GREEN")

def train_logit():

    # ----------------------------- Data -------------------------------
    df = pd.read_csv("/opt/Downloads/gmd/gmd.csv")
    us_cpi = df[df.ISO3 == "USA"].set_index("year")["CPI"]

    # Full feature table (unfiltered) -- used for scoring any country-year
    full = pd.concat([construct(g, us_cpi) for _, g in df.groupby("ISO3")], ignore_index=True)

    # Modelling panel (filtered)
    panel = full[(full.year >= MIN_YEAR) & (full.year <= MAX_YEAR)]
    panel = panel[panel["crisis"] != 1]
    panel = panel.dropna(subset=FEATS + ["target"])

    # ----------------------------- Train / test -----------------------
    train = panel[panel.year <= TRAIN_END].copy()
    test = panel[panel.year > TRAIN_END].copy()

    print(f"Train obs: {len(train)}, Test obs: {len(test)}")
    print("Test target distribution:")
    print(test.target.value_counts())

    Xtr = sm.add_constant(train[FEATS])
    logit = sm.Logit(train["target"], Xtr).fit(
        disp=0, cov_type="cluster", cov_kwds={"groups": pd.factorize(train.ISO3)[0]}
    )
    print(logit.summary())

    def score(frame):
        return logit.predict(sm.add_constant(frame[FEATS], has_constant="add"))

    train.loc[:, "score"] = score(train)
    test.loc[:, "score"] = score(test)

    print("\nAUC in-sample (train):", roc_auc_score(train.target, train.score))
    print("AUC out-of-time (test):", roc_auc_score(test.target, test.score))

    Q_AMBER, Q_RED = 0.80, 0.90                     # chosen a priori
    cut_amber = train["score"].quantile(Q_AMBER)
    cut_red   = train["score"].quantile(Q_RED)

    # Historical reference: what each tier delivered in the TRAINING sample
    train["tier"] = train["score"].apply(tier, args=(cut_amber, cut_red))
    tier_ref = train.groupby("tier")["target"].agg(n="size", crises="sum", hit_rate="mean")
    tier_ref["lift_vs_train_base"] = tier_ref["hit_rate"] / train["target"].mean()
    print("\nTier reference (training sample, in-sample):")
    print(tier_ref)
    
    return logit, tier_ref, cut_amber, cut_red, full, train

def predict_pair(iso3, target_year, logit, tier_ref, cut_amber, cut_red, full, train):
    feat_year = target_year - H

    row = full[(full.ISO3 == iso3) & (full.year == feat_year)]
    if row.empty:
        raise ValueError(f"No feature row for {iso3} in {feat_year}")
    row = row.iloc[0]

    x = row[FEATS].astype(float)
    missing = x[x.isna()].index.tolist()
    if missing:
        x[missing] = 0.0          # neutral z-score: no contribution from these terms

    beta = logit.params
    contrib = beta[FEATS] * x
    log_odds = beta["const"] + contrib.sum()
    prob = 1.0 / (1.0 + np.exp(-log_odds))
    pct = (train["score"] < prob).mean() * 100
    t = tier(prob, cut_amber, cut_red)

    # Optional: realized outcome, only if the data has it
    tgt = full[(full.ISO3 == iso3) & (full.year == target_year)]
    realized = tgt["crisis"].iloc[0] if not tgt.empty else np.nan

    out = {
        "ISO3": iso3,
        "target_year": target_year,
        "features_from_year": feat_year,
        "probability": prob,
        "percentile_vs_train": pct,
        "tier": t,
        "tier_train_hit_rate": tier_ref.loc[t, "hit_rate"],
        "train_base_rate": train["target"].mean(),
        "crisis_in_feature_year": bool(row["crisis"] == 1),
        "realized_crisis_in_target_year": realized,   # NaN if unknown,
        "imputed_features": missing
    }
    detail = pd.DataFrame({"z_score": x, "coef": beta[FEATS], "contribution": contrib})
    return out, detail

def predict(TARGET_ISO3,TARGET_YEAR, logit, tier_ref, cut_amber, cut_red, full, train):

    #TARGET_ISO3 = "TUR"
    #TARGET_YEAR = 2027      # features observed in this year -> crisis probability in TARGET_YEAR + H
    res, detail = predict_pair(TARGET_ISO3, TARGET_YEAR, logit, tier_ref, cut_amber, cut_red, full, train)
    print("\n=== Single country-year prediction ===")
    for k, v in res.items():
        print(f"{k:>32}: {v:.4f}" if isinstance(v, float) else f"{k:>32}: {v}")
    print("\nFeature breakdown:")
    print(detail.round(3))
    
if __name__ == "__main__": 
    logit, tier_ref, cut_amber, cut_red, full, train = train_logit()
    predict("TUR", 2027, logit, tier_ref, cut_amber, cut_red, full, train)
    
