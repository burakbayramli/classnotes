# Ported to Python via Gemini 3.6 Flash
import numpy as np
import pandas as pd
from scipy.stats import t

def mlag(x, nlag):
    """Generates matrix of lagged variables (mimics LeSage's mlag.m)."""
    nobs, nvar = x.shape
    xlag = np.zeros((nobs, nvar * nlag))
    for i in range(1, nlag + 1):
        xlag[i:, (i - 1) * nvar : i * nvar] = x[:-i, :]
    return xlag

def scstd(y, nlag):
    """Computes univariate AR residual standard deviation for scaling (mimics scstd.m)."""
    nobs = len(y)
    # Build lag matrix for univariate AR
    ylag = mlag(y.reshape(-1, 1), nlag)
    X = np.hstack([ylag[nlag:, :], np.ones((nobs - nlag, 1))])
    Y = y[nlag:]
    
    # OLS estimation
    beta = np.linalg.lstsq(X, Y, rcond=None)[0]
    residuals = Y - X @ beta
    sige = np.sum(residuals**2) / (nobs - nlag - nlag - 1)
    return np.sqrt(sige)

def bvar(y, nlag, tight=0.1, weight=0.5, decay=1.0, vnames=None):
    """
    Performs Bayesian Vector Autoregression via Theil-Goldberger Mixed Estimation.
    Replicates LeSage's bvar.m and theilbv.m.
    """
    nobs, neqs = y.shape
    nobse = nobs - nlag
    k = neqs * nlag + 1  # total variables per equation including constant
    
    # 1. Scale factors using univariate AR model
    scale = np.zeros(neqs)
    for j in range(neqs):
        scale[j] = scstd(y[:, j], nlag)
        
    scale2 = np.zeros((neqs, neqs))
    for j in range(neqs):
        for i in range(neqs):
            scale2[i, j] = scale[j] / scale[i]

    # 2. Build lagged explanatory matrix X
    xlag = mlag(y, nlag)
    xmat = np.hstack([xlag[nlag:nobs, :], np.ones((nobse, 1))])
    
    # 3. Setup weight matrix
    if np.isscalar(weight):
        wght = np.full((neqs, neqs), weight)
        np.fill_diagonal(wght, 1.0)
    else:
        wght = np.array(weight)

    results = []

    # 4. Estimate equation-by-equation (Theil-Goldberger)
    for eqn in range(neqs):
        yvec = y[nlag:nobs, eqn]
        
        # Calculate Doan's sigma(i, j, l)
        sigma = np.zeros(k)
        idx = 0
        for l in range(nlag):
            ldecay = 1.0 / ((l + 1) ** decay)
            for j in range(neqs):
                sigma[idx] = (tight * wght[eqn, j] * ldecay) * scale2[j, eqn]
                idx += 1
                
        # Build prior R diagonal matrix
        R = np.zeros((k, k))
        for i in range(k - 1):  # exclude constant (diffuse prior)
            R[i, i] = scale[eqn] / sigma[i]
            
        # Build prior c vector
        c = np.zeros(k)
        cind = eqn * nlag  # position of own lag 1
        c[cind] = scale[eqn] / sigma[cind]
        
        # Augmented Normal Equations (X'X + R'R) \ (X'y + R'c)
        xpxrpr = xmat.T @ xmat + R.T @ R
        xpxi = np.linalg.inv(xpxrpr)
        xpyrpc = xmat.T @ yvec + R.T @ c
        
        beta = xpxi @ xpyrpc
        yhat = xmat @ beta
        resid = yvec - yhat
        
        # Standard error and t-statistics (Litterman degrees of freedom)
        sigu = np.sum(resid**2)
        sige = sigu / (nobse - 1)
        
        se = np.sqrt(sige * np.diag(xpxi))
        tstat = beta / se
        tprob = 2.0 * (1.0 - t.cdf(np.abs(tstat), df=nobse - 1))
        
        # R-squared metrics
        ym = yvec - np.mean(yvec)
        rsqr = 1.0 - (sigu / np.sum(ym**2))
        rbar = 1.0 - ((sigu / (nobse - k)) / (np.sum(ym**2) / (nobse - 1)))
        
        # Build variable names list for printing
        var_labels = []
        for l in range(1, nlag + 1):
            for eq_idx in range(neqs):
                v_name = vnames[eq_idx] if vnames else f"var{eq_idx+1}"
                var_labels.append(f"{v_name} lag{l}")
        var_labels.append("constant")

        eq_res = pd.DataFrame({
            "Variable": var_labels,
            "Coefficient": beta,
            "t-statistic": tstat,
            "t-probability": tprob
        }).set_index("Variable")

        results.append({
            "eqn_name": vnames[eqn] if vnames else f"eq_{eqn+1}",
            "rsqr": rsqr,
            "rbar": rbar,
            "sige": sige,
            "summary": eq_res
        })
        
    return results

def print_results(results):
    """Prints output formatted like LeSage's prt_var."""
    print("***** Bayesian Vector Autoregressive Model *****\n")
    for res in results:
        print(f"Dependent Variable = {res['eqn_name']:>16}")
        print(f"R-squared     = {res['rsqr']:>9.4f}")
        print(f"Rbar-squared  = {res['rbar']:>9.4f}")
        print(f"sige          = {res['sige']:>9.4f}")
        print("*" * 66)
        print(res["summary"].to_string(formatters={
            'Coefficient': '{:12.6f}'.format,
            't-statistic': '{:12.6f}'.format,
            't-probability': '{:12.6f}'.format
        }))
        print("\n")

def write_results(results, fout):
    fout = open(fout, "w")
    for res in results:
        fout.write(f"Dependent Variable = {res['eqn_name']:>16}\n")
        fout.write(f"R-squared     = {res['rsqr']:>9.4f}\n")
        fout.write(f"Rbar-squared  = {res['rbar']:>9.4f}\n")
        fout.write(f"sige          = {res['sige']:>9.4f}\n")
        fout.write("*" * 66)
        fout.write("\n")
        fout.write(res["summary"].to_string(formatters={
            'Coefficient': '{:12.6f}'.format,
            't-statistic': '{:12.6f}'.format,
            't-probability': '{:12.6f}'.format
        }))
        fout.write("\n")
    
