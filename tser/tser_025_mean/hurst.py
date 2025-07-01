from numpy import cumsum, log, polyfit, sqrt, std, subtract
from numpy.random import randn
import numpy as np

def hurst(ts):
    lags = range(2, 100)
    
    stds = [std(subtract(ts[lag:].values, ts[:-lag].values)) for lag in lags]
    
    valid_lags = []
    valid_tau = []
    for i, s in enumerate(stds):
        if s > 0 and not np.isnan(s) and not np.isinf(s):
            valid_lags.append(lags[i])
            valid_tau.append(sqrt(s))
    
    if not valid_lags or not valid_tau:
        print("Not enough valid data points to calculate Hurst exponent.")
        return np.nan

    poly = polyfit(log(valid_lags), log(valid_tau), 1)
    res = poly[0]*2.0
    return res
