import statsmodels.formula.api as smf
import pandas as pd
import numpy as np

def halflife(df,col):
    df['ylag'] = df[col].shift(1)
    df['deltaY'] = df[col] - df['ylag']
    results = smf.ols('deltaY ~ ylag', data=df).fit()
    lam = results.params['ylag']
    halflife=-np.log(2)/lam
    return lam, halflife
