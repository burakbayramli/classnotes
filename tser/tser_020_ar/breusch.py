import numpy as np
import statsmodels.api as sm
import scipy as sp

def breusch_pagan_test(y,x):
     results=sm.OLS(y,x).fit()
     resid=results.resid
     n=len(resid)
     sigma2 = sum(resid**2)/n
     f = resid**2/sigma2 - 1
     results2=sm.OLS(f,x).fit()
     fv=results2.fittedvalues
     bp=0.5 * sum(fv**2)
     df=results2.df_model
     p_value=1-sp.stats.chi.cdf(bp,df)
     return round(bp,6), df, round(p_value,7)

