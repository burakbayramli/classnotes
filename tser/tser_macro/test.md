
```python
import pandas as pd
df = pd.read_csv('jpy.csv',parse_dates=['DATE'])
df = df.set_index('DATE')
df2 = pd.read_csv('jpypos.csv',parse_dates=['DATE'])
df2 = df2.set_index('DATE')
df = df.join(df2)
df = df.interpolate(method='linear')
df = df.dropna(axis=0)

df['S'] = df.nonclong - df.noncomshort
df['N'] = df.comlong - df.comshort
df['T'] = df['exp'] - df['imp']
df['e'] = df.jpyus
df['i'] = df.jpyrate - df.usrate
df = df.dropna(axis=0)
print df.tail(5)
df.to_csv('/tmp/out.csv')
```

```text
               jpyus  jpyrate  usrate          exp           imp  nonclong  \
DATE                                                                         
2017-09-01  110.7760    0.056    1.25  5968.324745  10807.305850   12596.0   
2017-10-01  112.9148    0.063    1.26  5617.537774  12047.994224   12596.0   
2017-11-01  112.8190    0.063    1.32  5773.435680  11529.345754   12596.0   
2017-12-01  112.9405    0.063    1.53  6433.521691  11960.532667   12596.0   
2018-01-01  110.8710    0.068    1.67  5650.378604  11302.129766   12596.0   

            noncomshort  comlong  comshort       S        N            T  \
DATE                                                                       
2017-09-01       3985.0   3043.0   17632.0  8611.0 -14589.0 -4838.981105   
2017-10-01       3985.0   3043.0   17632.0  8611.0 -14589.0 -6430.456450   
2017-11-01       3985.0   3043.0   17632.0  8611.0 -14589.0 -5755.910074   
2017-12-01       3985.0   3043.0   17632.0  8611.0 -14589.0 -5527.010976   
2018-01-01       3985.0   3043.0   17632.0  8611.0 -14589.0 -5651.751162   

                   e      i  
DATE                         
2017-09-01  110.7760 -1.194  
2017-10-01  112.9148 -1.197  
2017-11-01  112.8190 -1.257  
2017-12-01  112.9405 -1.467  
2018-01-01  110.8710 -1.602  
```

```python
import statsmodels.formula.api as smf
results = smf.ols('e ~ N + T + S ', data=df).fit()
print results.summary()
```

```text
                            OLS Regression Results                            
==============================================================================
Dep. Variable:                      e   R-squared:                       0.228
Model:                            OLS   Adj. R-squared:                  0.215
Method:                 Least Squares   F-statistic:                     18.01
Date:                Sun, 27 May 2018   Prob (F-statistic):           2.76e-10
Time:                        18:49:21   Log-Likelihood:                -727.68
No. Observations:                 187   AIC:                             1463.
Df Residuals:                     183   BIC:                             1476.
Df Model:                           3                                         
Covariance Type:            nonrobust                                         
==============================================================================
                 coef    std err          t      P>|t|      [95.0% Conf. Int.]
------------------------------------------------------------------------------
Intercept     94.2969      5.178     18.210      0.000        84.080   104.514
N             -0.0004   7.55e-05     -4.994      0.000        -0.001    -0.000
T             -0.0017      0.001     -2.000      0.047        -0.003 -2.29e-05
S             -0.0005   9.29e-05     -5.448      0.000        -0.001    -0.000
==============================================================================
Omnibus:                        6.112   Durbin-Watson:                   0.066
Prob(Omnibus):                  0.047   Jarque-Bera (JB):                3.317
Skew:                          -0.059   Prob(JB):                        0.190
Kurtosis:                       2.358   Cond. No.                     4.13e+05
==============================================================================

Warnings:
[1] Standard Errors assume that the covariance matrix of the errors is correctly specified.
[2] The condition number is large, 4.13e+05. This might indicate that there are
strong multicollinearity or other numerical problems.
```

```python
import statsmodels.formula.api as smf
results = smf.ols('S ~ e + i', data=df).fit()
print results.summary()
```

```text
                            OLS Regression Results                            
==============================================================================
Dep. Variable:                      S   R-squared:                       0.067
Model:                            OLS   Adj. R-squared:                  0.057
Method:                 Least Squares   F-statistic:                     6.621
Date:                Sun, 27 May 2018   Prob (F-statistic):            0.00167
Time:                        18:46:25   Log-Likelihood:                -2248.2
No. Observations:                 187   AIC:                             4502.
Df Residuals:                     184   BIC:                             4512.
Df Model:                           2                                         
Covariance Type:            nonrobust                                         
==============================================================================
                 coef    std err          t      P>|t|      [95.0% Conf. Int.]
------------------------------------------------------------------------------
Intercept   6.083e+04   2.61e+04      2.329      0.021      9307.175  1.12e+05
e           -718.8629    261.650     -2.747      0.007     -1235.084  -202.642
i           1094.9441   2094.571      0.523      0.602     -3037.520  5227.409
==============================================================================
Omnibus:                       15.134   Durbin-Watson:                   0.136
Prob(Omnibus):                  0.001   Jarque-Bera (JB):               10.560
Skew:                          -0.459   Prob(JB):                      0.00509
Kurtosis:                       2.283   Cond. No.                         926.
==============================================================================

Warnings:
[1] Standard Errors assume that the covariance matrix of the errors is correctly specified.
```


```python
import statsmodels.formula.api as smf
results = smf.ols('N ~ e + i', data=df).fit()
print results.summary()
```

```text
                            OLS Regression Results                            
==============================================================================
Dep. Variable:                      N   R-squared:                       0.037
Model:                            OLS   Adj. R-squared:                  0.026
Method:                 Least Squares   F-statistic:                     3.492
Date:                Sun, 27 May 2018   Prob (F-statistic):             0.0325
Time:                        18:46:43   Log-Likelihood:                -2285.3
No. Observations:                 187   AIC:                             4577.
Df Residuals:                     184   BIC:                             4586.
Df Model:                           2                                         
Covariance Type:            nonrobust                                         
==============================================================================
                 coef    std err          t      P>|t|      [95.0% Conf. Int.]
------------------------------------------------------------------------------
Intercept  -6.139e+04   3.18e+04     -1.928      0.055     -1.24e+05  1443.275
e            842.4412    319.101      2.640      0.009       212.874  1472.008
i           3905.6002   2554.474      1.529      0.128     -1134.225  8945.426
==============================================================================
Omnibus:                       20.995   Durbin-Watson:                   0.122
Prob(Omnibus):                  0.000   Jarque-Bera (JB):               12.730
Skew:                           0.492   Prob(JB):                      0.00172
Kurtosis:                       2.185   Cond. No.                         926.
==============================================================================

Warnings:
[1] Standard Errors assume that the covariance matrix of the errors is correctly specified.
```


```python
import statsmodels.tsa.stattools as t
res = t.grangercausalitytests(df[['i','e']],maxlag=2)
res = t.grangercausalitytests(df[['e','i']],maxlag=2)
```

```text

Granger Causality
('number of lags (no zero)', 1)
ssr based F test:         F=2.3413  , p=0.1277  , df_denom=183, df_num=1
ssr based chi2 test:   chi2=2.3797  , p=0.1229  , df=1
likelihood ratio test: chi2=2.3646  , p=0.1241  , df=1
parameter F test:         F=2.3413  , p=0.1277  , df_denom=183, df_num=1

Granger Causality
('number of lags (no zero)', 2)
ssr based F test:         F=4.7366  , p=0.0099  , df_denom=180, df_num=2
ssr based chi2 test:   chi2=9.7363  , p=0.0077  , df=2
likelihood ratio test: chi2=9.4888  , p=0.0087  , df=2
parameter F test:         F=4.7366  , p=0.0099  , df_denom=180, df_num=2

Granger Causality
('number of lags (no zero)', 1)
ssr based F test:         F=0.0087  , p=0.9259  , df_denom=183, df_num=1
ssr based chi2 test:   chi2=0.0088  , p=0.9252  , df=1
likelihood ratio test: chi2=0.0088  , p=0.9252  , df=1
parameter F test:         F=0.0087  , p=0.9259  , df_denom=183, df_num=1

Granger Causality
('number of lags (no zero)', 2)
ssr based F test:         F=0.3957  , p=0.6738  , df_denom=180, df_num=2
ssr based chi2 test:   chi2=0.8134  , p=0.6659  , df=2
likelihood ratio test: chi2=0.8116  , p=0.6665  , df=2
parameter F test:         F=0.3957  , p=0.6738  , df_denom=180, df_num=2
```

```python
import pandas as pd
df3 = pd.read_csv('bojliq.csv',sep='\s*')
df3['DATE'] = df3.apply(lambda x: pd.to_datetime("%d-%02d-01" % (x.year,x.mon)), axis=1)
df3 = df3.set_index('DATE')

import pandas as pd
df4 = pd.read_csv('curr.csv',parse_dates=['DATE'])
df4 = df4.interpolate(method='linear')
df4 = df4.set_index('DATE')
df4 = df4.join(df3)
df4['liqus'] = (df4['liqus'] - df4['liqus'].mean()) / df4['liqus'].std()
df4['liqjp'] = (df4['liqjp'] - df4['liqjp'].mean()) / df4['liqjp'].std()
#df4['exjpus'] = (df4['exjpus'] - df4['exjpus'].mean()) / df4['exjpus'].std()
#df4['exjpus'] = df4['exjpus'].pct_change()

df4['liqjpus'] = (df4.liqjp - df4.liqus)

df4 = df4.dropna(axis=0)

df4.to_csv('/tmp/out.csv')

import statsmodels.formula.api as smf
results = smf.ols('exjpus ~ liqjpus', data=df4).fit()
print results.summary()
```

```text
                            OLS Regression Results                            
==============================================================================
Dep. Variable:                 exjpus   R-squared:                       0.249
Model:                            OLS   Adj. R-squared:                  0.246
Method:                 Least Squares   F-statistic:                     75.96
Date:                Sun, 27 May 2018   Prob (F-statistic):           5.94e-16
Time:                        19:19:03   Log-Likelihood:                -1213.9
No. Observations:                 231   AIC:                             2432.
Df Residuals:                     229   BIC:                             2439.
Df Model:                           1                                         
Covariance Type:            nonrobust                                         
==============================================================================
                 coef    std err          t      P>|t|      [95.0% Conf. Int.]
------------------------------------------------------------------------------
Intercept    165.9374      3.122     53.148      0.000       159.786   172.089
liqjpus      -26.6226      3.055     -8.715      0.000       -32.641   -20.604
==============================================================================
Omnibus:                      172.690   Durbin-Watson:                   0.231
Prob(Omnibus):                  0.000   Jarque-Bera (JB):               21.421
Skew:                           0.398   Prob(JB):                     2.23e-05
Kurtosis:                       1.739   Cond. No.                         1.22
==============================================================================

Warnings:
[1] Standard Errors assume that the covariance matrix of the errors is correctly specified.
```

```python
import statsmodels.tsa.stattools as t
res = t.grangercausalitytests(df4[['liqjpus','exjpus']],maxlag=2)
res = t.grangercausalitytests(df4[['exjpus','liqjpus']],maxlag=2)
```

```text

Granger Causality
('number of lags (no zero)', 1)
ssr based F test:         F=0.1627  , p=0.6871  , df_denom=227, df_num=1
ssr based chi2 test:   chi2=0.1648  , p=0.6847  , df=1
likelihood ratio test: chi2=0.1648  , p=0.6848  , df=1
parameter F test:         F=0.1627  , p=0.6871  , df_denom=227, df_num=1

Granger Causality
('number of lags (no zero)', 2)
ssr based F test:         F=3.6223  , p=0.0283  , df_denom=224, df_num=2
ssr based chi2 test:   chi2=7.4062  , p=0.0246  , df=2
likelihood ratio test: chi2=7.2890  , p=0.0261  , df=2
parameter F test:         F=3.6223  , p=0.0283  , df_denom=224, df_num=2

Granger Causality
('number of lags (no zero)', 1)
ssr based F test:         F=1.9262  , p=0.1665  , df_denom=227, df_num=1
ssr based chi2 test:   chi2=1.9517  , p=0.1624  , df=1
likelihood ratio test: chi2=1.9435  , p=0.1633  , df=1
parameter F test:         F=1.9262  , p=0.1665  , df_denom=227, df_num=1

Granger Causality
('number of lags (no zero)', 2)
ssr based F test:         F=1.2886  , p=0.2777  , df_denom=224, df_num=2
ssr based chi2 test:   chi2=2.6347  , p=0.2678  , df=2
likelihood ratio test: chi2=2.6196  , p=0.2699  , df=2
parameter F test:         F=1.2886  , p=0.2777  , df_denom=224, df_num=2
```



















