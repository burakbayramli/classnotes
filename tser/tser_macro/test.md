
```python
import pandas as pd
df = pd.read_csv('jpy.csv',parse_dates=['DATE'])
df = df.set_index('DATE')

df2 = pd.read_csv('jpypos.csv',parse_dates=['DATE'])
df2 = df2.set_index('DATE')
df = df.join(df2)
df = df.dropna(axis=0)

df['S'] = df.nonclong + df.noncomshort
df['N'] = df.comlong + df.comshort
df['T'] = df['exp'] + df['imp']
df['S'] = df.S.pct_change() 
df['N'] = df.N.pct_change()
df['T'] = df['T'].pct_change()
df['e'] = df.jpyus.pct_change()
df['i'] = df.usrate - df.jpyrate
df = df.dropna(axis=0)
print df.tail(5)
```

```text
               jpyus  jpyrate  usrate          exp           imp  nonclong  \
DATE                                                                         
2015-12-01  121.6350    0.169    0.54  4759.099756  11312.683452   31228.0   
2016-03-01  112.9317    0.098    0.55  5374.393013  12085.645915   94070.0   
2016-11-01  108.4430    0.056    0.71  5438.801950  11335.202965   81010.0   
2017-08-01  109.8270    0.056    1.25  5409.476394  11962.985700   36445.0   
2017-08-01  109.8270    0.056    1.25  5409.476394  11962.985700   12596.0   

            noncomshort   comlong  comshort         S         N         T  \
DATE                                                                        
2015-12-01     106129.0  197457.0   92536.0 -0.093957 -0.121752  0.104147   
2016-03-01      34445.0  132993.0  191615.0 -0.064372  0.119365  0.086378   
2016-11-01      37850.0   59272.0   87511.0 -0.075127 -0.547815 -0.039292   
2017-08-01     148641.0  178378.0   53446.0  0.557177  0.579365  0.035678   
2017-08-01       3985.0    3043.0   17632.0 -0.910415 -0.910816  0.000000   

                   e      i  
DATE                         
2015-12-01  0.012380  0.371  
2016-03-01 -0.071553  0.452  
2016-11-01 -0.039747  0.654  
2017-08-01  0.012762  1.194  
2017-08-01  0.000000  1.194  
```

```python
import statsmodels.formula.api as smf
results = smf.ols('e ~ N + T + S ', data=df).fit()
print results.summary()
```

```text
                            OLS Regression Results                            
==============================================================================
Dep. Variable:                      e   R-squared:                       0.064
Model:                            OLS   Adj. R-squared:                 -0.053
Method:                 Least Squares   F-statistic:                    0.5448
Date:                Sun, 27 May 2018   Prob (F-statistic):              0.656
Time:                        17:04:13   Log-Likelihood:                 32.017
No. Observations:                  28   AIC:                            -56.03
Df Residuals:                      24   BIC:                            -50.71
Df Model:                           3                                         
Covariance Type:            nonrobust                                         
==============================================================================
                 coef    std err          t      P>|t|      [95.0% Conf. Int.]
------------------------------------------------------------------------------
Intercept     -0.0019      0.017     -0.117      0.908        -0.036     0.032
N             -0.0005      0.011     -0.047      0.963        -0.022     0.021
T             -0.1804      0.188     -0.960      0.347        -0.568     0.208
S              0.0034      0.007      0.463      0.648        -0.012     0.019
==============================================================================
Omnibus:                        6.159   Durbin-Watson:                   1.842
Prob(Omnibus):                  0.046   Jarque-Bera (JB):                5.391
Skew:                           0.496   Prob(JB):                       0.0675
Kurtosis:                       4.907   Cond. No.                         42.9
==============================================================================

Warnings:
[1] Standard Errors assume that the covariance matrix of the errors is correctly specified.
```

```python
import statsmodels.formula.api as smf
results = smf.ols('S ~ e + i', data=df).fit()
print results.summary()
```

```text
                            OLS Regression Results                            
==============================================================================
Dep. Variable:                      S   R-squared:                       0.241
Model:                            OLS   Adj. R-squared:                  0.180
Method:                 Least Squares   F-statistic:                     3.964
Date:                Sun, 27 May 2018   Prob (F-statistic):             0.0320
Time:                        17:04:16   Log-Likelihood:                -67.122
No. Observations:                  28   AIC:                             140.2
Df Residuals:                      25   BIC:                             144.2
Df Model:                           2                                         
Covariance Type:            nonrobust                                         
==============================================================================
                 coef    std err          t      P>|t|      [95.0% Conf. Int.]
------------------------------------------------------------------------------
Intercept     -0.3475      0.687     -0.506      0.617        -1.763     1.068
e              5.2186      6.683      0.781      0.442        -8.545    18.982
i              0.8265      0.310      2.663      0.013         0.187     1.466
==============================================================================
Omnibus:                       40.823   Durbin-Watson:                   2.685
Prob(Omnibus):                  0.000   Jarque-Bera (JB):              176.125
Skew:                           2.708   Prob(JB):                     5.69e-39
Kurtosis:                      14.028   Cond. No.                         29.1
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
Dep. Variable:                      N   R-squared:                       0.207
Model:                            OLS   Adj. R-squared:                  0.144
Method:                 Least Squares   F-statistic:                     3.265
Date:                Sun, 27 May 2018   Prob (F-statistic):             0.0550
Time:                        17:04:21   Log-Likelihood:                -57.218
No. Observations:                  28   AIC:                             120.4
Df Residuals:                      25   BIC:                             124.4
Df Model:                           2                                         
Covariance Type:            nonrobust                                         
==============================================================================
                 coef    std err          t      P>|t|      [95.0% Conf. Int.]
------------------------------------------------------------------------------
Intercept     -0.2429      0.482     -0.503      0.619        -1.236     0.751
e              1.3706      4.692      0.292      0.773        -8.292    11.033
i              0.5492      0.218      2.521      0.018         0.101     0.998
==============================================================================
Omnibus:                       30.023   Durbin-Watson:                   2.934
Prob(Omnibus):                  0.000   Jarque-Bera (JB):               81.957
Skew:                           2.035   Prob(JB):                     1.60e-18
Kurtosis:                      10.327   Cond. No.                         29.1
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
ssr based F test:         F=5.6027  , p=0.0267  , df_denom=23, df_num=1
ssr based chi2 test:   chi2=6.3335  , p=0.0118  , df=1
likelihood ratio test: chi2=5.6682  , p=0.0173  , df=1
parameter F test:         F=5.6027  , p=0.0267  , df_denom=23, df_num=1

Granger Causality
('number of lags (no zero)', 2)
ssr based F test:         F=4.3630  , p=0.0268  , df_denom=20, df_num=2
ssr based chi2 test:   chi2=10.9075 , p=0.0043  , df=2
likelihood ratio test: chi2=9.0518  , p=0.0108  , df=2
parameter F test:         F=4.3630  , p=0.0268  , df_denom=20, df_num=2

Granger Causality
('number of lags (no zero)', 1)
ssr based F test:         F=0.4742  , p=0.4980  , df_denom=23, df_num=1
ssr based chi2 test:   chi2=0.5360  , p=0.4641  , df=1
likelihood ratio test: chi2=0.5306  , p=0.4664  , df=1
parameter F test:         F=0.4742  , p=0.4980  , df_denom=23, df_num=1

Granger Causality
('number of lags (no zero)', 2)
ssr based F test:         F=2.2711  , p=0.1292  , df_denom=20, df_num=2
ssr based chi2 test:   chi2=5.6777  , p=0.0585  , df=2
likelihood ratio test: chi2=5.1165  , p=0.0774  , df=2
parameter F test:         F=2.2711  , p=0.1292  , df_denom=20, df_num=2
```

```python
import pandas as pd
df3 = pd.read_csv('bojliq.csv',sep='\s*')
df3['DATE'] = df3.apply(lambda x: pd.to_datetime("%d-%02d-01" % (x.year,x.mon)), axis=1)
df3 = df3.set_index('DATE')

import pandas as pd
df = pd.read_csv('curr.csv',parse_dates=['DATE'])
df = df.dropna(axis=0)
df = df.set_index('DATE')
df['jp'] = (df.DDDI06JPA156NWDB * df.JPNNGDP)/100.0
df['jp'] = (df['jp'] - df['jp'].mean()) / df['jp'].std()
df['liqus'] = (df['liqus'] - df['liqus'].mean()) / df['liqus'].std()
df['exjpus'] = (df['exjpus'] - df['exjpus'].mean()) / df['exjpus'].std()
df['jpliq'] = df2.liq
df = df.dropna(axis=0)
df['jpliq'] = (df['jpliq'] - df['jpliq'].mean()) / df['jpliq'].std()
df['liqjpus'] = df.jpliq - df.liqus

import statsmodels.tsa.stattools as t
res = t.grangercausalitytests(df[['liqjpus','exjpus']],maxlag=1)
res = t.grangercausalitytests(df[['exjpus','liqjpus']],maxlag=1)
```

```text

Granger Causality
('number of lags (no zero)', 1)
ssr based F test:         F=0.7643  , p=0.4742  , df_denom=2, df_num=1
ssr based chi2 test:   chi2=1.9107  , p=0.1669  , df=1
likelihood ratio test: chi2=1.6182  , p=0.2033  , df=1
parameter F test:         F=0.7643  , p=0.4742  , df_denom=2, df_num=1

Granger Causality
('number of lags (no zero)', 1)
ssr based F test:         F=2.7863  , p=0.2370  , df_denom=2, df_num=1
ssr based chi2 test:   chi2=6.9658  , p=0.0083  , df=1
likelihood ratio test: chi2=4.3631  , p=0.0367  , df=1
parameter F test:         F=2.7863  , p=0.2370  , df_denom=2, df_num=1
```





















