
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
df['e'] = df.jpyus
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
2015-12-01  121.6350  0.371  
2016-03-01  112.9317  0.452  
2016-11-01  108.4430  0.654  
2017-08-01  109.8270  1.194  
2017-08-01  109.8270  1.194  
```

```python
import statsmodels.formula.api as smf
results = smf.ols('e ~ N + T + S ', data=df).fit()
print results.summary()
```

```text
                            OLS Regression Results                            
==============================================================================
Dep. Variable:                      e   R-squared:                       0.087
Model:                            OLS   Adj. R-squared:                 -0.027
Method:                 Least Squares   F-statistic:                    0.7606
Date:                Sun, 27 May 2018   Prob (F-statistic):              0.527
Time:                        17:39:37   Log-Likelihood:                -111.17
No. Observations:                  28   AIC:                             230.3
Df Residuals:                      24   BIC:                             235.7
Df Model:                           3                                         
Covariance Type:            nonrobust                                         
==============================================================================
                 coef    std err          t      P>|t|      [95.0% Conf. Int.]
------------------------------------------------------------------------------
Intercept    103.9373      2.747     37.842      0.000        98.269   109.606
N              0.7320      1.769      0.414      0.683        -2.918     4.382
T            -12.5409     31.268     -0.401      0.692       -77.076    51.994
S              0.8006      1.236      0.648      0.523        -1.751     3.352
==============================================================================
Omnibus:                        2.464   Durbin-Watson:                   0.414
Prob(Omnibus):                  0.292   Jarque-Bera (JB):                2.095
Skew:                          -0.564   Prob(JB):                        0.351
Kurtosis:                       2.276   Cond. No.                         42.9
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
Dep. Variable:                      S   R-squared:                       0.222
Model:                            OLS   Adj. R-squared:                  0.160
Method:                 Least Squares   F-statistic:                     3.573
Date:                Sun, 27 May 2018   Prob (F-statistic):             0.0432
Time:                        17:39:44   Log-Likelihood:                -67.459
No. Observations:                  28   AIC:                             140.9
Df Residuals:                      25   BIC:                             144.9
Df Model:                           2                                         
Covariance Type:            nonrobust                                         
==============================================================================
                 coef    std err          t      P>|t|      [95.0% Conf. Int.]
------------------------------------------------------------------------------
Intercept     -0.5554      4.879     -0.114      0.910       -10.604     9.493
e              0.0019      0.049      0.038      0.970        -0.099     0.103
i              0.8301      0.383      2.166      0.040         0.041     1.619
==============================================================================
Omnibus:                       41.315   Durbin-Watson:                   2.763
Prob(Omnibus):                  0.000   Jarque-Bera (JB):              181.475
Skew:                           2.743   Prob(JB):                     3.92e-40
Kurtosis:                      14.201   Cond. No.                         959.
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
Dep. Variable:                      N   R-squared:                       0.205
Model:                            OLS   Adj. R-squared:                  0.141
Method:                 Least Squares   F-statistic:                     3.224
Date:                Sun, 27 May 2018   Prob (F-statistic):             0.0568
Time:                        17:39:50   Log-Likelihood:                -57.254
No. Observations:                  28   AIC:                             120.5
Df Residuals:                      25   BIC:                             124.5
Df Model:                           2                                         
Covariance Type:            nonrobust                                         
==============================================================================
                 coef    std err          t      P>|t|      [95.0% Conf. Int.]
------------------------------------------------------------------------------
Intercept      0.2298      3.389      0.068      0.946        -6.750     7.209
e             -0.0049      0.034     -0.143      0.888        -0.075     0.065
i              0.5742      0.266      2.157      0.041         0.026     1.122
==============================================================================
Omnibus:                       29.083   Durbin-Watson:                   2.955
Prob(Omnibus):                  0.000   Jarque-Bera (JB):               75.370
Skew:                           1.988   Prob(JB):                     4.30e-17
Kurtosis:                       9.985   Cond. No.                         959.
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
ssr based F test:         F=0.0890  , p=0.7680  , df_denom=24, df_num=1
ssr based chi2 test:   chi2=0.1001  , p=0.7517  , df=1
likelihood ratio test: chi2=0.0999  , p=0.7519  , df=1
parameter F test:         F=0.0890  , p=0.7680  , df_denom=24, df_num=1

Granger Causality
('number of lags (no zero)', 2)
ssr based F test:         F=1.6867  , p=0.2093  , df_denom=21, df_num=2
ssr based chi2 test:   chi2=4.1766  , p=0.1239  , df=2
likelihood ratio test: chi2=3.8733  , p=0.1442  , df=2
parameter F test:         F=1.6867  , p=0.2093  , df_denom=21, df_num=2

Granger Causality
('number of lags (no zero)', 1)
ssr based F test:         F=0.0301  , p=0.8638  , df_denom=24, df_num=1
ssr based chi2 test:   chi2=0.0338  , p=0.8541  , df=1
likelihood ratio test: chi2=0.0338  , p=0.8542  , df=1
parameter F test:         F=0.0301  , p=0.8638  , df_denom=24, df_num=1

Granger Causality
('number of lags (no zero)', 2)
ssr based F test:         F=0.0372  , p=0.9636  , df_denom=21, df_num=2
ssr based chi2 test:   chi2=0.0921  , p=0.9550  , df=2
likelihood ratio test: chi2=0.0919  , p=0.9551  , df=2
parameter F test:         F=0.0372  , p=0.9636  , df_denom=21, df_num=2
```

```python
import pandas as pd
df3 = pd.read_csv('bojliq.csv',sep='\s*')
df3['DATE'] = df3.apply(lambda x: pd.to_datetime("%d-%02d-01" % (x.year,x.mon)), axis=1)
df3 = df3.set_index('DATE')

import pandas as pd
df4 = pd.read_csv('curr.csv',parse_dates=['DATE'])
df4 = df4.dropna(axis=0)
df4 = df4.set_index('DATE')
df4['jp'] = (df4.DDDI06JPA156NWDB * df4.JPNNGDP)/100.0
df4['jp'] = (df4['jp'] - df4['jp'].mean()) / df4['jp'].std()
df4['liqus'] = (df4['liqus'] - df4['liqus'].mean()) / df4['liqus'].std()
df4['exjpus'] = (df4['exjpus'] - df4['exjpus'].mean()) / df4['exjpus'].std()
df4['jpliq'] = df3.liq
df4 = df4.dropna(axis=0)
df4['jpliq'] = (df4['jpliq'] - df4['jpliq'].mean()) / df4['jpliq'].std()
df4['liqjpus'] = df4.jpliq - df4.liqus

import statsmodels.tsa.stattools as t
res = t.grangercausalitytests(df4[['liqjpus','exjpus']],maxlag=1)
res = t.grangercausalitytests(df4[['exjpus','liqjpus']],maxlag=1)
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





















