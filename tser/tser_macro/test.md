
```python
import statsmodels.tsa.stattools as t
import pandas as pd

df = pd.read_csv('rates.csv',parse_dates=['DATE'])
df = df.set_index('DATE')
df = df.resample('AS').last()
df['gdpinc'] = df.gdp.pct_change()*100.0
df = df.dropna(axis=0)
res = t.grangercausalitytests(df[['gdpinc','shortrate']],maxlag=2)
res = t.grangercausalitytests(df[['shortrate','gdpinc']],maxlag=2)
```

```text

Granger Causality
('number of lags (no zero)', 1)
ssr based F test:         F=0.0062  , p=0.9378  , df_denom=43, df_num=1
ssr based chi2 test:   chi2=0.0066  , p=0.9353  , df=1
likelihood ratio test: chi2=0.0066  , p=0.9353  , df=1
parameter F test:         F=0.0062  , p=0.9378  , df_denom=43, df_num=1

Granger Causality
('number of lags (no zero)', 2)
ssr based F test:         F=0.2645  , p=0.7689  , df_denom=40, df_num=2
ssr based chi2 test:   chi2=0.5952  , p=0.7426  , df=2
likelihood ratio test: chi2=0.5913  , p=0.7440  , df=2
parameter F test:         F=0.2645  , p=0.7689  , df_denom=40, df_num=2

Granger Causality
('number of lags (no zero)', 1)
ssr based F test:         F=14.0199 , p=0.0005  , df_denom=43, df_num=1
ssr based chi2 test:   chi2=14.9980 , p=0.0001  , df=1
likelihood ratio test: chi2=12.9812 , p=0.0003  , df=1
parameter F test:         F=14.0199 , p=0.0005  , df_denom=43, df_num=1

Granger Causality
('number of lags (no zero)', 2)
ssr based F test:         F=9.2853  , p=0.0005  , df_denom=40, df_num=2
ssr based chi2 test:   chi2=20.8919 , p=0.0000  , df=2
likelihood ratio test: chi2=17.1609 , p=0.0002  , df=2
parameter F test:         F=9.2853  , p=0.0005  , df_denom=40, df_num=2
```

```python
import scipy.stats as stats
df['gdpinc1'] = df.gdpinc.shift(1)
df2 = df.dropna(axis=0)
print stats.pearsonr(df2.shortrate, df2.gdpinc1)
```

```text
(0.75710287451587455, 1.1393168782627738e-09)
```








