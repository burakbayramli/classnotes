
```python
import pandas as pd
df = pd.read_csv('tr_account.csv',sep='\s+',index_col=0).T
df = df.astype(float) / 1000.0
df['All_Accounts'] = df["CURRENT_ACCOUNT"] + df["CAPITAL_ACCOUNT"] + df["FINANCIAL_ACCOUNT"] + df["RESERVE_ASSETS"]+ df["NET_ERRORS_AND_OMISSIONS"] 
print df["All_Accounts"]
df.to_csv('/tmp/out.csv',sep=';')

```

```text
-0.925
0.0
-4.829
5.206
1.302
1999      0.754
2000    -25.162
2001      3.266
2002     -2.768
2003     -6.130
2004    -26.720
2005    -38.970
2006    -64.128
2007    -74.544
2008    -75.040
2009    -18.174
2010    -90.262
2011   -132.264
2012    -99.690
2013   -125.394
2014    -85.190
2015    -44.748
2016    -44.290
2017    -92.866
Name: All_Accounts, dtype: float64
```
































