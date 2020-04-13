
principles gnss pg 126


```python
import pandas as pd

df = pd.read_csv('androsensor.csv',sep=';')
print (df.ix[:,[0,-1]].tail(3))
```

```text
    ACCELEROMETER X (m/s²)  YYYY-MO-DD HH-MI-SS_SSS
20                  0.8428  2019-08-04 23:36:59:100
21                  0.0144  2019-08-04 23:37:00:099
22                  0.1197  2019-08-04 23:37:01:100
```





