

```python
import pandas as pd, health
import scipy.linalg as lin

dir = './data/pots1/'
dfacc = pd.read_csv(dir + 'lacc.txt',header=None,sep='\s+')
fr=100; to=250
dfacc = np.array(dfacc)[fr:to]
t = dfacc[:,0] / 1e9
ax = dfacc[:,1]
ay = dfacc[:,2]
az = dfacc[:,3]
data = np.abs(ax) + np.abs(ay) + np.abs(az)

sample_rate = 25.0
cutoff = 5.0
thres = 0.1
order=4
dummy, ipeaks_smooth = health.heel_strikes(data, sample_rate,thres,order,cutoff,t)
print ipeaks_smooth
```

```text
nc 0.4
[64, 64, 64, 93, 107, 107, 107, 107, 107, 107, 107, 107, 107, 107, 121]
```














#




















#