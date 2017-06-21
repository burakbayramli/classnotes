

```python
import pandas as pd, health
import scipy.linalg as lin

def proj(u, n):
    return u - (np.dot(u,n) / np.dot(n,n)) * n

#dir = './data/pots1/'
dir = './data/pots2/'

fr=100; to=250

dfmag = pd.read_csv(dir + 'mags.txt',header=None,sep='\s+')
dfmag = np.array(dfmag)
dfmag = dfmag[fr:to,1:].mean(axis=0)
#dfmag = dfmag / lin.norm(dfmag)
print dfmag

dfg = pd.read_csv(dir + 'grav.txt',header=None,sep='\s+')
dfg = np.array(dfg)
dfg = dfg[fr:to,1:].mean(axis=0)
print dfg

pmag = proj(dfmag, dfg)
print pmag
```

```text
[  9.53088322  15.47789535 -28.83572706]
[ 1.04595925  2.87064062  9.30002479]
[ 11.86427484  21.88190082  -8.08864742]
```

```python
dfacc = pd.read_csv(dir + 'lacc.txt',header=None,sep='\s+')
dfacc = np.array(dfacc)[fr:to]
t = dfacc[:,0] / 1e9

ax = dfacc[:,1]
ay = dfacc[:,2]
az = dfacc[:,3]
sample_rate = 25.0
#cutoff = np.max([1, sample_rate/10.])
cutoff = 5.0
thres = 0.1
sf = 1.0/8.0
wdir = health.walk_direction_preheel(ax, ay, az, t, sample_rate=sample_rate,\
                                     stride_fraction=sf, threshold=thres,\
                                     order=4, cutoff=cutoff)
print wdir
pwdir = proj(wdir, dfg)
pwdir = pwdir / lin.norm(pwdir)
print pwdir
```

```text
nc 0.4
[ 0.68434307  0.42715438  0.59094306]
[ 0.92772476  0.31429804 -0.2013542 ]
```

```python
a = np.arccos(np.dot(pwdir, pmag) / (lin.norm(pwdir)*lin.norm(pmag)))
print np.rad2deg(a)
```

```text
41.7938893818
```
































