

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

data2 = data - np.mean(data)
filtered = health.butter_lowpass_filter(data2, sample_rate, cutoff, order)
transitions = health.crossings_nonzero_pos2neg(filtered)
print transitions
f=plt.figure()
#plt.plot(data2)
plt.plot(filtered)
plt.plot(transitions,filtered[transitions],'rd')
plt.savefig('out2.png')
```

```text
nc 0.4
[ 36  43  52  58  68  74  83  90  97 103 111 119 125 131 140 147]
```







```python
dummy, peaks = health.heel_strikes(data, sample_rate,thres,order,cutoff,t)
print peaks

plt.plot(data)
plt.plot(peaks,data[peaks],'rd')
plt.savefig('out1.png')
```


```text
nc 0.4
[64, 64, 64, 93, 107, 107, 107, 107, 107, 107, 107, 107, 107, 107, 121]
```

```python
interpeak = health.compute_interpeak(data, sample_rate)
print interpeak
```

```text
75
```

```python
print (6346075438000-6343028974000) / 1e9
```

```text
3.046464
```








#




















#