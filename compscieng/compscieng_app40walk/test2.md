
```python
import pandas as pd, health
import scipy.linalg as lin

dir = './data/pots1/'
dfacc = pd.read_csv(dir + 'lacc.txt',header=None,sep='\s+')
fr=120; to=250
dfacc = np.array(dfacc)[fr:to]
t = dfacc[:,0] / 1e9
ax = dfacc[:,1]
ay = dfacc[:,2]
az = dfacc[:,3]
data = np.abs(ax) + np.abs(ay) + np.abs(az)
```

Low pass filtered by the 4th order zero lag Butterworth filter whose
cut frequency is set to 5 Hz. After that, transitional positions where
AP acceleration changes from positive to negative can be identified.

```python
sample_rate = 25.0
cutoff = 5.0
thres = 0.1
order=4
stride_fraction = 1/8.

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
[ 15  22  32  38  47  54  63  69  77  83  91  99 105 111 120 126]
```

```python
strike_indices_smooth = []
filter_threshold = np.abs(thres * np.max(filtered))
for i in range(1, np.size(transitions)):
    segment = range(transitions[i-1], transitions[i])
    imax = np.argmax(filtered[segment])
    if filtered[segment[imax]] >	    filter_threshold:
        strike_indices_smooth.append(segment[imax])
	
print strike_indices_smooth
```

```text
[15, 30, 32, 46, 52, 62, 68, 75, 82, 89, 96, 103, 110, 118, 125]
```

```python
strike_indices = []
for ismooth in strike_indices_smooth:
    istrike = np.argmax(data[ismooth - decel:ismooth + decel])
    istrike = istrike + ismooth - decel
    strike_indices.append(istrike)

print strike_indices
plt.plot(filtered)
plt.plot(strike_indices,filtered[strike_indices],'rd')
plt.savefig('out3.png')
```

```text
[13, 29, 31, 44, 52, 60, 66, 73, 81, 87, 94, 101, 108, 116, 123]
```

```python
interpeak = health.compute_interpeak(data, sample_rate)
print interpeak
print stride_fraction * interpeak
decel = np.int(np.round(stride_fraction * interpeak))
print decel
# Find maximum peaks close to maximum peaks of smoothed data:
print decel
print data[13 - decel:13 + decel]
```

```text
4
0.5
0
0
[]
```


```python
interpeak = health.compute_interpeak(data, sample_rate)
decel = np.int(np.round(stride_fraction * interpeak))

# Find maximum peaks close to maximum peaks of smoothed data:
ipeaks = []
for ipeak_smooth in strike_indices:
    ipeak = np.argmax(data[ipeak_smooth - decel:ipeak_smooth + decel])
    ipeak += ipeak_smooth - decel
    ipeaks.append(ipeak)


# Compute the average vector for each deceleration phase:
vectors = []
for ipeak in ipeaks:
    decel_vectors = np.asarray([[ax[i], ay[i], az[i]]
                                for i in range(ipeak - decel, ipeak)])
    vectors.append(np.mean(decel_vectors, axis=0))

# Compute the average deceleration vector and take the opposite direction:
direction = -1 * np.mean(vectors, axis=0)

# Return the unit vector in this direction:
direction /= np.sqrt(direction.dot(direction))
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
[14, 29, 31, 46, 52, 62, 67, 75, 81, 88, 95, 102, 109, 117, 124]
```









#




















#