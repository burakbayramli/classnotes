

```python
import pandas as pd
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
threshold = 0.1
order=4
stride_fraction = 1.0/8.0
```

```python
# Sum of absolute values across accelerometer axes:
data = np.abs(ax) + np.abs(ay) + np.abs(az)

# Find maximum peaks of smoothed data:
plot_test2 = False


# Demean data (not in iGAIT):
data -= np.mean(data)

# Low-pass filter the AP accelerometer data by the 4th order zero lag
# Butterworth filter whose cut frequency is set to 5 Hz:
filtered = health.butter_lowpass_filter(data, sample_rate, cutoff, order)

# Find transitional positions where AP accelerometer changes from
# positive to negative.
transitions = health.crossings_nonzero_pos2neg(filtered)

f=plt.figure()
plt.plot(filtered)
plt.plot(transitions,filtered[transitions],'rd')
plt.savefig('out1.png')
```

```text
nc 0.4
```

```python
# Find the peaks of AP acceleration preceding the transitional positions,
# and greater than the product of a threshold and the maximum value of
# the AP acceleration:
strike_indices_smooth = []
filter_threshold = np.abs(threshold * np.max(filtered))
for i in range(1, np.size(transitions)):
    segment = range(transitions[i-1], transitions[i])
    imax = np.argmax(filtered[segment])
    if filtered[segment[imax]] > filter_threshold:
        strike_indices_smooth.append(segment[imax])

# Compute number of samples between peaks using the real part of the FFT:
interpeak = health.compute_interpeak(data, sample_rate)
decel = np.int(interpeak / 2)

# Find maximum peaks close to maximum peaks of smoothed data:
strike_indices = []
for ismooth in strike_indices_smooth:
    istrike = np.argmax(data[ismooth - decel:ismooth + decel])
    istrike = istrike + ismooth - decel
    strike_indices.append(istrike)

strikes = np.asarray(strike_indices)
strikes -= strikes[0]
strikes = strikes / sample_rate

ipeaks_smooth = strike_indices

f=plt.figure()
plt.plot(data)
plt.plot(ipeaks_smooth,data[ipeaks_smooth],'rd')
plt.savefig('out2.png')
```

```text
nc 0.4
```

```python
# Compute number of samples between peaks using the real part of the FFT:
interpeak = health.compute_interpeak(data, sample_rate)
#print interpeak
#print 'sf', stride_fraction, stride_fraction*interpeak
decel = np.int(np.round(stride_fraction * interpeak))

# Find maximum peaks close to maximum peaks of smoothed data:
ipeaks = []
for ipeak_smooth in ipeaks_smooth:
    #print decel, ipeak_smooth-decel, ipeak_smooth + decel
    #print data[ipeak_smooth - decel:ipeak_smooth + decel]
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

print direction
```

```text
[ 0.24212494 -0.25265038  0.93677281]
```














