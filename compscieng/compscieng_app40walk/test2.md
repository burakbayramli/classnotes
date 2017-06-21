
Re: heel strikes (from Yang, et al., 2012): "The heel contacts are
detected by peaks preceding the sign change of AP acceleration [3]. In
order to automatically detect a heel contact event, firstly, the AP
acceleration is low pass filtered by the 4th order zero lag
Butterworth filter whose cut frequency is set to 5 Hz.  After that,
transitional positions where AP acceleration changes from positive to
negative can be identified. Finally the peaks of AP acceleration
preceding the transitional positions, and greater than the product of
a threshold and the maximum value of the AP acceleration are denoted
as heel contact events...  This threshold is defined as the ratio to
the maximum value of the AP acceleration, for example 0.5 indicates
the threshold is set at 50% of the maximum AP acceleration. Its
default value is set to 0.4 as determined experimentally in this
paper, where this value allowed correct detection of all gait events
in control subjects. However, when a more irregular pattern is
analysed, the threshold should be less than 0.4. The user can test
different threshold values and find the best one according to the gait
event detection results."

This program derives the local walk direction vector from the end of
the primary leg's stride, when it is decelerating in its swing.  While
the WalkCompass relies on clear heel strike signals across the
accelerometer axes, this program just uses the most prominent strikes,
and estimates period from the real part of the FFT of the data.



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


# Demean data
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


# Find the peaks of AP acceleration preceding the transitional
# positions, and greater than the product of a threshold and the
# maximum value of the AP acceleration:

```python
strike_indices_smooth = []
filter_threshold = np.abs(threshold * np.max(filtered))
for i in range(1, np.size(transitions)):
    segment = range(transitions[i-1], transitions[i])
    imax = np.argmax(filtered[segment])
    if filtered[segment[imax]] > filter_threshold:
        strike_indices_smooth.append(segment[imax])

f=plt.figure()
plt.plot(data)
plt.plot(strike_indices_smooth,data[strike_indices_smooth],'rd')
plt.savefig('out3.png')

# Compute number of samples between peaks using the real part of the FFT:
interpeak = health.compute_interpeak(data, sample_rate)
decel = np.int(interpeak / 2)
print 'decel', decel

# Find maximum peaks close to maximum peaks of smoothed data:
strike_indices = []
for ismooth in strike_indices_smooth:
    istrike = np.argmax(data[ismooth - decel:ismooth + decel])
    istrike = istrike + ismooth - decel
    strike_indices.append(istrike)

strikes = np.asarray(strike_indices)
strikes -= strikes[0]
strikes = strikes / sample_rate

f=plt.figure()
plt.plot(data)
plt.plot(strike_indices,data[strike_indices],'rd')
plt.savefig('out2.png')
```

```text
decel 37
```

```python
# Compute number of samples between peaks using the real part of the FFT:
decel = np.int(np.round(stride_fraction * interpeak))
print 'decel', decel

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

print direction
```

```text
decel 9
[ 0.24212494 -0.25265038  0.93677281]
```















