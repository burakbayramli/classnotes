import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
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

strike_indices_smooth = []
filter_threshold = np.abs(thres * np.max(filtered))
for i in range(1, np.size(transitions)):
    segment = range(transitions[i-1], transitions[i])
    imax = np.argmax(filtered[segment])
    if filtered[segment[imax]] >	    filter_threshold:
        strike_indices_smooth.append(segment[imax])
	
print strike_indices_smooth

ipeaks_smooth = strike_indices_smooth
interpeak = health.compute_interpeak(data, sample_rate)
decel = np.int(np.round(stride_fraction * interpeak))

print interpeak
print decel

















