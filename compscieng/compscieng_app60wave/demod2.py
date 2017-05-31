# Import the plotting library
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import scipy.signal
import numpy as np

# Define the setup
fCarrier = 10;
fAudio = 1;
fs = 1000;
timeEnd = 1;
time = np.linspace(0,2,fs*timeEnd);

# Create the signals
carrier = np.sin(2*np.pi*fCarrier*time);
plt.figure()
plt.plot(carrier)
plt.savefig('out0.png')
audio = np.sin(2*np.pi*fAudio*time);
plt.figure()
plt.plot(audio)
plt.savefig('out1.png')
audioInt = -np.cos(2*np.pi*fAudio*time);
freqMod = np.sin(2*np.pi*fCarrier*time + 2*np.pi*1*audioInt);
plt.figure()
plt.plot(freqMod)
plt.savefig('out2.png')

# Downconvert
analyticSignal = scipy.signal.hilbert(freqMod); # wikipedia analytic signal
baseband = analyticSignal * np.exp(-2*np.pi*fCarrier*time*1j); # complex mixing
audioDemod = np.angle( baseband[1::1] * np.conjugate(baseband[0:-1:1]) ); # fm demod
plt.figure()
plt.plot(audioDemod)
plt.savefig('out3.png')
