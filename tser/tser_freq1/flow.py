import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

def hamm(window_size):
    N = window_size;
    output = np.zeros((N, 1));
    if np.mod(N, 2) == 0 :
        m = np.fix(N / 2)
        n = m
    else:
        m = np.fix(N / 2)+1; 
        n = m-1; 
    window = 0.54 - 0.46 * np.cos(2*np.pi*(np.arange(m)) / (N-1))
    tmp1 = window[:int(m)]
    tmp2 = window[np.arange(int(n)-1,-1,-1)]
    return np.hstack((tmp1,tmp2))
    
def sinc_filter_low(order, fc1, fs):
    Fc1 = fc1 / np.float(fs) 
    M  = order
    B = np.zeros((M+1, 1))
    window = hamm(M+1)
    for i in range(M+1):
        if 2 * i == M:
            B[i] = 2*np.pi*Fc1
        else:
            tmp1 = 2*np.pi*Fc1 *(i-(M/2.))
            tmp2 = (i-(M/2.))
            B[i] = np.sin(tmp1) / tmp2
        B[i] = B[i] * window[i]
    return B / np.sum(B)

fy=300; #signal frequency in Hz
wy=2*np.pi*fy; #signal frequency in rad/s
fs=50; #sampling frequency in Hz
tiv=1./fs; #time interval between samples;
tend = 5 # seconds
t=np.linspace(0,tend,tend/tiv); #time intervals set (5 seconds)

y=0.6*np.sin(wy*t)+0.3*np.sin(3*wy*t)+0.2*np.sin(5*wy*t); #signal data set
f=plt.figure()
plt.plot(t,y)
plt.savefig('out0.png')

fou = np.fft.fft(y, fs)
hmag=np.real(fou); ah=hmag/len(t);
f=plt.figure()
plt.stem(range(10),ah[:10])
plt.savefig('out1.png')

order = 32
fc1 = 4
fs = 20
f1 = sinc_filter_low(order, fc1, fs);











