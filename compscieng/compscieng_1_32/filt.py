import scipy, matplotlib.pyplot as plt
import numpy as np

def hamming(window_size):
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
    window = hamming(M+1)
    for i in range(M+1):
        if 2 * i == M:
            B[i] = 2*np.pi*Fc1
        else:
            tmp1 = 2*np.pi*Fc1 *(i-(M/2.))
            tmp2 = (i-(M/2.))
            B[i] = np.sin(tmp1) / tmp2
        B[i] = B[i] * window[i]
    return B / np.sum(B)
 
def sinc_filter_high(order, fc1, fs):
    Fc1 = fc1 / np.float(fs) 
    M  = order
    B = np.zeros((M+1, 1))
    window = hamming(M+1)
    for i in range(M+1):
        if 2 * i == M:
            B[i] = 2*np.pi*Fc1
        else:
            tmp1 = 2*np.pi*Fc1 *(i-(M/2.))
            tmp2 = (i-(M/2.))
            B[i] = np.sin(tmp1) / tmp2
        B[i] = B[i] * window[i]
    B = B / np.sum(B)
    B = -B
    B[(M/2)] = B[(M/2)] + 1
    return B
    
def sinc_filter_band(order, fc1, fc2, fs):
    M = order
    A = sinc_filter_low(order, fc1, fs).T[0]
    B = sinc_filter_high(order, fc2, fs).T[0]
    output = A+B
    output = -output
    output[(M/2)] = output[(M/2)] + 1.
    return output

def plotSpectrum(y,Fs):
    n = len(y) # sinyal uzunlugu
    k = np.arange(n)
    T = n/Fs
    frq = k/T # frekansin her iki kismi
    frq = frq[range(n/2)] # frekansin tek tarafi
    Y = scipy.fft(y)/n # fft hesaplamak ve normalizasyon
    Y = Y[range(n/2)] 
    plt.plot(frq,np.abs(Y),'r') # spektrumu grafiklemek
    
