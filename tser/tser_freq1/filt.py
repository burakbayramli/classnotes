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

def sinc_filter_high(order, fc1, fs):
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
    B = B / np.sum(B)
    B = -B
    B[(M/2)] = B[(M/2)] + 1
    return B
    






