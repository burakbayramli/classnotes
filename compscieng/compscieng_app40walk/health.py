# http://github.com/sage-bionetworks/mhealthx baz alinmistir
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from scipy.fftpack import rfft, fftfreq
from scipy.signal import butter, lfilter

def crossings_nonzero_pos2neg(data):
    # sifir gecislerini burada hesapla
    pos = data > 0
    crossings = (pos[:-1] & ~pos[1:]).nonzero()[0]
    return crossings

def butter_lowpass_filter(data, sample_rate, cutoff=10, order=4):
    nyquist = 0.5 * sample_rate
    normal_cutoff = cutoff / nyquist
    b, a = butter(order, normal_cutoff, btype='low', analog=False)
    y = lfilter(b, a, data)
    return y

def compute_interpeak(data, sample_rate):
    freqs = fftfreq(data.size, d=1.0/sample_rate)
    f_signal = rfft(data)
    imax_freq = np.argsort(f_signal)[-2]
    freq = np.abs(freqs[imax_freq])
    # tepe noktalari arasindaki veri nokta sayisi
    interpeak = np.int(np.round(sample_rate / freq))

    return interpeak

