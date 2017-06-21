# https://github.com/Sage-Bionetworks/mhealthx baz alinmistir
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from scipy.fftpack import rfft, fftfreq
from scipy.signal import butter, lfilter

def crossings_nonzero_pos2neg(data):
    pos = data > 0
    crossings = (pos[:-1] & ~pos[1:]).nonzero()[0]
    return crossings

def butter_lowpass_filter(data, sample_rate, cutoff=10, order=4):
    nyquist = 0.5 * sample_rate
    normal_cutoff = cutoff / nyquist
    print 'nc', normal_cutoff
    b, a = butter(order, normal_cutoff, btype='low', analog=False)
    y = lfilter(b, a, data)
    return y

def compute_interpeak(data, sample_rate):

    # Real part of FFT:
    freqs = fftfreq(data.size, d=1.0/sample_rate)
    f_signal = rfft(data)

    # Maximum non-zero frequency:
    imax_freq = np.argsort(f_signal)[-2]
    freq = np.abs(freqs[imax_freq])

    # Inter-peak samples:
    interpeak = np.int(np.round(sample_rate / freq))

    return interpeak

def heel_strikes(data, sample_rate, threshold=0.2, order=4, cutoff=5,t=None):

    # Demean data (not in iGAIT):
    data -= np.mean(data)

    # Low-pass filter the AP accelerometer data by the 4th order zero lag
    # Butterworth filter whose cut frequency is set to 5 Hz:
    filtered = butter_lowpass_filter(data, sample_rate, cutoff, order)

    # Find transitional positions where AP accelerometer changes from
    # positive to negative.
    transitions = crossings_nonzero_pos2neg(filtered)

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
    interpeak = compute_interpeak(data, sample_rate)
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

    return strikes, strike_indices


def walk_direction_preheel(ax, ay, az, t, sample_rate, 
                           stride_fraction=1.0/8.0, threshold=0.5,
                           order=4, cutoff=5):


    # Sum of absolute values across accelerometer axes:
    data = np.abs(ax) + np.abs(ay) + np.abs(az)

    # Find maximum peaks of smoothed data:
    dummy, ipeaks_smooth = heel_strikes(data, sample_rate,
                                        threshold,
                                        order, cutoff, t)

    # Compute number of samples between peaks using the real part of the FFT:
    interpeak = compute_interpeak(data, sample_rate)
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

    return direction

if __name__ == "__main__": 
 
    import pandas as pd
    #dir = './data/pots1/'
    #dir = './data/pots2/'
    dir = './data/pots3/'
    dfacc = pd.read_csv(dir + 'lacc.txt',header=None,sep='\s+')
    dfacc = np.array(dfacc)[50:300,]
    #dfacc = np.array(dfacc)
    t = dfacc[:,0] / 1e9
    ax = dfacc[:,1]
    ay = dfacc[:,2]
    az = dfacc[:,3]
    sample_rate = 25.0
    #cutoff = np.max([1, sample_rate/10.])
    cutoff = 5.0
    thres = 0.1
    sf = 1.0/8.0
    res = walk_direction_preheel(ax, ay, az, t, sample_rate=sample_rate,\
                                 stride_fraction=sf, threshold=thres,\
                                 order=4, cutoff=cutoff)
    print res



