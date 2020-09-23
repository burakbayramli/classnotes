# Program to demodulate a FM wave!
# Ajinkya Kadam | N14265382 | adk427

##Import Libraries 
import numpy as np
import matplotlib.pyplot as plt
import scipy.signal as signal

## Import data from file
dir = "/home/burak/Documents/Dropbox/Public/data"
extract_data = np.fromfile(dir + "/fm1.dat",dtype="uint8")
interleavedData = extract_data[0::2] + 1j*extract_data[1::2]

#Plot SpectoGram for the complex data 
#plt.specgram(interleavedData, NFFT =1024, Fs=1140000)
#plt.title("SpectoGram of 'signal' loaded from file")
#plt.xlabel("Time")
#plt.ylabel("Frequency")
#plt.figure(1)
#plt.show()

#plt.psd(interleavedData, NFFT=1024, Fs=1140000)
#plt.title("PSD of interleaved Data")
#plt.figure()
#plt.show()
#plt.scatter(np.real(interleavedData[0:1140000]), np.imag(interleavedData[0:1140000]))
#plt.title("Constellation of the 'signal' loaded from file")
#plt.show()

##Normalizing Range to [-1 to 1] 
calculate_range = max(interleavedData) - min(interleavedData);
data = (interleavedData - min(interleavedData))/ calculate_range
x1 = (data*2) - 1

#Plot SpectoGram for the normalized signal
#plt.specgram(x1, NFFT =1024, Fs=1140000)
#plt.title("SpectoGram of signal post normalization")
#plt.xlabel("Time")
#plt.ylabel("Frequency")
#plt.figure(1)
#plt.show()

#plt.psd(x1, NFFT=1024, Fs=1140000)
#plt.title("PSD of normalized signal")
#plt.show()

#plt.scatter(np.real(x1[0:1140000]), np.imag(x1[0:1140000]))
#plt.title("Constellation of the 'signal' loaded from file")
#plt.show()

## Mixer Processing
Fs = 1140000
fc = np.exp(-1.0j*2.0*np.pi* 250000/Fs*np.arange(len(x1)))
x2 = x1*fc


#plt.psd(x1, NFFT=1024, Fs=1140000, color = "blue") 
#plt.psd(x2, NFFT=1024, Fs=1140000, color="green")
#plt.title("PSD of shifted baseband signal ")
#plt.show()

## Filter Operations
f_bw=200000
Fs=1140000
n_taps=64
lpf= signal.remez(n_taps, [0, f_bw, f_bw +(Fs/2-f_bw)/4,Fs/2], [1,0], Hz=Fs)
w,h = signal.freqz(lpf)
#plt.plot(w, 20*np.log10(abs(h)))
#plt.xscale('log')
#plt.title('Filter Frequency Response')
#plt.xlabel('Frequency')
#plt.ylabel('Amplitude')
#plt.margins(0,1)
#plt.grid(which='both',axis='both')
#plt.show()

x3 = signal.lfilter(lpf, 1.0, x2)
#plt.psd(x2, NFFT=1024, Fs=1140000, color="blue")  # original
#plt.psd(x3, NFFT=1024, Fs=1140000, color="green")  # filtered
#plt.title("PSD of output signal from LPF Vs Original Signal")
#plt.show()

## Decimate the Signal
dec_rate = int(Fs/f_bw)
x4 = signal.decimate(x3, dec_rate)
Fs_x4 = Fs/dec_rate

#plt.psd(x4, NFFT=1024, Fs=Fs_x4, color="blue")
#plt.title("PSD of deimated signal")
#plt.show() 

## Frequency Discriminator
y = x4[1:] * np.conj(x4[:-1])
x5 = np.angle(y)

#plt.psd(x5, NFFT=1024, Fs=Fs_x4, color="blue")
#plt.title("PSD of Post Frequency Discrimination")
#plt.show() 

## De Emphasis Filter
d = Fs_x4 * 75e-6   # Calculate the # of samples to hit the -3dB point
r = np.exp(-1/d)   # Calculate the decay between each sample
b = [1-r]          # Create the filter coefficients
a = [1,-r]
x6 = signal.lfilter(b,a,x5)
#plt.psd(x6, NFFT=1024, Fs=Fs_x4, color="blue")
#plt.title("PSD of signal Post DeEmphasis")
#plt.show() 

## Decimating Output of De Emphasis Filter
dec_rate = int(Fs/f_bw)
x7=signal.decimate(x6,dec_rate)

#plt.psd(x7, NFFT=1024, Fs=Fs_x4)
#plt.title("PSD of Decimated Signal Post DeEmphasis")
#plt.show() 

## Write Data To AudioFile
x7*= 10000 / np.max(np.abs(x7))               # scale so it's audible
x7.astype("int16").tofile("radio.raw")
# aplay radio.raw -r 100000.0 -f S16_LE -t raw -c 1
# aplay radio.raw -r 45600 -f S16_LE -t raw -c 1
