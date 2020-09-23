from scikits.talkbox.features import mfcc
import pyaudio, numpy as np, wave
import scipy.io.wavfile
from hmmlearn import hmm

N = 8
CHUNK = 1024 
FORMAT = pyaudio.paInt16 #paInt8
CHANNELS = 1
RATE = 44100 #sample rate
RECORD_SECONDS = 2

# Unix sistemlerinde /dev/shm altindaki tum dosyalar RAM / ucucu
# bellege alinir, yani diske yazilmis. Daha hizli islemek icin
# boyle yaptik. 
WAVE_OUTPUT_MEMORY_FILENAME = '/dev/shm/mic.wav'

sample_rate, X = scipy.io.wavfile.read('computer.wav')
ceps_c, mspec, spec = mfcc(X)
sample_rate, X = scipy.io.wavfile.read('emacs.wav')
ceps_e, mspec, spec = mfcc(X)
sample_rate, X = scipy.io.wavfile.read('nothing.wav')
ceps_n, mspec, spec = mfcc(X)

#hmm_computer = .. 
#hmm_emacs = ..
#hmm_nothing = ..

print 'DB ready..'

p = pyaudio.PyAudio()

stream = p.open(format=FORMAT,
                channels=CHANNELS,
                rate=RATE,
                input=True,
                frames_per_buffer=CHUNK) #buffer


while (True):
    frames = []
    for i in range(0, int(RATE / CHUNK * RECORD_SECONDS)):
        data = stream.read(CHUNK)
        frames.append(data) # 2 bytes(16 bits) per channel
        
    wf = wave.open(WAVE_OUTPUT_MEMORY_FILENAME, 'wb')
    wf.setnchannels(CHANNELS)
    wf.setsampwidth(p.get_sample_size(FORMAT))
    wf.setframerate(RATE)
    wf.writeframes(b''.join(frames))
    wf.close()

    sample_rate, X = scipy.io.wavfile.read(WAVE_OUTPUT_MEMORY_FILENAME)
    print len(X), type(X)
    ceps, mspec, spec = mfcc(X)

    # basit uzaklik ile
    d1 = np.sum(np.sum((ceps_c-ceps)**2))
    d2 = np.sum(np.sum((ceps_e-ceps)**2))
    d3 = np.sum(np.sum((ceps_n-ceps)**2))
    c = np.array([d1,d2,d3])
    if np.argmin(c) == 0: print 'computer'
    if np.argmin(c) == 1: print 'emacs'
    if np.argmin(c) == 2: print 'nothing'

    # hmm ile, her hmm objesine eldeki ceps "sorulur', hangisi daha
    # fazla olurluk rapor ediyorsa o secilir.
