import scipy.io.wavfile
from scikits.talkbox.features import mfcc

sample_rate, X = scipy.io.wavfile.read('computer.wav')
print len(X), type(X)
ceps, mspec, spec = mfcc(X)
print len(X) / 256, ceps.shape
print 88064 / 549.

