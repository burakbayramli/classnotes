# Mikrofon Verisi, Grafiklemek, FFT

Mikrofondan gelen veriyi ekranda surekli olarak plot etmek, fourier
transformunu almak gibi islemler icin neler yapmali? Oncelikle, daha
once belirttigimiz "fromstring" ve "fromfile" usulu okuma sekli bize
tum wav verisini veriyor ve bu verinin icinde baslik (header) bilgisi
de var. Verinin sadece bizi ilgilendiren kismini alabilmek icin wav
formatini taniyan turden bir kutuphane lazim.Bunun icin audiolab ve
libsnd adli iki kutuphane kullanacagiz. Audiolab bir Python
kutuphanesidir ve C ile yazilmis libsnd adli alt kutuphaneyi sarmalar
(wrap). Ilk once libsnd kurulacak; suradan. Tipik C kaynak kod bazli
kurulum: "configure" ve "sudo make install". Daha sonra audiolab
Github'dan alinip kurulabilir. Bu da klasik: "sudo python setup.py
build install".Simdi, surekli olarak mikrofondan veri alip zaman
serisi olarak ekrana basan program soyle:

```
import os, matplotlib.pyplot as plt
import audiolab
import numpy as np

from scipy import *

plt.ion()

while (True):
   plt.clf()
   os.system("arecord -d 1 -f dat > /mnt/rd/ses.wav")
   (snd, sampFreq,nBits) = audiolab.wavread('/mnt/rd/ses.wav')
   s1 = snd[:,0] timeArray = np.arange(0, float(snd.shape[0]), 1)
   timeArray = timeArray / sampFreq
   timeArray = timeArray * 1000 plt.plot(timeArray, s1, color='k')
   plt.ylabel('Amplitude') plt.xlabel('Time (ms)')
   plt.show()
```

Programin daha onceki yazida belirttigimiz ramdisk kavramini
kullandigini goruyoruz. Mikrofondan gelen veriler direk ramdisk'e
yaziliyor, ve hemen arkasindan wavread ile okunuyor. Boylece /mnt/rd
bir ara tampon bolgesi haline geliyor, fakat bu bolge hafizada oldugu
icin bu tamponlama isi gayet hizli isliyor.Fourier transform alip
ekranda gosteren program soyle:

```
import os, matplotlib.pyplot as pltimport audiolabimport numpy as
npfrom scipy import *plt.ion()while (True): plt.clf()
os.system("arecord -d 1 -f dat > /mnt/rd/ses.wav") (snd, sampFreq,
nBits) = audiolab.wavread('/mnt/rd/ses.wav') s1 = snd[:,0] n = len(s1)
p = fft(s1) nUniquePts = ceil((n+1)/2.0) p = p[0:nUniquePts] p =
abs(p) p = p / float(n) p = p**2 if n % 2 > 0: p[1:len(p)] =
p[1:len(p)] * 2 else: p[1:len(p) -1] = p[1:len(p) - 1] * 2 freqArray =
np.arange(0, nUniquePts, 1.0) * (sampFreq / n);
plt.plot(freqArray/1000, 10*log10(p), color='k') plt.xlabel('Frequency
(kHz)') plt.ylabel('Power (dB)') plt.show()
```

Bu noktadan sonra eger ek islemler yapilmak istense, mesela bu ses
verisi, fft sonrasinda alinip onceden kaydedilmis kelimelerin fft
ciktilari ile karsilastirilip oruntu tanima (pattern recognition) ile
kelime, cumle tanima islemleri yapilmak istense, bu
yapilabilir. Performans problem olmamalidir cunku libsnd C ile
yazilmis, ek matematiksel islemleri icin kullanilacak numpy, scipy
ayni sekilde derlenmis kod bazlarindan olusuyor.





