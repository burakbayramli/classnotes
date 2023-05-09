# Linux ile Mikrofona Baglanmak

Linux uzerinde standart mikrofon ve ses geri calma araclari olan
arecord ve aplay ile disaridan gelen sesleri bilgisayara
aktarabilirsiniz.arecord -d 1 -f datkomutu bir saniye sureli olarak
wav formatina uyumlu ses verisini ekrana basar. Bu bilgiyi bir wav
dosyasina aktarabiliriz, ama bu kaydi ek araclar ile islemden gecirmek
istiyorsak (mesela ses tanima uygulamalari icin) o zaman bir Python
scripti ile isletebiliriz.

```
import osoutput = os.popen('arecord -d 2 -f dat').read()
```

Su anda output String tipindedir ve bu String icindeki her karakter
ses kaydinin bir parcasini tasimaktadir.print len(output)print
type(output)print output[1:10]komutlari sirasiyla bu verinin
uzunlugunu, tipini ve ilk 10 karakteri ekrana basar. Eger dosyaya
yazmak istiyorsak

```
outfile = open("/tmp/out.wav", "w")
outfile.write(output)
```

Simdi aplay /tmp/out.wav komutu ile bu ses kaydinin dinlememiz
mumkun. Degisken output icindeki veriyi Numpy, Scipy ile islemden
gecirebiliriz; FFT (fast fourier transform) bu islemlerden sadece bir
tanesi.





