# Python Profilleme (Profiling)

```
sudo pip install line_profiler
```

Komut satirindan isletmek

```
import time

@profile
def f():
    for i in range(100): print i
    g()
   def g():
    time.sleep(1)

if __name__ == "__main__":
    f()
```

Komut satirinda

```
python -m kernprof -l -v test.py > /tmp/prof1.txt
```

Sonuc 

```
Line #
      Hits
         Time
  Per Hit
   % Time
  Line Contents==============================================================
     3
                                           @profile
     4
                                          
     5
                                          
def f():
     6
       101
          233
      2.3
      0.0
     
for i in range(100): print i
     7
         1
      1001139 1001139.0
    100.0
      g()
 
```

Sonuca bakarsak zamanin yuzde yuzunun g() icinde gecirildigini
goruyoruz. Bu normal cunku icinde sleep ibaresi var! Hits bu satirin
kac kez isletildigini gosterir, time bu satirda toplam ne kadar zaman
gecirildigini gosterir. Per hit satirin her isletildiginde ne kadar
zaman harcandigini belirtir. Yuzde ise tum isletim icinde bu satirda
yuzde kac zaman harcandigini belirtir.

Not: Eger  ustteki kodu python test.py olarak isletirseniz @profile
dekoratorunun import edilmedigi hakkinda sikayet gelecektir.

Bir diger yontem, (sudo pip install profilehooks yaptiktan sonra),

```
from profilehooks import profile
```

ile dekaratoru import etmek. O zaman normal komut satirindan python
test.py ile isletim olur. Ya da herhangi bir sekilde kod isletimi
olur, mesela Flask icinden yuklenip ayni kodun isletilmesi gibi. Bu
durumda sonuc raporlari bu modulu import eden surec "bir sekilde
bittigi" zaman ekrana basilir.

Bizim tercihimiz ilk gosterilen secenek. Calisma seklimiz soyle:
sonuclara bakip, duzeltme yapiyoruz, tekrar isletiyoruz, daha sonra
@profile dekatorunu kod icinde cagrim zinciri acisindan daha derin
noktalara koyuyoruz, mesela a ->b -> c cagrimi var, a ile basliyoruz,
sonra dekorator b, c uzerine konanabiliyor. Ilk yontemin onemli bir
avantaji sonuclarin bir dosyaya hemen yazilabiliyor olmasi.

Bu profilleme yontemi "satir profillemesi (line profiler)" olarak
biliniyor.






