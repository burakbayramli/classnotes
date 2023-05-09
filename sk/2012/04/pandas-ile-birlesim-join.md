# Pandas ile Birlesim (Join)

Iki cetrefil SQL'in urettigi sonuclarin bir adres kodu (zip) kolonu
uzerinden birlestirilmesi gerekiyordu, ve sartlar oyledir ki Web
sitemizin surekli kullandigi tabanin bir kopyasi olan
sadece-okunabilir (read-only) tabana (dogal olarak) yazim yetkisi
mevcut degildi. Sonuclar nasil birlestirilebilir?

Iki SQL'in ciktisi CSV olarak alinir. Sonra bu ciktilar ayri ayri
Pandas ile DataFrame olarak yuklenir. Pandas'in DataFrame'leri hafiza
icinde sanki tabloymus gibi birlestirebilme (join) ozelligi vardir!
Kod altta. Not olarak duselim, Pandas ve Numpy oldukca buyuk csv
dosyalarini hafizada cok hizli isleyebilir. Bizim durumumuzda 100 MB
buyuklugunde ciktilar vardi, ve Pandas hizla birlesimi gerceklestirdi.

```
import numpy as np
from pandas import *

csv1 = read_csv("dosya1.csv")
csv2 = read_csv("dosya2.csv")
res =  merge(csv1, csv2, left_on='plz', right_on='zip', how='inner')
res.to_csv("sonuc.csv",  sep='\t')
```




