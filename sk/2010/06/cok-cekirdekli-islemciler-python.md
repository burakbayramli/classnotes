# Cok Cekirdekli Islemciler, Python

Moore kanunda (tek islemci baglaminda) bir duvara carpildi; Islemciler
artik cok cekirdekli (multicore) mimariye dogru gidiyor. Dizustunde en
az iki olmak uzere, 4, 8, vs. gibi cok cekirdekli islemciler normal
hale gelmeye basladi. Buna ek olarak: Cok cekirdekli mimarilerdeki tek
bir cekirdek (islemci), sadece bir tane islemci iceren "eski usul"
mimarideki tek islemciden daha yavas olacak. Yani programcilar
yazdiklari kodu hizlandirmak istiyorlarsa muhakkak cok cekirdekli
ortamda calismaya alismali; yoksa eldeki mevcut hizi bile yeni
mimaride kaybetmemiz mumkun.Python baglaminda: birden fazla cekirdegi
kullanmak icin tavsiye edilen Thread objesini degil, `multiprocessing`
adli bir paketi kullanmak. Thread objeleri GIL adli bir kilite
takiliyor, bu cpython derleyicisi ile ilgili bir durum (imis); bu
sebeple cekirdekleri kullanmak icin multiprocessing gibi ayri / farkli
bir yaklasim gerekli.Bu paket baslatilan ilk surec icinde alt surecler
(subprocess) baslatiyor, ve bu alt surecler dogru sekilde birden fazla
cekirdege yayilabiliyor.

Kurmak icin onceki baglantidan zip dosyasini indirin, acin ve "sudo
python setup.py install" komutunu isletin.

Ornek bir kod:

```python
from multiprocessing import Process
from pylab import *

import timedef f(name):
   while 1:
      print 'hello', name
      for i in range(10000):
         y = cos(i)

if __name__ == '__main__':
  p1 = Process(target=f, args=('first',))
  p1.start()
  p2 = Process(target=f, args=('second',))
  p2.start()
```

Bu ornekte iki surec baslatiyoruz, her surecte matematik cosine
islemini yaptiriyoruz ve ekrana o surece ozel bir seyler
basiyoruz. Isletince Ubuntu uzerinde System | Administration | System
Monitor ile cekirdeklerin durumuna bakabilirsiniz. Bizde mevcut iki
cekirdek %100'e yakin kullanim (utilization) rapor ettiler.





