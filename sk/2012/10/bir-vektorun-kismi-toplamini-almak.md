# Bir Vektorun Kismi Toplamini Almak

Diyelim ki bir Numpy vektorunun tum elemanlarini degil, ucer ucer
bloklar halinde toplamak istiyoruz. Kullanilacak komut add.reduceat
komutu.

```
import numpy as np

a = np.array([1,2,3,4,5,6])print np.add.reduceat(a,[0,3])

[ 6 15]
```

sonucunu verir. Yani sifirinci ve ucuncu indis arasindaki bir toplam,
ucuncu ve vektor sonuna kadar olan bolum baska bir toplam haline
geldi. Geri dondurulen sonuc her zaman verilen indis matrisi ile esit
olacaktir.






