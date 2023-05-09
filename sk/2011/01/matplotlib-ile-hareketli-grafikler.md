# Matplotlib ile Hareketli Grafikler

Robotik hakkinda bir yazida, baska seylerle beraber daha once
bahsetmistik, ama daha detayli olarak; Python Matplotlib ile akici,
interaktif grafikler kodlamasinin kalibi soyle.import
matplotlib.pyplot as pltplt.ion()arkasindan, akisini istedigimiz veri
cercevesinde bir donguyle (for, while, vs) plt uzerinde istedigimiz
cizim cagrilarini teker teker yapiyoruz, ilk cizim sonrasi
plt.hold(True) cagiriyoruz.  Dongude mesela plt.imshow ile bir bitmap
grafigi gosterebiliriz, plt.hold(True) sonrasi bir plt.contour cagrisi
yapabiliriz, sonra bir plt.plot yapabiliriz vs.

Bir "kare" bittikten sonra en sonunda plt.draw cagrisi yapariz
(dikkat, plt.show degil).

"Video" akisi biraz yavaslasin istiyorsak (en basta) import time, ve
yavaslik istedigimiz yerde time.sleep(saniye) ile suni gecikmeyi
ekleriz. Cizim, bekleme bittikten sonra bu ekrani "silmek" icin
plt.hold(False) cagiririz.Kalip soyle:

```
import matplotlib.pyplot as plt
import time
plt.ion()
for .. in range(...):
   plt.imshow(..)
   plt.hold(True)
   CS = plt.contour(..)
   plt.plot(..)  ...
   plt.draw()
   time.sleep(1)
   plt.hold(False)
```   





