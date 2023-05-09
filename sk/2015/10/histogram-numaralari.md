# Histogram Numaraları

Matplotlib kutuphanesinin histogram hist cagrisinin histogram
grafigini basmak haricinde bazi ek ozellikleri var. Mesela bir verinin
histograminin kutu (bins) noktalarinin nerede oldugunu
raporlattirabiliriz, ayrica bu kutularin  icine ne kadar veri
dustugunu, sayi ya da normalize edilmis sayi olarak
gorebiliriz. Normalize edilmis sayi ozellikle onemli cunku bu sayi,
olasiliksal baglamda, bir tur sayisal yogunluk fonksiyonu (density
function) olarak gorulebilir.

Ornek

1,2,3,..,40 sayilarini iceren bir vektor uzerinden,

```
hst = plt.hist(range(40), bins=3)
print hst[1]
print hst[0]
```

```
[  0.  13.  26.  39.]
[ 13.  13.  14.]
```

Sonuclardan birincisi her kutunun sinir noktalarini veriyor. 3 kutu
olustur dedik (bins=3 ile) ve mesela 1. kutu 0 ile 13 arasinda
olusmus, sonraki 23-26, vs. Kutular icinde  sirasiyla 13,13 ve 14 tane
deger var. 

Eger kutu icindeki sayilari olasiliksal yogunluk degeri gibi gormek
isteseydik, normed=True parametresini vermek lazim, 

```
hst = plt.hist(range(40), normed=True,bins=3)

print hst[0]
```

```
[ 0.025       0.025       0.02692308]
```

Yogunluklar birbirine cok yakin tabii ki cunku verimiz 1-40 arasi
sayilar. Dagilimi daha degisik olan bir veri uzerinde farkli sayilar
gorurduk. Mesela orta noktasi 10, standart sapmasi 0.1 olan normal
(gaussian) dagilmis / uretilmis sayilar uzerinden, 

```
data = np.random.normal(mu,sigma,40)

hst = plt.hist(data, normed=True,bins=3)

print hst[0]

print hst[1]
```

```
[ 1.996045    4.19169449  1.7964405 ]

[  9.79075557   9.91600325  10.04125093  10.1664986 ]
```

Orta noktada cok daha fazla yogunluk oldugunu goruyoruz, bu normal
tabii cunku dagilim da normal (!). Not: yogunluk fonksiyonu 1'den
buyuk degerler dondurebilir (ama kumulatif yogunluk donduremez). 

