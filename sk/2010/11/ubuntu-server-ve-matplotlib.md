# Ubuntu Server ve Matplotlib

Icinde matplotlib kutuphanesini kullanarak grafik olusturan, mesela
plot() kullanan kodlar, gelistirme ortaminda yeni bir pencere acarlar
ve grafigi basarlar. Ama Ubuntu Server uzerinde ayni kodlari
isletiyorsak, ekran tabii ki acilamaz, ama plot() cagirmiyor olsak
bile sadece matplotlib import etmek bile problemli olabiliyor. Su hata
gelebilir:

```
server GtkWarning: could not open display
```

Bu mesaj ekran olmayan server ortaminda grafik olusturulamiyor
demektir.Tamir icin, Python komut satirina girin

>>> import matplotlib
>>> matplotlib.matplotlib_fname()

Son komut bir dosya ismi verecek. Bu dosyaya sudo uzerinden bir editor
ile girin, ve `backend` diyen satirda `GTKAgg` ibaresini `PDF`
kelimesine degistirin. Bundan sonra gorsel islemler bos gecilecek, ama
hesap yapan kodlar hala isleyecek.


