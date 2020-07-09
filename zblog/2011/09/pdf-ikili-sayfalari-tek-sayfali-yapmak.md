# İkili PDF Sayfaları Tek Sayfalı Yapmak (Ortadan Bölmek)

2 sayfa yanyana scan edilmiş PDF dokümanlarını normal hale getirmek
için alttaki yöntem kullanılabilir.

Önce pdfjam adlı program lazım, `apt-get` bunu kurar. Ardından

```
pdfjam -o cift.pdf --trim '14cm 0cm 0cm 0cm' --clip true --scale 1.0 dokuman.pdf
```

```
pdfjam -o tek.pdf --trim '0cm 0cm 14cm 0cm' --clip true --scale 1.0 dokuman.pdf
```

```
pdftk A=cift.pdf B=tek.pdf shuffle B A output final_dokuman.pdf
```

Bu komutların yaptığı şu; biri sağdan bir soldan olmak üzere 14 cm'lik
bölümü kesip atıyoruz (trim ve clip true), ve bu budanmış
dokümanlardan birini tek sayılı sayfalar, diğerini çift sayılı
sayfalar olarak kaydediyoruz. Sonra bir pdftk taklası atmak gerekiyor,
bu komuta iki dökümanı birleştirmesini söylüyoruz, ama tek sayfaları
bir yerden, çift sayfaları başka bir yerden almasını söylüyoruz
(shuffle B A seçeneği)




