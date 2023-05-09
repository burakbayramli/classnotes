# Imagemagick, Imaj Dosyalarini Islemek, Format Degisimi, Convert Komutu

Ubuntu Linux üzerinden formatlararası geçiş çok basit, ImageMagick kurulur,

```
sudo apt-get install imagemagick
```

Bu bize `convert` adlı bir program veriyor. Mesela `dosya.jpg` dosyasını
dosya.png yapmak için

```
convert dosya.jpg dosya.png
```

Bir imajı yarısına indirmek / küçültmek için `-scale 50%` denir, yüzde
50 yani. Eğer Web'de yayın için fotoğraf boyutu indirmek istiyorsak
çoğunlukla ufak bir dosya isteriz ve biraz kalite kaybetmek bizi
endiselendirmez, o zaman kalitede indirim de yapılabilir, `-quality 90` ile,

```
convert buyuk_resim.jpg  -quality 90  -scale 50% yeni_resim.jpg
```


Eğer animasyon gif dosyası üzerinde aynı komutu işletirsek,
animasyondaki her kare ayrı bir png dosyası olarak verilirdi. Sonek
png dosyalarının numaralandırması kontrol edilebilir, Dikkat: tüm
imajın animasyondan çıkartılması için -coalesçe seçeneği gerekebilir.

```
convert -coalesce dosya.gif dosya-%3d.png
```

Birkaç png dosyasını animasyona çevirmek

```
convert -loop 0 -delay 100 *.png out.gif
```

Bu komutun pek çok özelliği var.

Bir dosya içinden belli bir bölgeyi çıkartmak için, mesela üst sol
köse referanslı olmak üzere `100,100` noktasından başlayarak sadece
1000 genişliğinde 600 yükseliğinde bölgeyi almak için

```
convert -extract 1000x600+100+000 dosya.jpg sonuc.jpg
```

Pek çok görüntü dosyasını üst üste birleştirmek için (arada 20
pikselllik boşluk ile)

```
convert -bordercolor White -border 2x20 dosya_*.jpg -append out.jpg
```

Yanyana (horizontally) birleştirmek için `-append` yerine `+append`

```
convert -bordercolor White -border 2x20 dosya_*.jpg +append out.jpg
```

Diyelim ki iki sayfası tek bir kağıtta yatay basılmış bir dokümanı
scan etmek istiyoruz. Scanımage ile tiff çıktıları aldıktan sonra,
imajı çevirmek için

```
convert -rotate 270 [dosya1.tiff] [dosya2.tiff]
```

270 yerine 90 da olabilir tabii, scan etme pozisyonuna göre doğrusunu
seçin. Eğer imaj yeterince net değilse, netleştirmek (sharpening) için
şu komut

```
convert ... -unsharp 1.5x1.2+1.0+0.10  [dosya1.tiff] [dosya2.tiff]
```

Rotate ve unsharp işleri tek bir satırda aynı anda yapılabilir.

Scan Edilmiş İmajları Ortadan Bölmek

Eğer elinizde iki sayfası tek bir kağıda basılmış şekilde bir scan
imajı varsa (bu şekilde basılmış bir kitaptan elde edilmiş olabilir),
bu imajı ortadan yatay olarak ikiye bölerek iki imaj elde etmenin
çaresi İmageMagick ile şöyle:

```
convert dosya.tiff -crop 100%x50% +repage dosya_%d.tiff
```

Bu çağrıdan sonuç olarak dosya_00.tiff, dosya_01.tiff adında iki imaj
ortaya çıkacak.

Bir video dosyasını numaralanmış görüntü dosyalarına çevirmek için,

```
convert video.avi chessb-right/video%03d.png
```

İki resmi yanyana eklemek için

```
convert resim1.jpg resim2.jpg +append output.jpg
```

GIF resmi optimize etmek için

```
convert -scale 70% -fuzz 6% +dither -layers Optimize +map in.gif out.gif
```

Arka plandaki saydamlık (transparent) durumunu çıkartmak için

``
convert dosya.png -background white -alpha remove -alpha off out.png
```

Işık seviyesi (brightness) ve farklılık (contrast) değişimi için

```
convert -brightness-contrast 30x20 in.tif out.tif
```

Bu örnekte ışık seviyesi 30 farklılık 20 arttırıldı.

