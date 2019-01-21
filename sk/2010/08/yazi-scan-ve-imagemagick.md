# Imagemagick, Imaj Dosyalarini Islemek, Format Degisimi, Convert Komutu


Imagemagick, Imaj Dosyalarini Islemek, Format Degisimi, Convert Komutu




Ubuntu Linux uzerinden formatlararasi gecis cok basit, ImageMagick kurulur,

sudo apt-get install imagemagick

Bu bize convert adli bir program veriyor. Mesela dosya.jpg dosyasini dosya.png yapmak icin

convert dosya.jpg dosya.png

Eger animasyon gif dosyasi uzerinde ayni komutu isletirsek, animasyondaki her kare ayri bir png dosyasi olarak verilirdi. Sonek png dosyalarinin numaralandirmasi kontrol edilebilir, Dikkat: tum imajin animasyondan cikartilmasi icin -coalesce secenegi gerekebilir.

convert -coalesce dosya.gif dosya-%3d.png 


Bu komutun pek cok ozelligi var.

Bir dosya icinden belli bir bolgeyi cikartmak icin, mesela ust sol kose referansli olmak uzere 100,100 noktasindan baslayarak sadece1000 genisliginde 600 yukseliginde bolgeyi almak icin

convert -extract 1000x600+100+000 dosya.jpg sonuc.jpg

Pek cok goruntu dosyasini ust uste birlestirmek icin (arada 20 pikselllik bosluk ile)

convert -bordercolor White -border 2x20 dosya_*.jpg -append out.jpg

Diyelim ki iki sayfasi tek bir kagitta yatay basilmis bir dokumani scan etmek istiyoruz. Scanimage ile tiff ciktilari aldiktan sonra, imaji cevirmek icin

convert -rotate 270 [dosya1.tiff] [dosya2.tiff]

270 yerine 90 da olabilir tabii, scan etme pozisyonuna gore dogrusunu secin. Eger imaj yeterince net degilse, netlestirmek (sharpening) icin su komut

convert ... -unsharp 1.5x1.2+1.0+0.10  [dosya1.tiff] [dosya2.tiff]

Rotate ve unsharp isleri tek bir satirda ayni anda yapilabilir.

Scan Edilmis Imajlari Ortadan Bolmek

Eger elinizde iki sayfasi tek bir kagida basilmis sekilde bir scan imaji varsa (bu sekilde basilmis bir kitaptan elde edilmis olabilir), bu imaji ortadan yatay olarak ikiye bolerek iki imaj elde etmenin caresi ImageMagick ile soyle:

convert dosya.tiff -crop 100%x50% +repage dosya_%d.tiff

Bu cagridan sonuc olarak dosya_00.tiff, dosya_01.tiff adinda iki imaj ortaya cikacak.

Bir video dosyasini numaralanmis goruntu dosyalarina cevirmek icin,

convert video.avi chessb-right/video%03d.png




