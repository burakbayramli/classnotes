# Scanimage

Ubuntu'da scanner'a erismek icin USB uzerinden scanner bilgisayara
baglanir, sonra komut satirindanscanimage -Lile scanner'larin
listesine bakilir. Sizin cihaziniz listede var ise, 2. kolonda gorulen
tirnak icindeki cihaz ismini -d secenegine vererek suna benzer bir
komutu isletebilirsiniz.scanimage --mode=Gray --resolution 200 -x 200
-y 297-d plustek:libusb:005:021 --format=tiff > image.tiffBu komut,
gri modunda, 200 cozunurluk / detayda, eni 200, boyu 297 mm olmak
uzere tiff formatinda bir imaj scan eder ve image.tiff dosyasina
yazar.





