# Imaj Uzerine Text

OpenCV ile imaj işlerken aynı imaj üzerine yazı / metin (text) koymak
istiyorsak, şu çağrıyı kullanabiliriz.

```
pt1 = cv.cvPoint (100, 120)
font = cv.cvInitFont (CV_FONT_HERSHEY_SIMPLEX, 1.0, 1.0, 0, 1, CV_AA)
cvPutText (image, "Testing text rendering!", pt1, font,CV_RGB(255,0,0))
```

Font parametresinden sonra gelen her iki 1.0 yazının büyüklüğünü, en
sondaki 1 ise kalınlığını temsil ediyor. CV_RGB parametresi yazının
rengi için kullanılıyor (255,0,0 kırmızı rengi).





