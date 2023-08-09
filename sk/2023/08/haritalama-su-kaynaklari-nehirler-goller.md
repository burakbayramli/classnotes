# Haritalama, Nehirler, Göller

SHP dosyasi olarak farkli detaylarda nehirlerin, gollerin yerlerini
gosteren veri dosyalar GSHHG verisinde bulunabilir. Dosya tek bir zip
[1] olarak indirilebilir, acilinca 

GSHHS_shp

WDBII_shp

dizinleri görülür. 

Nehirler `WDBII_shp` altında ve alt dizinler olarak c,i,f,h,l
mevcut. Bu alt dizinlerin kodlaması şöyle,

`f`: Tam çözünürlük. Verinin en ham, en detaylı hali bu dizin altında.
    
`h`: Yüksek çözünürlük.  Douglas-Peucker çizgi indirgemesi
    kullanılmştir, ve veri büyüklüğü ~80% oranında azaltılmıştır (bazı
    detaylar kaybolmuştur doğal olarak).
    
`i`: Orta seviye çözünürlük.  Bir önceki 'h' seviyesine göre yine ~80%
    azaltma var.
    
`l`: Düşük çözünürlük: Önceki 'i' ye göre ~80% indirim
    
`c`: Kaba çözünürlük. Bir indirim daha, bu en düşük çözünürlük, en
     küçük veri büyüklüğü burada.

Her dosya ismi `WDBII_river_<çözünürlük>_L<seviye>.* olarak
kodlanmıştır, seviye (level) hangi 'tip' su öbeğinin veriye dahil
edildiğini kontrol eder.

Seviye  1: Nehir-goller
Seviye  2: Kalici, buyuk nehirler
Seviye  3: Ek ana nehirler
Seviye  4: Yine ekler
Seviye  5: Kucuk boyuttaki nehirler
Seviye  6: Ana ve gecici nehirler
Seviye  7: Ana ve gecici nehirler - ekler
Seviye  8: Ufak boyuttaki gecici nehirler
Seviye  9: Buyuk kanallar
Seviye 10: Ufak kanallar
Seviye 11: Sulama kanallari

GSHHS_<resolution>_L<level>.*

Seviye 1: Continental land masses and ocean islands, except Antarctica.
Seviye 2: Lakes
Seviye 3: Islands in lakes
Seviye 4: Ponds in islands within lakes
Seviye 5: Antarctica based on ice front boundary.
Seviye 6: Antarctica based on grounding line boundary.














Kaynaklar

[1] https://www.ngdc.noaa.gov/mgg/shorelines/data/gshhg/latest/

