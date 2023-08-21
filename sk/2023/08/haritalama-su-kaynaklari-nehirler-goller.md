# Haritalama, Nehirler, Göller

SHP dosyası olarak farklı detaylarda nehirlerin, gollerin yerlerini
gösteren veri dosyalar GSHHG verisinde bulunabilir. SHP dosyalarının
nasıl işleneceğini [2] yazısında görmüştük. Şu alanları verisi tek bir
zip [1] olarak indirilebilir, açılinca

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

Her dosya ismi

`WDBII_river_<çözünürlük>_L<seviye>.*`

olarak kodlanmıştır, seviye (level) hangi 'tip' su öbeğinin veriye
dahil edildiğini kontrol eder.

Nehirler için,

Seviye  1: Nehir-göller

Seviye  2: Kalıcı, büyük nehirler

Seviye  3: Ek ana nehirler

Seviye  4: Yine ekler

Seviye  5: Küçük boyuttaki nehirler

Seviye  6: Ana ve geçici nehirler

Seviye  7: Ana ve geçici nehirler - ekler

Seviye  8: Ufak boyuttaki geçici nehirler

Seviye  9: Büyük kanallar

Seviye 10: Ufak kanallar

Seviye 11: Sulama kanalları

Göller için,

GSHHS_<resolution>_L<level>.*

Seviye 1: Kıtasal satıhlar ve Antartica haricindeki okyanus adaları

Seviye 2: Göller

Seviye 3: Göller içindeki adalar

Seviye 4: Göller içindeki adalardaki ufak göller

Seviye 5: Buz kütlelerine göre Antartika sınırı

Seviye 6: Kıta sathına göre Antartika sınırı

Gerekli detaylılık, bilgi tipine göre üstteki seçeneklerden herhangi biri
devreye sokulabilir.

Bazı önemli notlar; bazen yaygın olarak 'nehir' olarak tanımlanan
alanlar GSHHG için bu şekilde tanımlanmamış olabilir. Dışarıdan
gözleyenler için birbiri ile bağlantılı çok geniş olmayan ve akıntısı
olan şu öbekleri bağlantılı bir nehirdir, fakat bu şekilde grafikleme
yaptığımızda, örnek olarak Ukrayna'daki Dniper nehrini alalım,

```python
from pygeodesy.sphericalNvector import LatLon, perimeterOf, meanOf
import shapefile
data_dir = "/tmp"
lim = 2
```

```python
def plot_rivers():
    file = data_dir + "/WDBII_river_c_L02.shp"
    rivers = []
    sf = shapefile.Reader(file)
    r = sf.records()
    waters = sf.shapes()
    for idx in range(len(waters)):
        water = waters[idx]
        bounds = list(water.parts) + [len(water.points)]
        for (previous, current) in zip(bounds, bounds[1:]):
            geo = [[x[1],x[0]] for x in water.points[previous:current]]
        if len(geo) < lim: continue
        latlons = [LatLon(a[0],a[1]) for a in geo]
        per = np.round(perimeterOf(latlons, radius=6371),2)
        mid = meanOf(latlons)
        latlons = [[a.lat,a.lon] for a in latlons]
        regarr = np.array(latlons)    
        plt.plot(regarr[:,1],regarr[:,0],color='cyan')
```

```python
plot_rivers()
plt.text(35.14619940,47.8257815,"Zaporizhza")
plt.text(34.58240228,48.5429277,"Kamianske")
plt.xlim(29,37)
plt.ylim(47,55)
plt.savefig('su1.jpg',quality=30)
```

Bu kodla [şuradaki gibi](su1.jpg) bir görüntü ortaya çıkacaktır. Ana
nehirleri seçtik, detay `c` seviyesi, kabaca olsa bile bir nehir
görüntüsünün ortaya çıkmasını bekledik, fakat olmadı. Niye? Burada
eksik olan GSHHG verisinin Dnieper'in bazı bölümlerini göl olarak
kaydetmiş olmasıdır. O zaman hem nehir hem göl grafiklemesi gerekir,

```python
def plot_lakes():
    file = data_dir + "/GSHHS_l_L2.shp"
    rivers = []
    sf = shapefile.Reader(file)
    r = sf.records()
    waters = sf.shapes()
    for idx in range(len(waters)):
        water = waters[idx]
        bounds = list(water.parts) + [len(water.points)]
        for (previous, current) in zip(bounds, bounds[1:]):
            geo = [[x[1],x[0]] for x in water.points[previous:current]]
        if len(geo) < lim: continue
        latlons = [LatLon(a[0],a[1]) for a in geo]
        per = np.round(perimeterOf(latlons, radius=6371),2)
        mid = meanOf(latlons)
        latlons = [[a.lat,a.lon] for a in latlons]
        regarr = np.array(latlons)    
        plt.fill(regarr[:,1],regarr[:,0],color='cyan')

plot_rivers()
plot_lakes()
plt.text(35.14619940,47.8257815,"Zaporizhza")
plt.text(34.58240228,48.5429277,"Kamianske")
plt.xlim(29,37)
plt.ylim(47,55)
plt.savefig('su2.jpg',quality=30)
```

![](su2.jpg)

Bu daha net bir Dnieper görüntüsü ortaya çıkardı. 

Kaynaklar

[1] https://www.ngdc.noaa.gov/mgg/shorelines/data/gshhg/latest/

[2] ../../2020/02/haritalamak.html

