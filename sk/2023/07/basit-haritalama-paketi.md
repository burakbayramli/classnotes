# Yeni Haritalama Paketi

Çorbada bizim de tuzumuz olsun; İnternet bağlantısı gerektirmeyen,
gerekli verisini paket kurulum dosyalarında taşıyan haritalama paketi
bulamadık, kendimiz yazdık - `simplegeomap`.  Açık yazılım olarak
paylaşılıyor [1], ve PyPi üzerinde kurulmaya hazır whl dosyası var,
`pip install simplegeomap` ile kurulabilir.

Simplegeomap temel ihtiyaçları basit, hızlı bir şekilde cevaplaması
için yazılmıştır, bu ihtiyaçlar en azından bizim için istenen bir
bölge içine düşen kıta, ülke sınırlarını çizebilmek, sınırlar
dışındaki denizleri belli bir renkte vermek, çok detaylı olmasa da
yükseklik (dağlar) ve şu alanları (nehir, gol gibi) haritalamanın,
raporlamanın mümkün olması.

Smgm yuvarlık olan yerkürenin farklı şekildeki iki boyuta yansıtma
tekniklerini kullanmıyor, en temel yaklaşım olan boylamı x, enlemi y
kordinatı kabul edip grafiklemeyi bu şekilde yapmayı seçiyor. Bu
yaklaşım her çok uzun mesafelerde kesin olmayabilir, fakat yakın
mesafeler ve objelerin genel yerlerini göstermesi açısından
yeterlidir.

## Kıtalar, Ülkeler

En temel cizimle baslayalim. Bir kordinate merkez alip belli bir
odak (zoom) seviyesine gore o noktadaki kita sinirlarini cizelim,

```python
import simplegeomap as sm

sm.plot_continents(clat=0,clon=0,zoom=20)
plt.savefig('sm_01.jpg',quality=40)
```

![](sm_01.jpg)


Farklı yerlere odaklanabiliriz, 

```python
sm.plot_continents(clat=30,clon=30,zoom=5)
plt.savefig('sm_02.jpg',quality=40)
```

![](sm_02.jpg)

Smgm üstteki türden haritalama için iç renk ve dış renk (`incolor`,
`outcolor`) kavramlarını kullanır. Sınırları olan alanlar, kıtalar, ya
da ülkelerin içi `incolor` ile dışarıda kalan herşey `outcolor` ile
renklenir. Mesela ic kahverengimsi, dis daha koyu mavi istersek bunu
yapabiliriz,

```python
sm.plot_continents(clat=30,clon=30,zoom=5,incolor='yellow',outcolor='blue')
plt.savefig('sm_03.jpg',quality=40)
```

![](sm_03.jpg)

Olağan (default) değerler iç `lightyellow` dış `lightblue` kullanıyor.

Ülkeler için

```python
sm.plot_countries(clat=30,clon=30,zoom=2)
plt.savefig('sm_04.jpg',quality=40)
```

![](sm_04.jpg)

Görüldüğü gibi gösterilen bölgenin içine düşen tüm ülke sınırları çizildi.
Tekrar belirtmek gerekirse, kıta sınırları, ülke sınırlarını içeren veri
dosyaları paketin bir parçası, bu dosyalar kurulum ile beraber geliyorlar
ve her an erişime hazırlar, İnternet bağlantısına gerek yok.


Kaynaklar

[1] https://github.com/burakbayramli/simplegeomap

[2] https://pypi.org/project/simplegeomap/

