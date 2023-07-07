# Yeni Haritalama Paketi

İnternet bağlantısı gerektirmeyen, gerekli verisini paket kurulum
dosyalarında taşıyan haritalama paketi revaçta yoktu, yazmak zorunda
kaldık - `simplegeomap`.  Açık yazılım olarak paylaşılıyor [1], ve
PyPi üzerinde kurulmaya hazır whl dosyası var, `pip ınstall
simplegeomap` ile kurulabilir.

Simplegeomap temel ihtiyaçları basit, hızlı bir şekilde cevaplaması
için yazılmıştır, bu ihtiyaçlar en azından bizim için istenen bir
bölge içine düşen kıta, ülke sınırlarını çabuk bir şekilde çizebilmek,
sınırlar dışında kalanları (mesela denizler) belli bir renkte vermek,
çok detaylı olmasa da yükseklik (dağlar) ve su alanları (nehir, gol
gibi) haritalamanın, raporlamanın mümkün olması.

Smgm yuvarlak olan yerkürenin farklı şekildeki iki boyuta yansıtma
tekniklerini kullanmıyor, en temel yaklaşım olan boylamı x, enlemi y
kordinatı kabul edip veriyi direk kartezyen hale getirme teknigini
seçiyor. Bu yaklaşım her çok uzun mesafelerde kesin olmayabilir, fakat
yakın mesafeler ve objelerin genel yerlerini göstermesi açısından
yeterlidir.

## Kıtalar, Ülkeler

En temel çizimle başlayalım. Bir kordinatı merkez alıp belli bir
odak (zoom) seviyesine göre o noktadaki kıta sınırlarını çizelim,

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
renklenir. Mesela iç kahverengimsi, dış daha koyu mavi istersek bunu
yapabiliriz,

```python
sm.plot_continents(clat=30,clon=30,zoom=5,incolor='yellow',outcolor='blue')
plt.savefig('sm_03.jpg',quality=40)
```

![](sm_03.jpg)

Olağan (default) değerler iç `lightyellow` dış `lightblue`. Kullanici bu degerleri
goruldugu gibi degistirebiliyor.

Ülkeler,

```python
sm.plot_countries(clat=30,clon=30,zoom=2)
plt.savefig('sm_04.jpg',quality=40)
```

![](sm_04.jpg)

Görüldüğü gibi gösterilen bölgenin içine düşen tüm ülke sınırları
çizildi.  Tekrar belirtmek gerekirse, kıta sınırları, ülke sınırlarını
içeren veri dosyaları paketin bir parçasıdır, bu dosyalar kurulum ile
beraber gelirler ve her an erişime hazır olacaklardır, İnternet
bağlantısına ihtiyaçları yoktur.

Paket grafiklemesini yaparken temel `matplotlib` kavramlarını
kullanır, zaten enlem/boylam bilgisini boylam/enlem olarak x,y
noktaları olarak kullandığımızı söylemiştik, bu durumda, eğer ek
bilgileri haritaya konuşlandırmak istersek bunu hala `matplotlib`
yapılarıyla x,y kordinatlarını kullanarak yapabiliriz. Mesela üstteki
haritada Mısır'da enlem=28, boylam=30 noktasını göstermek istiyorum,
oraya bir kırmızı 'elmas' şekli koyabilirim, altına yazı yazabilirim,

```python
sm.plot_countries(clat=30,clon=30,zoom=2)
plt.plot(30,28,'rd')
plt.text(25,28.5,'Buraya dikkat')
plt.savefig('sm_05.jpg',quality=40)
```

![](sm_05.jpg)

Bolgeler, Cizgiler

Bir kordinat listesi ile bir eğri, çizgi göstermek mümkündür. Çizgiyi
temsil eden noktaları liste olarak smgm'e verince çizgi gösterilir.

Eğer kapalı bölge istersem verilen listenin son kordinatı ilk
kordinatına bağlı kabul edilecektir, ve alan kapatılacaktır.

```python
pts = np.array([[40,10],[43,10], [43,15]])
fig, ax = plt.subplots()
sm.plot_countries(clat=40,clon=10,ax=ax,zoom=1.0)
sm.plot_line(pts,ax,color='red')
plt.savefig('sm_06.jpg',quality=40)
```

![](sm_06.jpg)

```python
fig, ax = plt.subplots()
sm.plot_countries(clat=40,clon=10,ax=ax,zoom=1.0)
sm.plot_region(pts,ax,color='lightpink')
plt.savefig('sm_07.jpg',quality=40)
```

![](sm_07.jpg)



Kaynaklar

[1] https://github.com/burakbayramli/simplegeomap

[2] https://pypi.org/project/simplegeomap/

