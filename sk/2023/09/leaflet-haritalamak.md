# Leaflet ile Haritalamak

Python bazlı haritalama paketi Folium yazılımından daha önce [1]
bahsettik, aslında Folium paketinin ilk çıkışı Javascript bazlı
Leaflet yazılımıdır. Hatta Folium Python çağrıları sonrası `save` ile
kaydedilebilen HTML dosyasına bakarsak orada leaflet Javascript
çağrıları olduğunu göreceğiz. Yani Folium aslında Leaflet'i
sarmalayan (wrap) ona arayüz sağlayan ince bir tabakadır.

Leaflet'in en avantajlı tarafı Javascript bazlı, yani tarayıcıda
işleyen kodları sayesinde dinamik görüntülenen haritaları
yaratabilmesidir. Yakınlaştırma (zoom), sağa sola kaydırma gibi pek
çok işlemi bir Leaflet haritası üzerinde yapabiliriz. Ayrıca leaflet
arka planda "fayans (tile)" denen görüntüleri basabilir, bunlar mevcut
bir haritalama veri tabanı tarafından önceden üretilmiş farklı
yakınlık seviyelerindeki görüntülerdir, yakınlık seviyesi kullanıcı
tarafından arttırıldıkça gerekli görüntü haritalama servisinden alinip
arka plana koyulur. Leaflet tüm bu işlemleri otomatik olarak yapar.
Fayanslar mesela dağları, nehirler gösteren bir doğa tabakası olabilir,
ya da tüm caddeleri, dükkanları gösteren bir şehir tabakası olabilir.
Kullanıcı istediği fayans tabakasını haritayı oluştururken seçebilir.

### Giriş

En basit leaflet kodu alttaki gibi,

```html
<html>
  <head>
    <meta name="viewport" content="width=device-width, initial-scale=1.0"/>
    <link rel="stylesheet" href="https://unpkg.com/leaflet@1.9.4/dist/leaflet.css"
          integrity="sha256-p4NxAoJBhIIN+hmNHrzRCf9tD/miZyoHS5obTRR9BMY="
          crossorigin=""/>
    <style>
      #map {
        height: 500px;
      }
    </style>
  </head>

  <script src="https://unpkg.com/leaflet@1.9.4/dist/leaflet.js"
          integrity="sha256-20nQCchB9co0qIjJZRGuk2/Z9VM+kNiyxNV1lvTlZBo="
          crossorigin=""></script>

  <script>
    function init() {
      map = L.map('map').setView([40,30], 6);
      L.tileLayer('https://tile.openstreetmap.org/{z}/{x}/{y}.png', {
                   maxZoom: 19,
                   attribution: 'OSM'
      }).addTo(map);
   }
  </script>
  
  <body onload='init()'>    
    <div id="map"></div>    
  </body>

</html>
```

[HTML](leaf1.html)

Farklı fayans servisleri var, üstteki servis her ülkenin şehirlerini
kendi alfabesi ile veriyor, eğer Latinize edilmiş isimler içeren
haritalar görmek istiyorsak, Basemap servisinden,

```
https://{s}.basemaps.cartocdn.com/rastertiles/voyager/{z}/{x}/{y}.png
```

adresini kullanabiliriz. 

### İşaretler

Bu haritaya gösterime sunmadan önce bazı ekler yapabiliriz. Mesela
farklı renklerde işaretleyici (marker) kullanabiliriz. Bu işaretler
için imaj dosyaları lazım, altta bu dosyalar bulunabilir,


[marker-icon-2x-black.png](marker-icon-2x-black.png),
[marker-icon-2x-blue.png](marker-icon-2x-blue.png),
[marker-icon-2x-gold.png](marker-icon-2x-gold.png),
[marker-icon-2x-green.png](marker-icon-2x-green.png),
[marker-icon-2x-grey.png](marker-icon-2x-grey.png),
[marker-icon-2x-orange.png](marker-icon-2x-orange.png),
[marker-icon-2x-red.png](marker-icon-2x-red.png),
[marker-icon-2x-violet.png](marker-icon-2x-violet.png),
[marker-icon-2x-yellow.png](marker-icon-2x-yellow.png),
[marker-shadow.png](marker-shadow.png)

İşaretleyici ekleme kodları,

```javascript
var LeafIcon = L.Icon.extend({
       	options: {
             shadowUrl: 'marker-shadow.png',
             iconSize:     [20, 40],
             shadowSize:   [25, 30],
             iconAnchor:   [10, 45],
             shadowAnchor: [2, 30],
             popupAnchor:  [-1, -30]
        }
});
   
var orangeIcon = new LeafIcon({iconUrl: 'marker-icon-2x-orange.png'});
var yellowIcon = new LeafIcon({iconUrl: 'marker-icon-2x-yellow.png'});
var greenIcon = new LeafIcon({iconUrl: 'marker-icon-2x-green.png'});

L.marker([41,31], {icon: orangeIcon}).addTo(map);
L.marker([41,32], {icon: yellowIcon}).addTo(map);
L.marker([42,32], {icon: greenIcon}).addTo(map);
```

[HTML](leaf2.html)

Üstteki işaretlere tıklanınca ortaya çıkan (popup) yazılar da
ekleyebilirdik, bunun için `L.marker` sonrası `.bindPopup("yazı").openPopup()`
çağrısı yeterli, bu çağrı yine bir işaretleyici objesi geri döndürüyor
böylece o obje üzerinde hala `addTo(map)` çağrısı yapabiliriz. Örnek,

[HTML](leaf3.html)

### Çizgiler, Poligonlar

Kordinat listesi vererek o noktaları birleştiren çizgiler çizebiliriz,

```javascript
path = [[40,31],[41,31],[41,30]];
var line = new L.Polyline(path, {
	     color: 'red', weight: 3, opacity: 0.5, smoothFactor: 1
});
line.addTo(map);
```

[HTML](leaf4.html)

Eğer verili noktalar bir poligon oluştursun istiyorsak leaflet bu
noktaları bir kapalı alan olarak işleyebilir, listedeki son nokta ilk
nokta ile birleştirilir, ve bizim verdiğimiz bir renk ile doldurulacak
şekilde bir poligon çizilir. Üstteki aynı noktaları kullanarak şunu
yapabilirdik,

```javascript
var poly = new L.Polygon(path, {
                   color: 'red', weight: 3, opacity: 0.5, smoothFactor: 1
});
poly.setStyle({fillColor: '#0000FF'});
poly.addTo(map);      
```

[HTML](leaf6.html)

### Tıklama Adresi

Haritada3 tıklanan yerdeki enlem/boylam kordinalarını almak için

```javascript
map.on('click', function(e){
  var coord = e.latlng;
  var latclick = coord.lat;
  var lngclick = coord.lng;
  console.log(latclick,lngclick);
});
```

### Boş Fayans

Arka planı yani fayans kısmını tamamen iptal edebiliriz, bunun faydası
ne olur diye soranlar olabilir, fayda şurada, arka plan olmasa da
sonradan eklenen çizgiler, noktalar hala büyütme, kaydırma kurallarına
tabi oluyor, yani tamamen kendimizin yarattığı istenen yerine
bakılabilen bir ağ yapısını burada çizebiliriz. Mesela ilk akla gelen
örnek, fayans iptal edilir, ve bir JSON dosyasından tüm dünya
kıtalarının sınırları alınıp çizmek, başka istenen ekler de yapılır,
böylece kendimizin sıfırdan oluşturduğu bir harita yaratmış oluruz.

Basit bir boş fayans örneği için CSS içine

```
.leaflet-container { background-color: #109DE3; }
```

Kod seviyesinde

```javascript
avar base = {
  'Empty': L.tileLayer(''),
    'OpenStreetMap': L.tileLayer('http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png', {
    'attribution': 'Map data &copy; OpenStreetMap contributors'
   })
};

var map = L.map('map', {
       'center': [40, 30],
       'zoom': 7,
       'layers': [
           base.Empty
        ]
 });

var control = L.control.layers(base).addTo(map);

path = [[40,31],[41,31],[41,30]];
var line = new L.Polyline(path, {
            color: 'red', weight: 3, opacity: 0.5, smoothFactor: 1
});
line.addTo(map);
```

[HTML](leaf5.html)

### Fayans Servisi

Kendi fayans servisimizi de yazabiliriz.  Leaflet'in arka plandaki
fayans servisi ile iletişimi direk, temiz bir yaklaşım, `tileLayer`
çağrısına geçilen parametreler haritanın belli bir parçasına nasıl
erişileceğini tarif ediyor, bu erişim basit dizin / dosya
üzerinden.. Makina ismi sonrası z,x,y parametreleri ile bir URL
oluşturuyor ve bu URL ile bir görüntü dosyası alınıyor (250x250
boyutunda), ki ünlü açık veri servisi OSM'nin zaten böyle bir servisi
var. Adresi https://tile.openstreetmap.org, dosya servisinin dizin
yapısında en üst dizinde büyüklük seviyesi, onun altındaki dizinde x
(boylam) dizinleri onun altında y (enlem) kordinatına tekabül eden
y.png dosyaları var. Bu bizi eğer mesela büyüklük seviyesi 4 boylam 11
enlem 7 ise bir

[https://tile.openstreetmap.org/4/11/7.png](https://tile.openstreetmap.org/4/11/7.png)

dosyasına eriştirecektir, mümkün her parametre kombinasyonu için bu
dosya servisinde imaj dosyaları vardır.

Fakat arka planda illa bir dosya servisi sart degil, erişim
parametrelerini bir servis kodu ile 'yakalayarak' kendi fayans
servisimizi kod ile sağlayabiliriz. Flask ya da herhangi bir REST
servisi ile makina / parametre1 / parametre2 / parametre3 gibi
erişimlerin parametrelerini okuyabileceğimizi biliyoruz. o zaman önce
leaflet'e kendi servis adresimizi veririz,


```javascript
L.tileLayer('http://localhost:5000/tiles/{z}/{x}/{y}.jpg',...
```

Ardından servis kodlarımızı yazarız,


```python
from flask import Flask, send_file
import os.path

app = Flask(__name__, static_url_path='/static')

@app.route('/tiles/<zoom>/<x>/<y>', methods=['GET', 'POST'])
def tiles(zoom, x, y):
    tile1 = os.getcwd() + '/static/tile1.jpg' 
    tile2 = os.getcwd() + '/static/tile2.jpg'
    print (tile1)
    y = y.replace(".jpg","")
    print ('zoom',zoom,'x',x,'y',y)
    m = int(x+y+zoom) % 2
    if m==0:
        return send_file(tile1)
    else:
        return send_file(tile2)
    
@app.route('/', methods=['GET', 'POST'])
def index():
    return app.send_static_file('index.html')

if __name__ == '__main__':
    app.run(debug=True, host='localhost', port=5000)
```

Kod icin gereken iki imaj altta,

[tile1.jpg](tile1.jpg),[tile2.jpg](tile2.jpg)

Bu dosyalar `index.html` ile bir `static` dizini altına yazılır,
servis için gereken `app.py` bir üstteki dizindedir, standard Flask
yapısı bu.

Başlatılınca kullanım herhangi bir leaflet haritası kullanır gibi,
görüntü olarak bazen daire bazen kare resimleri göreceğiz, hangi
resmin servis edildiği x,y,zoom parametreleri birleştirilip sayının
tek/çift olduğuna bakılarak yapılıyor, örnek amaçlı bir kod. Sayı tek
ise bir dosya, çift ise diğeri servis ediliyor. Leaflet'in
mekanizmasını anlamak açısından faydalı olabilir. Profosyonel bir
uygulama servise gönderilen x,y,z parametrelerini işleyerek bir veri
tabanından gerekli bir bilgiyi alıp görüntüyü yaratıp istemciye
verebilir, leaflet bu görüntüleri anında yapıştırıp akıcı bir harita
tecrübesi yaratabilir. Veri belki dağlar, belki nehirler, belki şehir
isimleridir, tüm mümkün görüntüleri önceden yaratmak yerine belki bu
şekilde veri bazlı bir işlem metotu daha uygundur.

Kaynaklar

[1] <a href="../../2020/02/haritalamak.html">Haritalamak</a>

[2] <a href="https://stackoverflow.com/questions/28094649/add-option-for-blank-tilelayer-in-leaflet-layergroup">Stackoverflow</a>

[3] <a href="https://nithanaroy.medium.com/create-your-own-tile-server-and-map-client-5f7515fff28">Medium</a>

