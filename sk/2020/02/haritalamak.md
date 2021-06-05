# Haritalamak

Folium

Bu haritalama sistemi ile dünyanın herhangi bir yerini alıp, istenen
odaklama seviyesinde göstermek, üzerini işaretlemek mümkün, ayrıca
haritanın üzerinde isteğe göre çizgiler, çemberler vs çizilmesine izin
veren çağrılar var. Folium arka planda bir Javascript kütüphanesi
Leaflet kullanır, bu sebeple haritaları HTML olarak kaydediyorsunuz,
bu HTML içine bakıldığında Leaflet çağrıları orada görülüyor.  Leaflet
son derece basit, hızlı işleyecek şekilde yazılmış, verisini arka
planda direk OpenStreetMap dosyalarını okuyarak alıyor, onların
gerekli yerlerini, hangisini alacağını arka planda
hallediyor. Programcıya tek kalan harita merkezini, ek bazı görsel
ayarları yapmak.

Not: Elde edilen HTML haritası etkileşimli (interactive) çalışıyor.
Büyütme, küçültme, haritayı sağa sola kaydırma bu HTML sayfasında
kullanıcı tarafından yapılabilir hala. Yani tek ölü bir görüntü dosyası
almıyorsunuz.

Kurmak icin `pip install folium`

```python
import folium

lat1,lon1 = 43.4521793912598, 25.597125946810632
lat2,lon2 = 44.572754823623114, 27.250000972553345
m = folium.Map(location=[lat1,lon1], tiles='Stamen Terrain', zoom_start=7)
coordinates=[(lat1,lon1),(lat2,lon2)]
folium.PolyLine(locations=coordinates,weight=2,color = 'blue').add_to(m)
folium.CircleMarker(location=(lat2,lon2),
                            fill_color='blue', radius=10, rotation=1.5).add_to(m)

title_html = "<h3>Title</h3>"
m.get_root().html.add_child(folium.Element(title_html))
m.save("index.html")

```

![](har7.jpg)

`lat1,lon1` merkezli bir harita çizdik, odak seviyesi 7, ve `lat1,lon1` den
başlayıp `lat2,lon2` de biten bir çizgi çektik, ve çizgi sonuna ufak bir daire
koyduk.

İşaretler

Harita üzerine konan işaretler içine tıklanabilir URL bağlantıları konabilir.
Ayrıca bu işaretler fare ile üzerinde durunca bir araç ipucu (tooltip)
gösterebilir.

```python
m = folium.Map(location=[45.372, -121.6972], zoom_start=12, tiles="Stamen Terrain")
tooltip = "Click me!"
folium.Marker(
    [45.3288, -121.6625], popup="<i>Mt. Hood Meadows</i>", tooltip=tooltip
).add_to(m)
```

Üstteki kodda görülen `popup` içine `a href` ifadesi de konulabilir,
böylece fare üstünde olunca önce ipucu, ona tıklanınca çıkan bölgede
tıklanabilir bir URL bağlantısı olacaktır. Biz ayrıca bu bağlantıyı
`target='_blank' rel='noopener noreferrer'` ile yarattık, böylece
tıklama bizi tarayıcıda ayrı bir tab'e götürüyor.


Ek bilgiler [5],[6],[7] de bulunabilir,

Cartopy

Kullanisli bir paket; fakat isletim sistemi seviyesinde GEOS paketinin
kurulmasi gerekli, bu bazi barindirilan, idareli bulut sistemlerinde
problem cikartabilir, cunku bu tur servislerde direk isletim sistemine
erisim her zaman mumkun olmayabiliyor.

```
sudo apt install libproj-dev proj-data proj-bin libgeos-dev

pip install cartopy

pip uninstall shapely

pip install shapely --no-binary shapely
```

Dünya haritası üzerinde bir nokta

```python
import cartopy.crs as ccrs
import cartopy
fig = plt.figure(figsize=(5, 2))
ax = fig.add_subplot(1, 1, 1, projection=ccrs.PlateCarree())
ax.set_global()
ax.stock_img()
ax.coastlines()
ax.plot(42.0, 21.53, 'ro', transform=ccrs.PlateCarree())

plt.savefig('har1.png')
```

![](har1.png)

Eger daha yakindan gostermek istersek, ilgilendigimiz bolge uzerinden
bir kutu yaratabiliriz,

```python
import cartopy.crs as ccrs
import cartopy

fig = plt.figure()
ax = fig.add_subplot(1, 1, 1, projection=ccrs.PlateCarree())
ax.set_global()
ax.stock_img()
ax.coastlines()
ax.plot(42.0, 21.53, 'ro', transform=ccrs.PlateCarree())
ax.set_extent([30, 50, 10, 30])
plt.savefig('har3.png')
```

![](har3.png)


Ülke ismi bazlı tüm ülkeyi renklendirmek,

```python
import matplotlib.pyplot as plt
import cartopy.io.shapereader as shpreader
import cartopy.crs as ccrs
import cartopy.feature as cfeature

def area(ax, iso, clr) :
    shp = shpreader.natural_earth(resolution='10m',category='cultural',
                                  name='admin_0_countries')
    reader = shpreader.Reader(shp)
    for n in reader.records() :
        if n.attributes['ADM0_A3'] == iso: 
            ax.add_geometries(n.geometry, ccrs.PlateCarree(), facecolor=clr) 
    return ax

fig = plt.figure(figsize=(5, 2))
ax = fig.add_subplot(1, 1, 1, projection=ccrs.PlateCarree())
ax = plt.axes(projection=ccrs.PlateCarree())
ax.stock_img()
ax.coastlines()
area(ax, "USA", "red")
plt.savefig('har2.png')
```

![](har2.png)

Bir kordinat etrafında belli büyüklükte bir alan üzerinde ızgara
yaratmak ve bu izgaradaki kordinat noktalarını haritada göstermek için
ne yaparız? Paket `geopy` kurulmuş olmalı,


```
import geopy.distance
import numpy as np

def goto_from_coord(start, distance, bearing):
    s = geopy.Point(start[0],start[1])
    d = geopy.distance.geodesic(kilometers = distance)
    reached = d.destination(point=s, bearing=bearing)
    return [reached.latitude, reached.longitude]

lat,lon=40.84343206497589, 29.926342357515754

dist = 60
res1 = goto_from_coord((lat,lon),dist,45)
print (res1)
res2 = goto_from_coord((lat,lon),dist,225)
print (res2)

lowlat = np.min([res1[0],res2[0]])
lowlon = np.min([res1[1],res2[1]])
hilat = np.max([res1[0],res2[0]])
hilon = np.max([res1[1],res2[1]])

x = np.linspace(lowlon,hilon,7)
y = np.linspace(lowlat,hilat,7)

xx,yy = np.meshgrid(x,y)
print (xx.flatten())

import cartopy.crs as ccrs
import cartopy

fig = plt.figure()
ax = fig.add_subplot(1, 1, 1, projection=ccrs.PlateCarree())
ax.set_global()
ax.stock_img()
ax.coastlines()
ax.plot(xx.flatten(), yy.flatten(), 'r.', transform=ccrs.PlateCarree())
ax.set_extent([28, 31, 40, 42])
plt.savefig('har4.png')
```

![](har4.png)

Nasıl kodladığımızı açıklayalım, önce merkez noktadan sağ üste (45
derece yönde) 60 km, ve sol alt yönde (225 derece) aynı uzaklıkta
birer adım atıyoruz, bu dörtgenin iki ucu. Bu uç noktalardan `lingrid`
sonra `meshgrid` ile ızgarayı oluşturuyoruz. 

Cartopy haritaları üzerinde klasik matplotlib komutlarını hala
kullanabiliriz, enlem, boylam kordinatları y ve x eksenleri haline
geliyor, ve gerisi bildiğimiz gibi. Mesela ok çizmek için kullanılan
`quiver` hala geçerli, mesela o ızgara noktalarındaki rüzgar yönü
verisi elimizde olsaydı, bunu harita üzerinde
grafikleyebilirdik. Altta bu veri `sin` ve `cos` üzerinden uyduruk
şekilde yaratıldı,

```python
fig = plt.figure()
ax = fig.add_subplot(1, 1, 1, projection=ccrs.PlateCarree())
ax.set_global()
ax.stock_img()
ax.coastlines()
u = np.sin(xx.flatten()*2)
v = np.cos(yy.flatten()*3)
ax.quiver(xx.flatten(), yy.flatten(), u, v)
ax.set_extent([28, 31, 40, 42])
plt.savefig('har5.png')
```

![](har5.png)

Eğer ana enlem, boylamları kordinate eksenleri üzerinde görmek
istersek, basmadan önce

```
gl = ax.gridlines(crs=ccrs.PlateCarree(), draw_labels=True, linewidth=0)
```

uygulayabiliriz.

Katmanlar

Üstteki karayı ve denizleri gösteren renklendirme sonradan uygulanan
bir katmandır. Cartopy bu şekilde pek çok farklı bilgiyi katman olarak
haritalara uygulayabilir. Mesela yol, sehir bilgileri de birer katman
olarak alttaki gibi alınabilir,

```python
import cartopy.crs as ccrs
import cartopy
import cartopy.io.img_tiles
lat,lon=40.84343206497589, 29.926342357515754
fig = plt.figure()
imagery = cartopy.io.img_tiles.OSM()
ax = fig.add_subplot(projection=imagery.crs)
zoom = 10
ax.add_image(imagery, zoom)
ax.plot(lon, lat, 'ro', transform=ccrs.PlateCarree())
EXT = 0.1
ax.set_extent([lon-EXT, lon+EXT, lat-EXT, lat+EXT])
gl = ax.gridlines(crs=ccrs.PlateCarree(), draw_labels=True, linewidth=0)
plt.savefig('har6.png')
```

![](har6.png)

Kaynaklar

[1] https://rabernat.github.io/research_computing_2018/maps-with-cartopy.html

[2] https://scitools.org.uk/cartopy/docs/latest/gallery/global_map.html#sphx-glr-gallery-global-map-py

[3] https://github.com/SciTools/cartopy/issues/1303

[4] Renk isimleri - https://matplotlib.org/3.1.0/gallery/color/named_colors.html

[5] https://python-visualization.github.io/folium/quickstart.html

[6] https://stackoverflow.com/questions/39093250/is-it-possible-to-draw-lines-with-arrowheads-in-a-folium-map

[7] https://www.geeksforgeeks.org/stamen-toner-stamen-terrain-and-mapbox-bright-maps-in-python-folium/


