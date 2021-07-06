# Genel Coğrafi Kordinat Kodları, HTML5, Javascript, Python

Mesafe hesabi yapmak

Iki enlem, boylam kordinati arasinda mesafe hesabi icin geopy kullanilabilir.

```python
import geopy.distance
dist = geopy.distance.vincenty((51.238689, 4.406747),(51.232246, 4.444266))
print (dist)
print (dist.km)
```

İkinci ifade float tıpınde mesafeyi verir, kilometre bazlıdır.

Daha az dış kütüphanelere bağımlı bir kod istersek, alttaki kod faydalı,

```python
from scipy import sin, cos, tan, arctan, arctan2, arccos, pi

def spherical_distance(lat1, long1, lat2, long2):
    phi1 = 0.5*pi - lat1
    phi2 = 0.5*pi - lat2
    r = 0.5*(6378137 + 6356752) # mean radius in meters
    t = sin(phi1)*sin(phi2)*cos(long1-long2) + cos(phi1)*cos(phi2)
    return r * arccos(t) / 1000.
```

İki nokta arasında birinciden ikinciye olan açısal yön (bearing),

```python
def get_bearing(lat1,lon1,lat2,lon2):
    dLon = lon2 - lon1;
    y = math.sin(dLon) * math.cos(lat2);
    x = math.cos(lat1)*math.sin(lat2) - math.sin(lat1)*math.cos(lat2)*math.cos(dLon);
    brng = np.rad2deg(math.atan2(y, x));
    if brng < 0: brng+= 360
    return brng
```

Sonuç 0 derece kuzey olmak üzere 0-360 derece arasında saat yönüne
doğru artacak şekilde açı.

Bir kordinattan "10 km doguya, batiya, vs. adim atinca nereye
geliriz?" sorusunun cevabi icin

```python
import geopy
import geopy.distance
# baslangic noktasi
start = geopy.Point(48.853, 2.349)
# mesafe 1 km
d = geopy.distance.VincentyDistance(kilometers = 1)
# derece olarak adim atilacak yon, 0 derece kuzey, 90 dogu, ..
reached = d.destination(point=start, bearing=0)
print (reached.latitude)
print (reached.longitude)
```

Daha duz ve API'siz isleyen bir kod

```python
def to_bearing(lat,lon,brng,d):
    R = 6378.1 #Radius of the Earth
    lat1 = math.radians(lat)
    lon1 = math.radians(lon)
    lat2 = math.asin( math.sin(lat1)*math.cos(d/R) +
         math.cos(lat1)*math.sin(d/R)*math.cos(brng))
    lon2 = lon1 + math.atan2(math.sin(brng)*math.sin(d/R)*math.cos(lat1),
                 math.cos(d/R)-math.sin(lat1)*math.sin(lat2))
    lat2 = math.degrees(lat2)
    lon2 = math.degrees(lon2)
    return lat2,lon2
```

Bir GPS kordinat listesinin orta noktasını bulmak için,

```python
from shapely.geometry import Polygon
pts = [[51.238689, 4.406747],[51.232246, 4.444266],[51.251485,4.472641],[51.265894, 4.452429]]
p = Polygon(pts)
print (p.centroid.x)
print (p.centroid.y)
```

Üstteki `shapely` kullanımı yerine (bu paketin geos C bazlı
kütüphanesine bağlantısı var, ki bu paket her ortamda
derlenemeyebilir) pür Python bazlı kod gerekirse alttaki kullanışlı.

```python
def get_centroid(poly):
    """Calculates the centroid of a non-intersecting polygon.
    Args:
        poly: a list of points, each of which is a list of the form [x, y].
    Returns:
        the centroid of the polygon in the form [x, y].
    Raises:
        ValueError: if poly has less than 3 points or the points are not
                    formatted correctly.
    """
    # Make sure poly is formatted correctly
    if len(poly) < 3:
        raise ValueError('polygon has less than 3 points')
    for point in poly:
        if type(point) is not list or 2 != len(point):
            raise ValueError('point is not a list of length 2')
    # Calculate the centroid from the weighted average of the polygon's
    # constituent triangles
    area_total = 0
    centroid_total = [float(poly[0][0]), float(poly[0][1])]
    for i in range(0, len(poly) - 2):
        # Get points for triangle ABC
        a, b, c = poly[0], poly[i+1], poly[i+2]
        # Calculate the signed area of triangle ABC
        area = ((a[0] * (b[1] - c[1])) +
                (b[0] * (c[1] - a[1])) +
                (c[0] * (a[1] - b[1]))) / 2.0;
        # If the area is zero, the triangle's line segments are
        # colinear so we should skip it
        if 0 == area:
            continue
        # The centroid of the triangle ABC is the average of its three
        # vertices
        centroid = [(a[0] + b[0] + c[0]) / 3.0, (a[1] + b[1] + c[1]) / 3.0]
        # Add triangle ABC's area and centroid to the weighted average
        centroid_total[0] = ((area_total * centroid_total[0]) +
                             (area * centroid[0])) / (area_total + area)
        centroid_total[1] = ((area_total * centroid_total[1]) +
                             (area * centroid[1])) / (area_total + area)
        area_total += area
    return centroid_total
```

Bir alternatif daha su [bağlantıdan](https://www.navlab.net/nvector/#example_7),
enlem, boylam bir üç boyutlu vektör haline getiriliyor, ve Kartezyen bazlı bu
vektörlerin ortalaması doğru ortalamayı veriyor. Kodun temel aldığı makale [1].

```python
import numpy as np
import numpy.linalg as lin

E = np.array([[0, 0, 1],[0, 1, 0],[-1, 0, 0]])

def lat_long2n_E(latitude,longitude):
    res = [np.sin(np.deg2rad(latitude)),
           np.sin(np.deg2rad(longitude)) * np.cos(np.deg2rad(latitude)),
           -np.cos(np.deg2rad(longitude)) * np.cos(np.deg2rad(latitude))]
    return np.dot(E.T,np.array(res))

def n_E2lat_long(n_E):
    n_E = np.dot(E, n_E)
    longitude=np.arctan2(n_E[1],-n_E[2]);
    equatorial_component = np.sqrt(n_E[1]**2 + n_E[2]**2 );
    latitude=np.arctan2(n_E[0],equatorial_component);
    return np.rad2deg(latitude), np.rad2deg(longitude)

def average(coords):
    res = []
    for lat,lon in coords:
        res.append(lat_long2n_E(lat,lon))
    res = np.array(res)
    m = np.mean(res,axis=0)
    m = m / lin.norm(m)
    return n_E2lat_long(m)
        

n = lat_long2n_E(30,20)
print (n)
print (n_E2lat_long(np.array(n)))

# fransa ve libya ortasi
coords = [[30,20],[47,3]]
m = average(coords)
print (m)
```

HTML5 ve Javascript ile Yer Bulmak

Javascript icinden yer bulmak mumkun, bu cep telefonunda da isliyor,
Google'in Wifi, Telekom, GPS uzerinden yer bulan arayuzu ile
baglantili zannederim. Kalitesini kontrol etmedim, ama alttaki kod
isler ve yer rapor eder. Isletince tarayici 'yer bilgisine erisim'
icin izin isteyecek. Izin verince (allow), bilgi sayfada basilacak ve
kullanim ornegi olsun diye bir de URL baglantilardan birine parametre
olarak eklenecek.

```
<html>
  <script>
    var lat = "lat";
    var lon = "lon";
    function getLocation() {
       navigator.geolocation.getCurrentPosition(setPosition);
    }
    function setPosition(position) {
      lat = position.coords.latitude;
      lon = position.coords.longitude;
      document.getElementById("locpos").innerHTML = lat + " " + lon;
      document.getElementById("url1").href="/bir/baglanti/" + lat + ";" + lon;
    }
</script>
<body onload="init()">

  <div class="navmenu">
    <nav>
      <div id="locpos">
        <p></p>
      </div>
      <ul>
        <li><a id="grab" href="#" onclick='getLocation()'>Yer Bul</a></li>
        <li><a id="url1" href="/bir/baglanti/32324">Baglanti</a></li>
        <li><a id="vsvs" href="">Vs..</a></li>
      </ul>
    </nav>
  </div>

</html>
```

Kaynaklar

[1] Kenneth Gade (2010), A Non-singular Horizontal Position Representation,
    The Journal of Navigation, Volume 63, Issue 03, pp 395-417, July 2010.

