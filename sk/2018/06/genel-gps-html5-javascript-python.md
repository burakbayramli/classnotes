# Genel GPS: HTML5, Javascript, Python, Kodlari, Tavsiyeleri


Genel GPS: HTML5, Javascript, Python, Kodlari, Tavsiyeleri




Mesafe hesabi yapmak

Iki enlem, boylam kordinati arasinda mesafe hesabi icin geopy kullanilabilir.

import geopy.distance
dist = geopy.distance.vincenty((51.238689, 4.406747),(51.232246, 4.444266))
print (dist)
print (dist.km)

İkinci ifade float tıpınde mesafeyi verir, kilometre bazlıdır.

İki nokta arasında birinciden ikinciye olan açısal yön (bearing),

def get_bearing(lat1,lon1,lat2,lon2):
    dLon = lon2 - lon1;
    y = math.sin(dLon) * math.cos(lat2);
    x = math.cos(lat1)*math.sin(lat2) - math.sin(lat1)*math.cos(lat2)*math.cos(dLon);
    brng = np.rad2deg(math.atan2(y, x));
    if brng < 0: brng+= 360
    return brng



Sonuç 0 derece kuzey olmak üzere 0-360 derece arasında saat yönüne doğru artacak şekilde açı.

Bir kordinattan "10 km doguya, batiya, vs. adim atinca nereye geliriz?" sorusunun cevabi icin

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

Bir GPS kordinat listesinin orta noktasini bulmak icin noktalari toplayip, bolmek yerine, ozel paket kullanmak daha iyi;

from shapely.geometry import Polygon
pts = [[51.238689, 4.406747],[51.232246, 4.444266],[51.251485,4.472641],[51.265894, 4.452429]]
p = Polygon(pts)
print (p.centroid.x)
print (p.centroid.y)

Ustteki shapely kullanimi yerine (bu paketin geos C bazli kutuphanesine baglantisi var, ki bu paket her ortamda -mesela Termux- derlenemeyebilir) pur Python bazli kod gerekirse alttaki kullanisli.

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

Bu kod daha once verilmisti ama tekrar daha temiz verelim. Google Maps'den belli bolgelere arasina dusen tum harita parcalarini, imaj olarak, istenen buyuklukte almak,

import itertools, time
import pandas as pd, time
import numpy as np
import matplotlib.pyplot as plt
from math import sin, cos, sqrt, atan2, radians
from io import BytesIO
from PIL import Image
import urllib.request, os.path

def get_map(lat, lon, region, zoom):
    api = open("[GOOGLE MAPS ANAHTAR DEGERININ OLDUGU DOSYA ISMI BURAYA").read()
    url = "http://maps.googleapis.com/maps/api/staticmap?center=" + \
       "%f,%f&scale=2&size=800x800&maptype='terrain'&zoom=%d&key=%s" % (lat,lon,zoom,api)
    print (url)
    lats = str(lat).replace(".","_")
    lons = str(lon).replace(".","_")
    fout = "%s/%s_map_%s_%s.png" % (region,region,lats,lons)
    if os.path.isfile(fout):
        print ("Already downloaded...")
        return False
    buffer = BytesIO(urllib.request.urlopen(url).read())
    image = Image.open(buffer)
    image.save(fout)
    return True
    
def get_maps(c1,c2,px,py,region,zoom=11):
    """
    c1: one corner of the region box
    c2: the opposite corner of the region box

    get_maps will always pick the smallest / largest of each
    coord and create a box to sweep over. 
    """
    a= np.linspace(min(c1[0],c2[0]), max(c1[0],c2[0]), px)
    b= np.linspace(min(c1[1],c2[1]), max(c1[1],c2[1]), py)
    aa,bb = np.meshgrid(a,b)
    for x,y in zip(aa.flatten(),bb.flatten()):
        if get_map(x,y,region,zoom) == False: continue


if __name__ == "__main__":
    # harita hangi kordinatlar arasinda olmali
    c1 = (51.450320,2.963884); c2 = (39.460801, 29.786351)
    get_maps(c1, c2, 80, 80, region="europe2")


Imaj dosyalari bir alt dizin region icinde yaziliyor, bu dizin yaratilmis olmali.

Kullanmak icin ustteki dizini zip dosyasi haline getiririz, ve o veri tabani haline gelir. Sonra o zip dosyasini zfile parametresi olarak veririz, ve

import geopy.distance
import pandas as pd, io
from PIL import Image
import os, glob, re, zipfile
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

# enlem/boylam ve pikseller arasinda gecis icin,
# her zoom seviyesi icin deneme/yanilma ile kendimiz bulduk
SCALEX = 2900. 
SCALEY = -4700.

def plot(points,outfile,zfile):
    """
    Birinci noktayi baz alarak gerekli harita inajini bul, ve diger
    tum noktalari bu harita uzerinde grafikle
    """
    plt.figure()
    center_res = points[0]
    imgcoord = []
    with zipfile.ZipFile(zfile, 'r') as z:
        for f in z.namelist():
            # the lat/lon middle of the map is encoded in the map's
            # filename
            tmp = re.findall("map_(\d+)_(\d+)_(\d+)_(\d+)",f,re.DOTALL)
            if len(tmp)==0: continue
            tmp = tmp[0]
            imgcoord.append([float(tmp[0] + "." + tmp[1]), float(tmp[2] + "." + tmp[3]), f])
    imgcoord2 = pd.DataFrame(imgcoord,columns=['lat','lon','file'])
    dists = imgcoord2.apply(lambda x: geopy.distance.vincenty((x['lat'],x['lon']),center_res).km, axis=1)
    # the closest map is picked
    found = imgcoord2.ix[dists.idxmin()]
    print (found.file)
    mapcenter = np.array(found[['lat','lon']])
    print (mapcenter)
    
    with zipfile.ZipFile(zfile, 'r') as z:
         im = Image.open(z.open(found.file))
         nim = np.array(im)
         c = nim.shape[0] / 2, nim.shape[0] / 2
         plt.axis('off')
         fig=plt.imshow(im)
         fig.axes.get_xaxis().set_visible(False)
         fig.axes.get_yaxis().set_visible(False)
         plt.imshow(im)
         for i,[lat,lon] in enumerate(points):
             dx,dy=((lon-mapcenter[1])*SCALEX,(lat-mapcenter[0])*SCALEY)             
             xx = c[0]+dx
             yy = c[1]+dy
             if xx > nim.shape[0] or yy > nim.shape[1] or xx<0 continue="" font="" or="" yy="">0>
             if i==0:
                 plt.plot(xx,yy,'rx')
             else:
                 plt.plot(xx,yy,'r.')
         plt.savefig(outfile, bbox_inches='tight', pad_inches = 0, dpi = 300)
Bahsedilen zip dosyalarin ornekleri icin su alt projemize bakilabilir. Ustteki kodlarin en son hali de bu projede olacaktir,

HTML5 ve Javascript ile Yer Bulmak

Javascript icinden yer bulmak mumkun, bu cep telefonunda da isliyor, Google'in Wifi, Telekom, GPS uzerinden yer bulan arayuzu ile baglantili zannederim. Kalitesini kontrol etmedim, ama alttaki kod isler ve yer rapor eder. Isletince tarayici 'yer bilgisine erisim' icin izin isteyecek. Izin verince (allow), bilgi sayfada basilacak ve kullanim ornegi olsun diye bir de URL baglantilardan birine parametre olarak eklenecek.

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






