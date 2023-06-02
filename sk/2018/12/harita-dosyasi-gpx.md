# Yol Dosyaları, GPX Formatı

Wikiloc gibi servisler yol tariflerini çoğunlukla GPX formatında
paylaşırlar. Paket `gpxpy` ile bu dosyaları işleyebiliriz. Bu yazıyla
aynı dizinde `yol1.gpx` dosyası var, içeriği,

```python
from bs4 import BeautifulSoup
bs = BeautifulSoup(open("yol1.gpx"), 'xml')
pretty_xml = bs.prettify()
print(pretty_xml[:900])
```

```text
<?xml version="1.0" encoding="utf-8"?>
<gpx creator="Wikiloc - https://www.wikiloc.com" version="1.1" xmlns="http://www.topografix.com/GPX/1/1" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://www.topografix.com/GPX/1/1 http://www.topografix.com/GPX/1/1/gpx.xsd">
 <metadata>
  <name>
   Wikiloc - Harita1
  </name>
  <author>
   <name>
    Kullanici1
   </name>
   <link href="https://www.wikiloc.com/wikiloc/user.do?id=2088213">
    <text>
     Kullanici1 on Wikiloc
    </text>
   </link>
  </author>
  <link href="https://www.wikiloc.com/hiking-trails/falan-filan">
   <text>
    Yer1 on Wikiloc
   </text>
  </link>
  <time>
   2016-09-21T19:40:58Z
  </time>
 </metadata>
 <trk>
  <name>
   Yer1
  </name>
  <cmt/>
  <desc/>
  <trkseg>
   <trkpt lat="40.964344" lon="41.010343">
    <ele>
     1983.363
    </ele>
    <time>
     2022-08-10T00:09:01Z
    </time>

```

Kordinat listesini bu dosyadan şöyle çıkartabiliriz,

```python
import gpxpy, gpxpy.gpx
gpx = gpxpy.parse(open("yol1.gpx").read())
points = []
for track in gpx.tracks:
    for segment in track.segments:
        for point in segment.points:
            lat,lon = point.latitude, point.longitude
            points.append([lat,lon])
print (len(points))
print (points[:4])
```

```text
170
[[40.964344, 41.010343], [40.964344, 41.010344], [40.964344, 41.010344], [40.964325, 41.010299]]
```

Yeni bir GPX dosyası sıfırdan yaratmak için,

```python
gpx = gpxpy.gpx.GPX()

gpx_track = gpxpy.gpx.GPXTrack()
gpx.tracks.append(gpx_track)

gpx_segment = gpxpy.gpx.GPXTrackSegment()
gpx_track.segments.append(gpx_segment)

gpx_segment.points.append(gpxpy.gpx.GPXTrackPoint(40.964344, 41.010343, elevation=1234))
gpx_segment.points.append(gpxpy.gpx.GPXTrackPoint(40.964344, 41.010344, elevation=1235))
gpx_segment.points.append(gpxpy.gpx.GPXTrackPoint(40.964325, 41.010299, elevation=1236))

print('GPX:', gpx.to_xml())
```

```text
Created GPX: <?xml version="1.0" encoding="UTF-8"?>
<gpx xmlns="http://www.topografix.com/GPX/1/1" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://www.topografix.com/GPX/1/1 http://www.topografix.com/GPX/1/1/gpx.xsd" version="1.1" creator="gpx.py -- https://github.com/tkrajina/gpxpy">
  <trk>
    <trkseg>
      <trkpt lat="40.964344" lon="41.010343">
        <ele>1234</ele>
      </trkpt>
      <trkpt lat="40.964344" lon="41.010344">
        <ele>1235</ele>
      </trkpt>
      <trkpt lat="40.964325" lon="41.010299">
        <ele>1236</ele>
      </trkpt>
    </trkseg>
  </trk>
</gpx>
```

Eğer birden fazla yol parçası (track) var ise, bunları yeni 

```
... = gpxpy.gpx.GPXTrack()
gpx.tracks.append(..)
```

çağrıları ile ekleyebiliriz. Eğer birden fazla yol parçasını geziyorsak, üstteki

```
for track in gpx.tracks:
    for segment in track.segments:
       ..
```

döngüdeki dış döngü tüm yol parçalarını gezecektir. 

Kaynaklar

[1] <a href="https://pypi.org/project/gpxpy/">PyPI</a>



