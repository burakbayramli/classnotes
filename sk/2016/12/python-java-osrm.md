# Python, Java, OSRM

OSRM harita servisi JSON ile bilgi gonderir; bu bilgiyi dekode etmek
icin Python ve Java araclari var. Mesela bir yerden bir yere yol
tarifi almak icin neler gerekir onlari gorelim.

Java

```java
private static StringBuffer encodeSignedNumber(int num) {
    int sgn_num = num << 1;
    if (num < 0) {
        sgn_num = ~(sgn_num);
    }
    return(encodeNumber(sgn_num));
}

private static StringBuffer encodeNumber(int num) {
    StringBuffer encodeString = new StringBuffer();
    while (num >= 0x20) {
        int nextValue = (0x20 | (num & 0x1f)) + 63;
        encodeString.append((char)(nextValue));
        num >>= 5;
    }
    num += 63;
    encodeString.append((char)(num));
    return encodeString;
}

public static String encode(ArrayList<GeoPoint> polyline, int precision) {
    StringBuilder encodedPoints = new StringBuilder();
    int prev_lat = 0, prev_lng = 0;
    for (GeoPoint trackpoint:polyline) {
        int lat = trackpoint.getLatitudeE6() / precision;
        int lng = trackpoint.getLongitudeE6() / precision;
        encodedPoints.append(encodeSignedNumber(lat - prev_lat));
        encodedPoints.append(encodeSignedNumber(lng - prev_lng));
        prev_lat = lat;
        prev_lng = lng;
    }
    return encodedPoints.toString();

}

public static ArrayList<String []> readOsrmUrl(String urlString) {
    ArrayList<String []> res = new ArrayList<String []>();
    try {
        URL url = new URL(urlString);
        BufferedReader in = new BufferedReader(new InputStreamReader(url.openStream()));
        String line = "";
        while ((line = in.readLine()) != null) {
            String regex =
                "bearing_after.*?:(\\d+),.*?" +
                "\\[(-*\\d+\\.\\d+,-*\\d+\\.\\d+)\\].*?" +
                "distance.\\:(.*?),.*?" +
                "name.\\:(.*?)," ;
            Pattern r = Pattern.compile(regex);
            Matcher m = r.matcher(line);
            while (m.find( )) {
                String [] tmp = new String[4];
                tmp[0] = m.group(1);
                tmp[1] = m.group(2);
                tmp[2] = m.group(3);
                tmp[3] = m.group(4);
                res.add(tmp);
            }
        }

    } catch (IOException e) {
        e.printStackTrace();
    }
    return res;
}

public static ArrayList<String []> shortestPath(double lat1, double lon1, double lat2, double lon2) {

    GeoPoint p1 = new GeoPoint(lat1, lon1);
    GeoPoint p2 = new GeoPoint(lat2, lon2);
    ArrayList<GeoPoint> l = new ArrayList<GeoPoint>();
    l.add(p1);
    l.add(p2);
    String geo = encode(l, 10);
    String ourl = "http://router.project-osrm.org/route/v1/foot/polyline("+ geo +
        ")?overview=simplified&steps=true&alternatives=false&geometries=polyline";
    ArrayList<String []> r = readOsrmUrl(ourl);

    return r;
}
```

Bu kodun kullandigi yardimci fonksiyonlar alttaki baglantida bulunabilir.

https://github.com/burakbayramli/kod/tree/master/mycamera2

Python

Once,

apt-get install binutils libproj-dev gdal-bin python-gdal

sudo pip install geopandas polyline Fiona 

Su alttaki GH adresindeki Python kodlari fena degil, fakat tek script icinden pat diye isletebilmek icin biraz basitlestirme gerekti. Bu kod direk localhost:5000 uzerinde isleyen servise baglanir ve bilgiyi alir.

https://github.com/ustroetz/python-osrm

Alttaki kod yol bulma (route)  icin yeterli

```python
import numpy as np
from pandas import DataFrame
from urllib2 import urlopen
from polyline import encode as polyline_encode
from ogr import Geometry
from polyline.codec import PolylineCodec
import json
def _chain(*lists):
    for li in lists:
        for elem in li: yield elem
class DefaultRequestConfig:
    def __init__(self):
        self.host = "http://localhost:5000"
        self.profile = "driving"
        self.version = "v1"
    def __str__(self):
        return("/".join([self.host, '*', self.version, self.profile]))
    def __repr__(self):
        return("/".join([self.host, '*', self.version, self.profile]))
    @staticmethod
    def __call__(addr=None):
        if not addr:
            return DefaultRequestConfig()
        else:
            tmp = addr.split('/')
            cla = DefaultRequestConfig()
            cla.host = tmp[0]
            i = len(tmp)
            cla.version = tmp[i-2]
            cla.profile = tmp[i-1]
            return cla
RequestConfig = DefaultRequestConfig()
def check_host(host):
    """ Helper function to get the hostname in desired format """
    if not ('http' in host and '//' in host) and host[len(host)-1] == '/':
        return ''.join(['http://', host[:len(host)-1]])
    elif not ('http' in host and '//' in host):
        return ''.join(['http://', host])
    elif host[len(host)-1] == '/':
        return host[:len(host)-1]
    else:
        return host
def decode_geom(encoded_polyline):
    ma_ligne = Geometry(2)
    lineAddPts = ma_ligne.AddPoint_2D
    for coord in PolylineCodec().decode(encoded_polyline):
        lineAddPts(coord[1], coord[0])
    return ma_ligne
def simple_route(coord_origin_old, coord_dest_old, coord_intermediate=None,
                 alternatives=False, steps=False, output="full",
                 geometry='polyline', overview="simplified",
                 url_config=RequestConfig, send_as_polyline=True):
    if geometry.lower() not in ('wkt', 'well-known-text', 'text', 'polyline',
                                'wkb', 'well-known-binary', 'geojson'):
        raise ValueError("Invalid output format")
    else:
        geom_request = "geojson" if "geojson" in geometry.lower() \
            else "polyline"
    coord_origin = tuple(reversed(coord_origin_old))
    coord_dest = tuple(reversed(coord_dest_old))
    host = check_host(url_config.host)
    if not send_as_polyline:
        url = [host, "/route/", url_config.version, "/", url_config.profile,
               "/", "{},{}".format(coord_origin[0], coord_origin[1]), ';']
        if coord_intermediate:
            url.append(";".join(
                [','.join([str(i), str(j)]) for i, j in coord_intermediate]))
        url.extend([
            '{},{}'.format(coord_dest[0], coord_dest[1]),
            "?overview={}&steps={}&alternatives={}&geometries={}".format(
                 overview, str(steps).lower(),
                 str(alternatives).lower(), geom_request)
            ])
    else:
        coords = [
            pt[::-1] for pt in _chain(
                        [coord_origin],
                        coord_intermediate if coord_intermediate else [],
                        [coord_dest])
            ]
        url = [
            host, "/route/", url_config.version, "/", url_config.profile, "/",
            "polyline(", polyline_encode(coords), ")",
            "?overview={}&steps={}&alternatives={}&geometries={}".format(
                 overview, str(steps).lower(),
                 str(alternatives).lower(), geom_request)
            ]
    rep = urlopen(''.join(url))
    parsed_json = json.loads(rep.read().decode('utf-8'))
    if "Ok" in parsed_json['code']:
        if geometry in ("polyline", "geojson") and output == "full":
            return parsed_json
        elif geometry in ("polyline", "geojson") and output == "routes":
            return parsed_json["routes"]
        else:
            if geometry == "wkb":
                func = Geometry.ExportToWkb
            elif geometry == "wkt":
                func = Geometry.ExportToWkt
            for route in parsed_json["routes"]:
                route["geometry"] = func(decode_geom(route["geometry"]))
        return parsed_json if output == "full" else parsed_json["routes"]
    else:
        raise ValueError(
            'Error - OSRM status : {} \n Full json reponse : {}'.format(
                parsed_json['code'], parsed_json))
# test
result1 = simple_route((40.987659,29.036428), (40.992186,29.039228),
                       output="routes",
                       geometry="wkt",
                       send_as_polyline=True, steps=True)
print result1
```

Bu arada ciktilarda enlem, boylam degerleri yer degisikligine ugramis,
OSRM her nedense boyle yapiyor, biz sadece girdide sirayi degistirdik.







