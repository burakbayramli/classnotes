# IP ve Bölge Eşlemesi, Python

IP adresini yaklasik olarak bir bolgeye, sehre, hatta enlem boylam
kordinatina eslemek icin pygeoip adli Python paketi var. Kurmak icin

```
sudo easy_install pygeoip
```

Kodun ek veri dosyalarina ihtiyaci var. Bu dosyalar

```
http://dev.maxmind.com/geoip/geolite#IP_Geolocation-1
```

adresinde Downloads bolumunden indirilebilir, GeoIP.dat.gz,
GeoIPv6.dat.gz, GeoLiteCity.dat.gz dosyalari mesela. Ornek kod, mesela
212.174.157.30 ip adresi icin (adresi nslookup www.tbmm.gov.tr ile
bulduk)

```
import pygeoipgi = pygeoip.GeoIP('[DIZIN]/GeoLiteCity.dat')
print gi.record_by_addr('212.174.157.30')
```

Sonuc

```
{'city': u'B\xfcy\xfck', 'region_name': u'61', 'area_code': 0,
'time_zone': 'Asia/Istanbul', 'dma_code': 0, 'metro_code': None,
'country_code3': 'TUR', 'latitude': 40.66669999999999, 'postal_code':
'', 'longitude': 40.400000000000006, 'country_code': 'TR',
'country_name': 'Turkey', 'continent': 'EU'}
```

Sonuc Istanbul (sitenin barindirma servisi degisik bir sehirde demek
ki). Diger ornekler

https://github.com/appliedsec/pygeoip

Eger mesela Pandas DataFrame objesi uzerindeki bir IP adresini
isleyip, hesaplanan enlem / boylam degerlerini ayni satira ayri
kolonlar olarak yazmak isteseydiniz,

```python
import pygeoip, os
import pandas as pd
gi = pygeoip.GeoIP('[DIZIN]/GeoLiteCity.dat')
def ip_loc(x):
    rec = gi.record_by_addr(x)
    lat = rec['latitude']
    lon = rec['longitude']
    return pd.Series([lat, lon], index=['latitude','longitude'])

df = pd.DataFrame({'ip': ['212.174.157.30','212.174.157.30','212.174.157.30']})
df[['latitude','longitude']] = df['ip'].apply(ip_loc)
print df
```

Buradan gelen sonuc

```
ip  latitude  longitude0  212.174.157.30   40.6667       40.41 
212.174.157.30   40.6667       40.42  212.174.157.30   40.6667      
40.4
```



