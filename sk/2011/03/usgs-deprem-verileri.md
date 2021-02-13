# USGS Deprem Verileri

## quakefeeds

En rahat yontem `quakefeeds` adli paketi kullanmak,

```python
import pandas as pd, datetime

from quakefeeds import QuakeFeed
feed = QuakeFeed("4.5", "month")
res = []
for i in range(len(feed)):
    d = datetime.datetime.fromtimestamp(feed[i]['properties']['time']/1000.0)
    s = feed[i]['properties']['mag']
    res.append([d,s])

df = pd.DataFrame(res).sort_values(by=0)
df = df.set_index(0)
df.columns = ['Magnitude']
print (df.tail(5))
```

```text
                         Magnitude
0                                 
2021-02-13 07:30:56.150        5.2
2021-02-13 08:09:12.552        4.9
2021-02-13 08:15:21.905        4.9
2021-02-13 08:18:33.479        5.3
2021-02-13 08:19:51.545        4.5
```

Zamna göre sıralınmış, son bir ay içindeki 4.5 ölçeğinden büyükteki depremleri aldık.

## USGS, Json

Aslında USGS'in Web üzerinden JSON döndüren bir hizmeti de var. Daha fazla esneklik
isteyenler bu paketi kullanabilir.

```python
import requests, time

import datetime
today = datetime.datetime.now()
start = today - datetime.timedelta(days=40)

usgs_request_url = 'https://earthquake.usgs.gov/fdsnws'
usgs_request_url+='/event/1/query.geojson?starttime=%s&endtime=%s'
usgs_request_url+='&minmagnitude=4.5&orderby=time&limit=1000'
usgs_request_url = usgs_request_url % (start.isoformat(), today.isoformat())
qres = requests.get(usgs_request_url).json()
res = []
for i in range(len(qres['features'])):
    d = datetime.datetime.fromtimestamp(qres['features'][i]['properties']['time']/1000.0)
    s = qres['features'][i]['properties']['mag']
    res.append([d,s])

import pandas as pd
df = pd.DataFrame(res).sort_values(by=0)
df = df.set_index(0)
df.columns = ['Magnitude']
print (df.tail(5))
```

```text
                         Magnitude
0                                 
2021-02-13 07:30:56.150        5.2
2021-02-13 08:09:12.552        4.9
2021-02-13 08:15:21.905        4.9
2021-02-13 08:18:33.479        5.3
2021-02-13 08:19:51.545        4.5
```

## pyearthquake

Bu Python paketi ile USGS sitesine bağlanarak istenen zaman
aralığındaki deprem verilerini almak, onları bir harita üzerinde
basmak mümkün oluyor. Daha önce blog'da paylaştığımız deprem Python
kodu statik, tek bir veri dosyası içinde, pyearthquake ile en son
verileri, istenen detayda almak mümkün.

Şuradaki yazıda güzel bilgiler var. Kurmak için PyPi paketini
indirin. Basemap için şurası. Sonra aynı komutu pyearthquake için
yapabilirsiniz.

Ornek kod:

```
from pyearthquake import *
catalog = usgs.retrieve_catalog("M1+PAST_7DAY")
print len(catalog)
mag6_list = [event for event in catalog if float(event["Magnitude"]) >= 6.0]
print len(mag6_list)
for row in mag6_list:
   print row["Eqid"], row["Magnitude"], row["Depth"],
   row["Datetime"], row["Depth"], row["Region"]  
usgs.plot_events(catalog)
```

Bu kod en son 7 gunluk, sonra Richter olceginde 6.0'dan buyuk deprem
verileri alacaktir, ve sonuncu verileri bir haritada
basacaktir. Istediginiz noktalara zoom yapmak icin zoom ikonuna
tiklayip istenen bolgeyi haritada bir dikdortgen icine aldiginiz zaman
o bolgenin detaylari gorulecektir. Ustte paylastigimiz yazida bunun
Japonya icin yapildigini goruyoruz.




