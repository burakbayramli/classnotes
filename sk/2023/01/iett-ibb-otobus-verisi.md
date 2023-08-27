# IETT Verileri, OSM, SOAP

Moovit [4] benzeri bir uygulamayı nasıl yazarız sorusuna cevap, önce
İETT baz seyahat verileri gerekli. Gerekli veriler açık kaynak OSM
verisi, ya da IETT API servisi üzerinden alınabilir.

### OSM

Open Street Map adresinde gönüllülerin oluşturduğu metro, otobüs hat
verileri var. Bu veriler her ülke için mevcut ana OSM dosyaları
içinde, onların tarama yöntemleri var, fakat bazı servisler ile bu
veriyi sorgulayıp çıktıları JSON dosyası ile yazmak ta mümkün. Mesela
alttaki siteye gidilebilir,

https://overpass-turbo.eu

Bu sitede sol kısımda kod girilen yere alttaki kod yazılır,

```
[out:json];
area[name="İstanbul"]->.a;
(
  nwr["route"="subway"](area.a);
);
out body;
>;
out skel;
```

Bu sorgu `Run` düğmesi ile işletilir, ardından `Export` düğmesi ile mesela GeoJSON
çıktısı alınabilir. Bu çıktı içinde metro hatlarının durakları, durak kordinatları,
her hattın geçtiği durak listesi vardır.

Sitede sorgu işletmek yerine aslında program işleterek aynı sonuçları
alabiliriz, işleri otomatikleştirme için bu daha iyi olur. Bunun için

https://overpass-api.de/api/interpreter

servisi var, istenen sorgu `?data=` sonrasında üstteki bağlantıya
verilebiliyor, arka plandaki servis sorguyu işletip sonucu
kayıtlamamızı sağlıyor. Tüm URL'in neye benzediği `Export` ile gidilen
ekranda `Overpass API` bağlantısında görülebilir. Üretilen string
basit aslında, üstteki sorgu kodunun kodlanmış hali, bu kodlamayı
Python ile biz de yapabiliriz, ve böylece çağrıyı otomatikleştirmiş
oluruz.

```python
import urllib.parse, requests

base_url = "https://overpass-api.de/api/interpreter?data="

q = """
[out:json];
area[name="İstanbul"]->.a;
(
  nwr["route"="bus"](area.a);
);
out body;
>;
out skel;
"""

safe_string = urllib.parse.quote_plus(q)
r = requests.get(base_url + safe_string)
fout = open("bus.json","w")
fout.write(r.text)
fout.close()
```

Artık üretilen `bus.json` verisini işleyip hat verisini çıkartabiliriz,
alttaki örnekte kordinatı olan duraklar hat verisini ekleniyor ve bir tanesi
örnek olarak haritaya basılıyor.

```python
import folium
import json

d = json.loads(open("bus.json").read())
lines = {}

nodes = {}
for e in d['elements']:
    if 'lat' in e:
        nodes[e['id']] = (e['lat'],e['lon'])

for e in d['elements']:
    if e['type'] == 'relation':
        if 'name' not in e['tags']: continue           
        line = [m['ref'] for m in e['members']]
        lines[e['tags']['name']] = line

coords = [nodes[m] for m in lines['43R Rumelihisarüstü-Kabataş'] if m in nodes]

m = folium.Map(location=[41,29], tiles='Stamen Terrain', zoom_start=10)

folium.PolyLine(locations=coords, color="blue").add_to(m)

m.save("43r.html")
```

[Çıktı](43r.html)

Eğer kısayol algoritmaları işletmek istersek, ki bunun için
düğüm/kenar verisini (yani durak/hat) bir sözlük içinde tutmak lazım,
bu sözlük `d['elements']` listesini gezerken bir `G` sözlüğü için
alttaki gibi yaratılabilir,

```python
  ...
  for i in range(len(e['members'])-1):
      src,dest = e['members'][i]['ref'], e['members'][i+1]['ref']
      if src not in G: G[src] = {}
      G[src][dest] = 1
```

Not olarak ekleyelim, OSM açık bir veri kaynağı olarak katkıcıların
her türlü veriyi kaydetmesini sağlar, tüm taban içinde kafeler,
sinemalar, benzin istasyonları, restoranlar gibi pek çok bilgi
bulunmaktadır. Mesela bir noktaya en fazla 2 km yakındaki kafeler için

```
q = """
[out:json];
  node["amenity"~"cafe"](around:2000,40.958905, 29.1020804);
  out center;
"""
```

Bir kutu içine düşen, mesela alt sol 30,20 üst sağ nokta 32,22 olsun,
tüm kamp alanlarını görmek için

```
[out:json];
   node["tourism"="camp_site"]
   (30,20,32,22); out; (._;>;);
out;
```

Yürüyüş (hiking) yolları bulmak için, yine bir kutu içinde, ki kutu
dort sayı ile tanımlanıyor, ilk iki sayı kutunun sol alt köşesinin
enlem, boylam sayıları, son iki sayı ise kutunun sağ üst köşesinin
enlem ve boylamı.

```
[out:json];
way["highway"~"path|track|footway|steps|bridleway|cycleway"]
(30,20,32,22);
out;
```

### IETT

IETT API'si hakkında belgeler [2]'de, örnek kod [1]'de.

Baz URL ve başlangıç ayarları,

```python
import pandas as pd, json
from zeep import Client
base = "https://api.ibb.gov.tr/iett"
pd.set_option('display.max_columns', None)
```

Mesela tüm duraklar için (biraz zaman alabilir)

```python
client = Client(wsdl=base + "/UlasimAnaVeri/HatDurakGuzergah.asmx?wsdl")
data_text=client.service.GetDurak_json(DurakKodu="")
df=pd.DataFrame(json.loads(data_text))
```

Bu verinin tam halini şuradan [3] alabilirsiniz.

Tek bir durak için

```python
client = Client(wsdl=base + "/UlasimAnaVeri/HatDurakGuzergah.asmx?wsdl")
data_text=client.service.GetDurak_json(DurakKodu="113252")
df=pd.DataFrame(json.loads(data_text))
print (df)
```

```text
   SDURAKKODU SDURAKADI                                  KOORDINAT  ILCEADI  \
0      113252    MASLAK  POINT (29.0209720086216 41.1085509961235)  Sariyer   

     SYON AKILLI  FIZIKI  DURAK_TIPI ENGELLIKULLANIM  
0  LEVENT    VAR  KAPALI  WALLMODERN     Uygun Degil  
```

Bir hat üzerindeki tüm otobüslerin durumu (canlı veri),


```python
client = Client(wsdl=base + "/FiloDurum/SeferGerceklesme.asmx?wsdl")
data_text=client.service.GetHatOtoKonum_json(HatKodu="15B")
df=pd.DataFrame(json.loads(data_text))
print (df)
```

```text
   kapino            boylam             enlem hatkodu guzergahkodu  \
0   C-231        29.1032215        41.0488515     15B     15B_G_D0   
1   C-301  29.0284016666667         41.035909     15B     15B_D_D0   
2   C-299  29.0548008333333  41.0486603333333     15B     15B_D_D0   
3   C-230  29.1017951666667  41.0361956666667     15B     15B_G_D0   
4   C-229         29.086476  41.0254901666667     15B     15B_G_D0   
5   C-296         29.079965         41.030994     15B     15B_D_D0   
6   C-228         29.079794  41.0324728333333     15B     15B_G_D0   
7   C-294  29.0963016666667  41.0258851666667     15B     15B_D_D0   
8   C-227  29.0490996666667  41.0459533333333     15B     15B_G_D0   
9   C-226  29.0331556666667        41.0386985     15B     15B_G_D0   
10  C-232  29.1069303333333  41.0502151666667     15B     15B_G_D0   
11  C-304        29.0160465        41.0276185     15B     15B_D_D0   
12  C-234         29.106912  41.0501541666667     15B     15B_G_D0   
13  C-297        29.0764325        41.0453085     15B     15B_D_D0   

                                       hatad                 yon  \
0   TOPAĞACI MAHALLESİ / GÜZELTEPE - ÜSKÜDAR  ÜSKÜDAR  CAMİİ ÖNÜ   
1   TOPAĞACI MAHALLESİ / GÜZELTEPE - ÜSKÜDAR         KURAN KURSU   
2   TOPAĞACI MAHALLESİ / GÜZELTEPE - ÜSKÜDAR         KURAN KURSU   
3   TOPAĞACI MAHALLESİ / GÜZELTEPE - ÜSKÜDAR  ÜSKÜDAR  CAMİİ ÖNÜ   
4   TOPAĞACI MAHALLESİ / GÜZELTEPE - ÜSKÜDAR  ÜSKÜDAR  CAMİİ ÖNÜ   
5   TOPAĞACI MAHALLESİ / GÜZELTEPE - ÜSKÜDAR         KURAN KURSU   
6   TOPAĞACI MAHALLESİ / GÜZELTEPE - ÜSKÜDAR  ÜSKÜDAR  CAMİİ ÖNÜ   
7   TOPAĞACI MAHALLESİ / GÜZELTEPE - ÜSKÜDAR         KURAN KURSU   
8   TOPAĞACI MAHALLESİ / GÜZELTEPE - ÜSKÜDAR  ÜSKÜDAR  CAMİİ ÖNÜ   
9   TOPAĞACI MAHALLESİ / GÜZELTEPE - ÜSKÜDAR  ÜSKÜDAR  CAMİİ ÖNÜ   
10  TOPAĞACI MAHALLESİ / GÜZELTEPE - ÜSKÜDAR  ÜSKÜDAR  CAMİİ ÖNÜ   
11  TOPAĞACI MAHALLESİ / GÜZELTEPE - ÜSKÜDAR         KURAN KURSU   
12  TOPAĞACI MAHALLESİ / GÜZELTEPE - ÜSKÜDAR  ÜSKÜDAR  CAMİİ ÖNÜ   
13  TOPAĞACI MAHALLESİ / GÜZELTEPE - ÜSKÜDAR         KURAN KURSU   

       son_konum_zamani yakinDurakKodu  
0   2023-01-23 12:41:54         215682  
1   2023-01-23 12:41:55         219271  
2   2023-01-23 12:41:52         219333  
3   2023-01-23 12:41:51         217141  
4   2023-01-23 12:41:53         222173  
5   2023-01-23 12:41:56         223352  
6   2023-01-23 12:41:58         223351  
7   2023-01-23 12:41:57         261901  
8   2023-01-23 12:41:57         219322  
9   2023-01-23 12:41:57         219282  
10  2023-01-23 12:41:53         223901  
11  2023-01-23 12:41:56         404091  
12  2023-01-23 12:41:52         223901  
13  2023-01-23 12:41:52         220111  
```

O andaki tüm otobüslerin durumu,

```python
client = Client(wsdl=base + "/FiloDurum/SeferGerceklesme.asmx?wsdl")
data_text=client.service.GetFiloAracKonum_json()
df=pd.DataFrame(json.loads(data_text))
print (df)
```

```text
     Operator                      Garaj KapiNo      Saat            Boylam  \
0         OHO                       None  A-001  12:42:58         29.045502   
1         OHO                       None  A-002  12:42:51  28.9921048333333   
2         OHO                       None  A-003  12:42:54  29.0243251666667   
3         OHO                       None  A-004  12:43:00  29.0538623333333   
4         OHO                       None  A-005  12:42:56  29.0375251666667   
...       ...                        ...    ...       ...               ...   
6585     IETT  IKITELLIISLETTIRMEGARAJI2  T1104  12:42:58         28.953418   
6586     IETT  IKITELLIISLETTIRMEGARAJI2  T1105  12:42:54  28.7240793333333   
6587     IETT  IKITELLIISLETTIRMEGARAJI2  T1106  12:43:00  28.7477818333333   
6588     IETT  IKITELLIISLETTIRMEGARAJI2  T1107  12:42:54  28.7472726666667   
6589     IETT  IKITELLIISLETTIRMEGARAJI2  T1108  12:42:55         28.675459   

                 Enlem Hiz       Plaka  
0           41.0786255  24  34 HO 1000  
1     41.0373373333333   3  34 HO 1001  
2     41.0480058333333   0  34 HO 1002  
3            41.099348  15  34 HO 1003  
4           41.1275815   0  34 HO 1004  
...                ...  ..         ...  
6585        41.0119565  26    34KT9714  
6586  40.9862843333333   0    34KT9728  
6587  41.0615378333333  36    34LD0364  
6588  41.1065363333333   0    34LD0369  
6589  41.0843873333333   0    34CFK810  

[6590 rows x 8 columns]
```

API'de olmayan bir veri çeşidi bir hattın geçtiği tüm durakların
listesi. Hat üzerinde giden otobüslerin o anda yakın olduğu duraklar
canlı olarak paylaşılıyor fakat statik hat verisi yok, önce şu durak
sonra şu durak gibi..  

Bazı Eksikler

API servisi yazılalı bayağı zaman geçmiş herhalde, SOAP diye bir
paylaşım tekniği kullanılmış, bu sistem artık yerini daha basit, daha
sağlam bir teknik olan REST tekniğine bıraktı. Basit Web URL'leri
üzerinde gerekirse JSON ile veri alınıp verilip bilgi paylaşımı
yapılabiliyor.

Ayrıca bazen hatalı durumlar ortaya çıktığında ekranda görülen `ORA-`
türü mesajlara bakınca arka plandaki veri tabanının Oracle olduğu
anlaşılıyor. Bu bir ticari tabandır, her ne kadar sağlam bir yazılım
olsa da, lisansları pahalıdır, kamu hizmeti veren bir servis açık
yazılım, bedava ve bir o kadar sağlam Postgresql tabanını kullanarak
daha iyi (ve ucuz) hizmet verebilir.


Kaynaklar

[1] https://github.com/hakanatak/dataibbgovtr_python

[2] https://data.ibb.gov.tr/dataset/3e32bb5d-2936-41eb-bdc7-65b843487e99/resource/6821f452-f6ff-49e9-940a-d4ebfc78f03e/download/iett-web-servis-kullanm-dokumanv.1.2.pdf

[3] https://drive.google.com/uc?export=view&id=1ATNV5qtW0gJoPuz2BKWBevakIe0mUiWF

[4] https://moovitapp.com/istanbul-1563/lines/14b/21021537/6467401/en

