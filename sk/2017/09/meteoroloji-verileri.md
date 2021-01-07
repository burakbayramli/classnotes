# Meteoroloji Verileri - ECMWF, NOAA, OpenWeatherMap

OpenWeatherMap

Bu Web servisi kayıt olanlara bir APİ anahtarı verir ve belli sayıda
APİ çağrısı için kullanım bedavadır. Servisten o andaki sıcaklık,
rüzgar, nem vs gibi verileri alabiliriz, ayrıca bu veriler için
tahminler de aynı APİ üzerinden paylaşılıyor. 

https://openweathermap.org

https://openweathermap.org/api

Yazının geri kalanında bu anahtarı aldığınızı, ve anahtarın `.owmkey`
adlı bir dosyada olduğunu farzediyoruz.

Altta şu anda `lat,lon` enlem ve boylamlarındaki rüzgar hızı ve yönünü alıyoruz,

```python
import requests, json
lat,lon = 41.969901,29.070148
base_url = 'http://api.openweathermap.org/data/2.5/weather?'
weatherapi = open(".key").read()

payload = { 'lat': str(lat), 'lon': str(lon), 'units': 'metric', 'APPID': weatherapi }
r = requests.get(base_url, params=payload) 
res = []
for x in r.iter_lines():
    x = json.loads(x.decode())
    res.append (x['wind'])

print ('json',res)
print ('hiz', res[0]['speed'])
print ('yon', res[0]['deg'])
```

```text
json [{'speed': 8.92, 'deg': 214}]
hiz 8.92
yon 214
```










ECMWF

Hava verisi uzerinde yapay ogrenim ile tahminler yapmak isteyenler ham
veriyi almak icin alttaki siteye basvurabilir.

https://www.ecmwf.int/

Bu bir Avrupa bilim organizasyonu, ayrica hava tahmini modellerini
isletip tahmin de uretiyorlar (Harvey kasirgasinin nereye vuracagini
ABD NOAA'dan daha iyi tahmin ettiler). Python ile veri indirmek
mumkun,

```
sudo pip install ecmwf-api-client
```

Burada "Register" ile kullanici bilgileri, email, vs. verilip
kaydolunur. Bir aktivasyon email'i sonra bir tane daha email geliyor,
ve kayit bitiyor.  Login yapilir. Simdi API'ye erismek icin anahtar
lazim,

https://api.ecmwf.int/v1/key/

Burada gosterilen 

```
{

    "url"   : "https://api.ecmwf.int/v1",

    "key"   : "[ANAHTAR]",

    "email" : "[email]"

}
```

formundaki anahtar $HOME/.ecmwfapirc dosyasina yazilir. 

Verilere erismeden once veri turune gore bazi lisans sayfalarinda bir
lisans kabuluna "evet" demek gerekiyor, mesela alttaki tur bir script
icin

```
from ecmwfapi import ECMWFDataServer

server = ECMWFDataServer()

server.retrieve({

    'dataset' : "tigge",

    'step'    : "24/to/120/by/24",

    'number'  : "all",

    'levtype' : "sl",

    'date'    : "20071001/to/20071003",

    'time'    : "00/12",

    'origin'  : "all",

    'type'    : "pf",

    'param'   : "tp",

    'area'    : "70/-130/30/-60",

    'grid'    : "2/2",

    'target'  : "data.grib"

    })
```

Su lisansa

http://apps.ecmwf.int/datasets/licences/tigge/

evet demis olmak lazim. Bir tane daha

http://apps.ecmwf.int/datasets/licences/general


Eger lisans kabul edilmemisse hata mesaji hangi sayfaya gidilecegini soyler.

NOAA

https://www.ncdc.noaa.gov/orders/qclcd/

adresinde gunluk dosyalar var.

Yapay Ogrenim

Hava tahmini icin gunluk (saatlik, vs) hava verisi cok boyutlu bir
zaman serisi olarak gorulebilir, mesela her t aninda sicaklik, nem,
ruzgar hizi cok boyutlu bir zaman serisi olarak geliyor, egitim icin N
tane gorulen veri kullanilir, bu veri N+1, N+2, . anlarindaki gelecek
zaman serisini tahmin icin kullanilir. Bu sekilde hava tahmininin
ornegi alttaki kodlarda bulunabilir,

https://github.com/mouradmourafiq/tensorflow-lstm-regression/blob/master/lstm_weather.ipynb
