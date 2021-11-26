# Yükseklik (Elevation) Verileri

Hangi servis ile yükseklik verisi alınır? Google Elevation servisi
var, belli miktarda kullanım için bedava, sonrası için fiyatlı. Google
Console'dan proje yaratıp projede elevation servisini aktif hale
getirmek lazım, bundan sonra proje APİ kodunu kullanıp ES çağrılabilir.


```python
from urllib.request import urlopen
import polyline, json

elev_query = "https://maps.googleapis.com/maps/api/elevation/json?" + \
	      "locations=enc:%s&key=%s"
#key = "[ANAHTAR DEGERI]"
locs = [[40.994252, 29.037847],[40.991771, 29.061873]]
locs = polyline.encode(locs)
url = elev_query % (locs, key)
html = urlopen(url)
json_res = json.loads(html.read().decode('utf-8'))
print (json_res)
```

```text
{'status': 'OK', 'results': [{'resolution': 610.8129272460938,
'location': {'lng': 29.03785, 'lat': 40.99425}, 'elevation':
9.726478576660156}, {'resolution': 610.8129272460938, 'location':
{'lng': 29.06187, 'lat': 40.99177}, 'elevation': 58.94558334350586}]}
```

Bir diğer seçenek veloroutes.org adresi; burada arkadaş bedava servis
veriyor; onun sayfalarından "kazıyarak" istenen veriyi alabiliriz,


```python
import re

url = "http://veloroutes.org/elevation/?" + \
      "location=41.40000%2C28.15000&units=m"
html = urlopen(url)
res = html.read().decode('utf-8')
p = "Elevation for .*? <span style=\"font-size\:20px\">(\d*)</span> meters"
rres = re.findall(p,res)
print (float(rres[0]))    
```

```text
215.0
```

Bir diğeri elevation.racemap.com adresinden; bu arkadaşlar da bedava
servis veriyorlar, Curl kullanıp

```python
! curl -d '[[51.3, 13.4], [51.4, 13.3]]' -XPOST \
  -H 'Content-Type: application/json' \
  https://elevation.racemap.com/api
```

```text
  % Total    % Received % Xferd  Average Speed   Time    Time     Time  Current
                                 Dload  Upload   Total   Spent    Left  Speed
  0     0    0     0    0     0      0      0 --:--:-- --:--:-- --:--:--     0100    37  100     9  100    28      8     27  0:00:01  0:00:01 --:--:--    27100    37  100     9  100    28      8     27  0:00:01  0:00:01 --:--:--    27
[101,100]
```

Bu komutu tabii ki sarmalayıp sonucu bir yere yazdırıp Python ile
güzel liste döndürmesi için ayarlayabiliriz. Ya da `requests`
kullanarak aynı çağrıyı yapabiliriz,

```python
import requests

headers = {  'Content-Type': 'application/json', }
data = '[[51.3, 13.4], [51.4, 13.3]]'
response = requests.post('https://elevation.racemap.com/api',
                         headers=headers, data=data)
print(response.text)
```

Daha Çetrefil Kullanım

Alttaki örnekte bir kordinat alanıda 7 x 7 büyüklüğünde bir izgara yaratıyoruz,
o izgara öğe kordinatları için yükseklik verisini alıyoruz, ve RBF tekniği [1]
ile aradeğerleme (interpolation) yaparak yüksekliği yaklaşık şekilde temsil
ediyoruz. 


```python
import requests

def get_elev_data(coords):
    chunk = [list(x) for x in coords]
    data = "["
    for i,x in enumerate(chunk):
        data += str(x)
        if i != len(chunk)-1: data += ","
    data += "]"
    response = requests.post('https://elevation.racemap.com/api',
                             headers={'Content-Type': 'application/json',},
                             data=data)
    res = response.text
    res = res.replace("]","").replace("[","")
    res = res.split(",")
    res = [float(x) for x in res]
    return res

```

Veri alındı, şimdi RBF,

```python
from scipy.interpolate import Rbf

latlow = 36; lathigh = 37
lonlow = 29; lonhigh = 30

D = 7
x = np.linspace(lonlow,lonhigh,D)
y = np.linspace(latlow,lathigh,D)
xx,yy = np.meshgrid(x,y)
xxf = xx.reshape(D*D)
yyf = yy.reshape(D*D)
sampleCoords = []
for yyy,xxx in zip(yyf,xxf):
    sampleCoords.append([yyy,xxx])
sampleCoords = np.array(sampleCoords)
print (sampleCoords.shape)

zr =  np.array(get_elev_data(sampleCoords))

yr = sampleCoords[:,0]
xr = sampleCoords[:,1]

rbfi = Rbf(xr,yr,zr,function='multiquadric')
```

```text
(49, 2)
```

Şimdi RBF kullanarak daha yüksek çözünürlü bir izgara için yüksekliği
aradeğerleme ile hesaplatabiliriz, altta 15 x 15 büyüklüğünde bir ızgara
için bunu yapıyoruz,

```python
D = 15
x = np.linspace(lonlow,lonhigh,D)
y = np.linspace(latlow,lathigh,D)
xx,yy = np.meshgrid(x,y)
yhat = rbfi(xx,yy)

fig, ax = plt.subplots()
CS = ax.contour(xx,yy,yhat)
plt.clabel(CS, inline=1, fontsize=10)
plt.savefig('elev1.png')
```

![](elev1.png)

Kaynaklar

[1] [RBF](https://burakbayramli.github.io/dersblog/stat/stat_175_rbf/dairesel_baz_fonksiyonlari__radial_basis_functions_rbf__yukseklik_verisi_daglar.html)




