# JSON

Ayar (config) ya da veriyi hem insan hem makine tarafından okunabilir
bir formatta paylaşmak istersek JSON kullanabiliriz. JSON bir bakıma
Python sözlük (dictionary) kavramının dosyaya çevrilmiş hali. Python ile
sözlük yaratmak hatırlarsak,

```python
mydict = {}
mydict['anahtar1'] = 'deger1'
print (mydict)
```

```text
{'anahtar1': 'deger1'}
```

ile yapılabiliyor. Bu çetrefil sözlükleri metin dosyasından başlayarak
yaratmak için JSON kullanırız. 90'li yıllarda XML kullanılan yerlerde
artık hep JSON görüyoruz.

JSON içindeki yapı hiyerarşik olabilir, dosyada direk anahtar bazlı
tekil değerler, ya da listeler, listeler içinde daha fazla listeler
olabilir. Altta mesela bir coğrafi veri işleminde kullanılabilecek bir
örnek,


```python
print (open("test.json").read())
```

```text
{
    "center": [41, 29],
    "points": {
	"city 1": [41.1, 29.2],
	"city 2": [41.4, 29.8]
    },
    "plist": [[3,3],[5,5],[7,7]],
    "url1": "https://www.cnn.com",
    "maps": [
	     "file1.gpx",
	     "file2.gpx",
	     "file3.gpx",
	     "file4.gpx"
     ]
	
}

```

Harita merkezi `center`, nokta listeleri `points` ve onun altında yine anahtar
bazlı `city `, `city 2` diye gidiyor. Erişmek için,

```python
import json

jf = json.loads(open("test.json").read())
print (jf.keys())
```

```text
dict_keys(['center', 'points', 'plist', 'url1', 'maps'])
```

Sözlük demiştik; işte en üst seviyede hakikaten `dict_keys` ibaresi
var. Şimdi `points` noktalarından `city 1` erişimi yapalım,


```python
print (jf['points']['city 1'])
```

```text
[41.1, 29.2]
```

Bu hiyerarşik yapı istediğimiz kadar derine gidebilir. İşin güzel tarafı
tiplemenin otomatik algılanıp doğru Python objesi yaratılması, mesela 

```python
print (type(jf['points']['city 1']))
```

```text
<class 'list'>
```

Yani köşeli parantez görünce `json` paketi bunun liste objesi olduğunu
anladı. Bu özelliği farklı yerlerde de kullanabiliriz, bazen bir veri
dosyasında bir metin bazlı bir liste depolanmış olabilir,

```python
str_liste1 = '["1","2","3"]'
```

Bu veri belki bir CSV'den Pandas objesi içinde geldi, ya da başka bir
yerden, pür metin halde.. Bu metni `json`'a çevirttirebiliriz, ki bu
yaklaşım `eval` çağrısından çok daha hızlı ve güvenli işler,

```python
liste1 = json.loads(str_liste1)
print (liste1)
print (type(liste1))
```

```text
['1', '2', '3']
<class 'list'>
```

Not: Programcılar bu paketin ismini 'jey-sin' olarak telafuz eder;
erkek ismi Jason ile ses benzerliği var, yazanlar herhalde bunu bilerek
ismi seçtiler.

