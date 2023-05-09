# Pandas

Pandas'ı ilk kez bir CSV dosyasını okumak ve işlemek için
kullanmıştım; numpy bu amacla yazılmamış -rivayete göre ticari ürün
Matlab hala CSV dosyalarını basit bir şekilde okuyamıyor-, ve bazı
belgelere göre Pandas bu işi R'ye benzer şekilde yapabiliyordu. Pandas
kurduktan sonra, alttaki gibi bir kod Pandas noktalı virgül ile
ayrılmış dosyayı söylendiği gibi güzelce okudu,

```python
import pandas as pd, io
pd.set_option('display.max_columns', None)

s1 = """
c;d;a;b
one;0;0;7
one;1;1;6
one;2;2;5
two;0;3;4
two;1;4;3
two;2;5;2
two;3;6;1
"""

df1 = pd.read_csv(io.StringIO(s1),sep=';')
print (df1)
```

```text
     c  d  a  b
0  one  0  0  7
1  one  1  1  6
2  one  2  2  5
3  two  0  3  4
4  two  1  4  3
5  two  2  5  2
6  two  3  6  1
```

Not: CSV'yi yazıda gösterebilmek için StringIO uzerinden okuduk, fakat
aslında read_csv ile direk dosyadan da okuyabilirdik.

Güzel... Ama sol tarafta bir takım sayılar var, 0,1,..,6 diye gidiyor,
bunlar nedir? CSV okuyup direk numpy matrisi elde etsek olmaz mıydı?
Bu "ekstra" "gereksiz" şeyleri ne yapacağız? Baştaki amacımız Pandas'i
numpy için bir önyüz gibi kullanmaktı (gerçi np.array(df1)) ile hemen
çevrim yapılabilirdi ama, şimdi, bu ek aşamaya ne gerek var?), fakat
Pandas belgelerini takip ettikçe, ve örnekleri gördükçe bu rakamların,
yanı indisin gerekliği anlaşılmaya başladı.

İndis

Pandas'in en temel iki objesi Series ve DataFrame'in muhakkak bir
indisi vardır. Öyle ki herhangi bir Series, DataFrame için indis
tanımlanmamissa, Pandas otomatik olarak bir indis kendisi yaratır. Bu
indis cok temel, birer birer artan düz sayılar olacaktır (üstteki
gibi) ama muhakkak bir indis olur.

Pandas ile bir kolona erismek istersem bu cok basittir; mesela

```python
print (df1['b'])
```

```text
0    7
1    6
2    5
3    4
4    3
5    2
6    1
Name: b, dtype: int64
```

Dikkat, kolona erişince indis de onunla "beraber geldi". Üstte elde
ettiğimiz bir Series objesi, DataFrame'in kolonları Series
objeleridir. Series tek bir kolonu temsil eden bir objedir.

İndis pek çok türlü tipte olabilir. Bir tarih, bir string bile
olabilir. Onu mevcut bir kolon üzerinden kendimiz tanımlayabiliriz,

```python
s2 = """
c;d;a;b
2016-01-02;one;0;0;7
2016-01-03;one;1;1;6
2016-01-04;one;2;2;5
2016-01-05;two;0;3;4
2016-01-06;two;1;4;3
2016-01-07;two;2;5;2
2016-01-08;two;3;6;1
"""

df2 = pd.read_csv(io.StringIO(s2),sep=';', parse_dates=True,index_col=0)
print (df2)
```

```text
              c  d  a  b
2016-01-02  one  0  0  7
2016-01-03  one  1  1  6
2016-01-04  one  2  2  5
2016-01-05  two  0  3  4
2016-01-06  two  1  4  3
2016-01-07  two  2  5  2
2016-01-08  two  3  6  1
```

Sıfırıncı, ilk kolonu indis olarak tanımladık, Pandas'a ayrıca
parse_dates ile bu kolonun içinde "tarihimsi" veriler olduğunu
söyledik ki onları otomatik olarak DateTime objesi haline
getirsin. Böylece tarihsel olarak büyüktür, küçüktür işlemlerini
kullanabiliriz,

```python
print (df2[df2.index > '2016-01-06'])
```

```text
              c  d  a  b
2016-01-07  two  2  5  2
2016-01-08  two  3  6  1
```

İndisi sonradan değiştirmek mümkün. Mesela b kolonunu indisi yapalım,

```python
print (df2.reset_index().set_index('b'))
```

```text
       index    c  d  a
b                      
7 2016-01-02  one  0  0
6 2016-01-03  one  1  1
5 2016-01-04  one  2  2
4 2016-01-05  two  0  3
3 2016-01-06  two  1  4
2 2016-01-07  two  2  5
1 2016-01-08  two  3  6
```

Dikkat `reset_index()` ile mevcut indisi iptal ettik, o indis normal
bir kolon haline geldi. Tabii bir isme sahip olmasi gerekiyordu,
Pandas da ona "index" diye bir isim verdi. Bu isim degistirilebilir
muhakkak.

İndisin esas değeri DataFrame'de yeni bir kolon yaratmak istediğimiz
zaman ortaya çıkar. Diyelim ki bir şekilde elimizde şöyle bir Series
var,

```python
s1 = pd.Series(['x','y','z'], index=[1,2,3])
print (s1)
```

```text
1    x
2    y
3    z
dtype: object
```

Bu seriyi tamamen kendimiz, yan bir tarafta apayrı bir şekilde
yarattık. Şimdi bu seriyi bir kolon olarak df1'e ekleyelim. Ne olacak
acaba?

```python
df1['s1'] = s1
print (df1)
```

```text
     c  d  a  b   s1
0  one  0  0  7  NaN
1  one  1  1  6    x
2  one  2  2  5    y
3  two  0  3  4    z
4  two  1  4  3  NaN
5  two  2  5  2  NaN
6  two  3  6  1  NaN
```

Pandas seriyi aldı, indisine baktı, ve o indisi df1 ile eşledi, uyan
öğeleri yeni kolon öğesi olarak ekledi, diğerlerini boş bıraktı!

Pandas'in bu özelliği sayesinde zaten formülsel hesaplar çok rahat
yapılabiliyor. Mesela x = d + a + b gibi bir hesabı direk DataFrame
üzerinde yapabiliriz,

```python
df1['x'] = df1.d + df1.a + df1.b
print df1
```

```
     c  d  a  b   s1   x
0  one  0  0  7  NaN   7
1  one  1  1  6    x   8
2  one  2  2  5    y   9
3  two  0  3  4    z   7
4  two  1  4  3  NaN   8
5  two  2  5  2  NaN   9
6  two  3  6  1  NaN  10
```

Bu ifadenin çok rahat bir şekilde işleyebilmesinin arkasında yatan sır
kolon erişiminin, toplama işlemlerinin sonucunun hepsinin içinde indis
olan sonuçlar üretmeleri - bu sayede Pandas bu sonucu alıp pat diye
geri DataFrame içine yazabiliyor.

İndis uyumu üzerinden akla gelebilecek her türlü operasyon mümkün;
mesela bir dizin içinde Series objeleri var, onları yanyana yapıştırıp
bir DataFrame oluşturabilirim, Pandas indisleri uyan hücreleri aynı
satıra koyar.

```python
s1 = pd.Series(['x','y','z'], index=[1,2,3])
s2 = pd.Series(['a','b','c'], index=[1,2,3])
s3 = pd.Series(['aa','bb','cc'], index=[1,2,3])
df3 = pd.concat([s1,s2,s3],axis=1)
df3.columns = ['bir','iki','uc']
print df3
```

```
  bir iki  uc

1   x   a  aa

2   y   b  bb

3   z   c  cc
```

Aynı şekilde DataFrame'ler de yanyana yapıştırılabilir. 

Bu arada Pandas aynen SQL tabanları gibi birleştirme operasyonu
yapabiliyor, yani iki DataFrame'i alıyorum, indis uyumu üzerinden, ya
da sadece bazı kolonların ismini verip kolon uyumu üzerinden iki
DataFrame birleştirilebilir. İşlem oldukça hızlı; bir projede her biri
1 gigabaytlık iki DataFrame'i birleştirip üçüncü bir devasa DataFrame
yaratmıştım bir kez, tamamen hafızada!

Kordinat bazlı (kordinat derken indeks ve kolon, ki indeks hangi tipte
ise o) kalıcı değişimler için

```python
df2.loc['2016-01-05','d'] = 1000
print df2
```

```
              c     d  a  b
2016-01-02  one     0  0  7
2016-01-03  one     1  1  6
2016-01-04  one     2  2  5
2016-01-05  two  1000  3  4
2016-01-06  two     1  4  3
2016-01-07  two     2  5  2
2016-01-08  two     3  6  1
```

Sadece tek bir hücre değiştirdik.

Fonksiyonlar

Üstte gördüğümüz formülsel erişim her türlü satırsal işlem için
yeterli olmayabilir. Belki bir Series'in tüm öğleri, ya da
DataFrame'in tüm satırları üzerinde bir fonksiyon işlemesi
gerekir... Burada map ve apply fonksiyonları var; Python'un
fonksiyonel ruhuna uygun bir şekilde satır satır gezinen kod
yazmıyoruz, genel bir geziciye bir fonksiyon geçiyoruz, ve gezici her
satıra / öğeye verilen fonksiyonu uyguluyor.

Diyelim ki bir kolondaki her ögeyi string haline getirip yanına "XX"
ekliyoruz,

```python
def f(x): return str(x)+"XX"

print df1.d.map(f)
```

```
0    0XX

1    1XX

2    2XX

3    0XX

4    1XX

5    2XX

6    3XX

Name: d, dtype: object
```

Çok basit fonksiyonlar için Python'un lambda kullanımı var,

```python
print df1.d.map(lambda x: str(x)+"XX")
```

Aynı sonucu verir. Elde edilen sonucun bir Series olduğuna dikkat, bir
indisi var, ve alıp bu Series'i bir DataFrame içine yazabilirdik.

Eğer fonksiyon içinde tüm DataFrame satırına  erişim gerekiyorsa,
apply kullanımı var, apply ona geçilen fonksiyona satır geçer; yani
apply satırları teker teker gezer ve satırlar sırasıyla bizim
verdiğimiz fonksiyonun ilk parametresine "düşer".

```python
print df1.apply(lambda x: str(x.c) + ":" + str(x.d), axis=1)
```

```
0    one:0

1    one:1

2    one:2

3    two:0

4    two:1

5    two:2

6    two:3
```

Sözlük

Bazen bir DataFrame'in indis değerlerini baz alan bir sözlük yaratmak
isteyebiliriz. Bunun için `to_dict` çağrısı var. Test verisi yaratalım,

```python
df = pd.DataFrame(np.array(range(20)).reshape(10,2))
df.columns = ['a','b']
df = df.set_index('a')
print (df)
```

```text
     b
a     
0    1
2    3
4    5
6    7
8    9
10  11
12  13
14  15
16  17
18  19
```

Eğer `a` bazlı bir sözlük yaratmak istersem,

```python
d = df.to_dict('index')
d
```

```text
Out[1]: 
{0: {'b': 1},
 2: {'b': 3},
 4: {'b': 5},
 6: {'b': 7},
 8: {'b': 9},
 10: {'b': 11},
 12: {'b': 13},
 14: {'b': 15},
 16: {'b': 17},
 18: {'b': 19}}
```

Artık mesela `d[2]['b']` ile 2 indis değerindeki `b` kolon değerine
erişmiş oluyorum bir bakıma, eğer daha fazla kolon olsaydı onlara da
benzer şekilde erişim sağlanacaktı. 

Pivot

Satır olarak tekrar eden iki kolondaki verileri bir tür kordinat
olarak alıp, üçüncü bir kolondaki değere göre bir tablo hücresinde
değer atamak istiyorsak, Pandas paketinin pivot özelliği ise
yarar. Pivot genel bir veri prezentasyon yaklaşımı, Postgres içinde
crosstab diye bir fonksiyon var, fakat kullanımı pek kolay değil.

```python
import numpy as np
from pandas import DataFrame
df=DataFrame({
       'foo': ['one',  'one',  'one',  'two',  'two',  'two'],
       'bar':  [ 'A' ,   'B' ,   'C' ,   'A' ,   'B' ,   'C' ] ,
       'baz':  [ '1',   '2',   '3',   '4',   '5',   '6']
       })
print df
pv = df.pivot('bar', 'foo')
print pv.to_string()
```

Sonuc

```
foo one two
bar       
A     1   4
B     2   5
C     3   6
```

Kolonda Liste

DataFrame pek cok Python tipini kabul edebilir, liste de bunlardan biri,

```python
import numpy as np
from pandas import DataFrame
df=DataFrame({
       'foo': ['one',  'one',  'one',  'two',  'two',  'two'],
       'bar':  [ '[2,3]',   '[4,5]', '[6,7]', '[8,9]', '[10,11]', '[12,13]'],
       'baz':  [ [2,3],   [4,5],   [6,7],   [8,9],   [10,11],   [12,13]]
       })
print (df)
```

```text
   foo      bar       baz
0  one    [2,3]    [2, 3]
1  one    [4,5]    [4, 5]
2  one    [6,7]    [6, 7]
3  two    [8,9]    [8, 9]
4  two  [10,11]  [10, 11]
5  two  [12,13]  [12, 13]
```

Artık `bar` ve `baz` kolonları içinde liste var, biri metin bazli
digeri duz Python objesi halinde. Listeleri nasıl geri okuruz?

```python
print (df.iloc[0].baz)
print (type(df.iloc[0].baz))
print (df.iloc[0].bar)
print (type(df.iloc[0].bar))
```

```text
[2, 3]
<class 'list'>
[2,3]
<class 'str'>
```

İlk durum kolay, liste doğru Python tipinde direk erişip
kullanırız. Eğer liste bir string olarak geliyorsa, onu Python tipine çevirmek
lazım. Burada `eval` kullanımı önerenler olabilir fakat bu kullanım yavaştır,
ayrıca verilen parametreyi bir Python kodu kabul edip işlettiği için tehlikeli
olabilir. Bu durumda en hızlı ve güvenli çözüm `json` kullanmak.

```python
import json
res = json.loads(df.iloc[0].bar)
print (res)
print (type(res))
print (res[0])
```

```text
[2, 3]
<class 'list'>
2
```

Bu yaklaşım işledi çünkü liste kabaca zaten bir JSON formatına
sahiptir, sözlük (dictionary) yok ama liste var. Bu sebeple `loads`
listeye çevrimi yapabildi.


Dosyalardan Okumak

Üstteki örneklerde `StringIO` kullandık, ama en basit Pandas kullanımı
aslında `df = pd.read_csv('[dosya]')` ile direk diskten okumak.

```python
import pandas as pd

df = pd.read_csv('data.csv')
```

Burada ilginç bazı ek numaralar da yapılabiliyor, örnek zip içinde olan csv
direk zip üzerinden okunabilir! Mesela `test.zip` içinde olan `test.csv`
dosyasını okumak için

```python
import pandas as pd, zipfile
with zipfile.ZipFile('test.zip', 'r') as z:
      df =  pd.read_csv(z.open('test.csv'))
      ... 
```

Eğer CSV dosyası dosyası direk, basit ekranda gösterilebilen türden
bir bağlantıysa, o zaman `https://...` diye giden bağlantıyı
`read_csv` çağrısına geçmek yeterlidir.

```python
df = pd.read_csv('https://.../data.csv')
```

Fakat bazen URL bağlantısı bir 'dosya indirme' aksiyonu tetikler, bu
durumda o indirilen dosyanın 'yakalanması' ve okunması gerekir. Bu
durum için ek hareketler gerekir, mesela Yahoo Finance üzerinde
görelim,

```python
import pandas as pd, urllib.request as urllib2, io

url = "https://query1.finance.yahoo.com/v7/finance/download/AAPL?period1=1492524105&period2=1495116105&interval=1d&events=history"
r = urllib2.urlopen(url).read()
file = io.BytesIO(r)
df = pd.read_csv(file,index_col='Date')
print (df['Adj Close'].tail())
```

```text
Date
2017-05-12    37.154709
2017-05-15    37.059498
2017-05-16    37.004768
2017-05-17    35.762302
2017-05-18    36.307369
Name: Adj Close, dtype: float64
```

Bunun bir ileri noktasi İnternet üzerinden indirme tetikleyen bir ZIP
bağlantısını hiç diske indirmeden hem yakalamak, hem açmak, sonra
içindeki dosyayı Pandas'a okutmak!

```python
import pandas as pd, datetime
import urllib.request as urllib2
r = urllib2.urlopen("https://www.filanca.com/test.zip").read()
file = ZipFile(BytesIO(r))
csv = file.open("test.csv")
df = pd.read_csv(csv,sep='\t',header=None)
```

Bazi Ayarlar

Ekranda `print` ile dataframe basınca bazen tüm kolonlar gözükmeyebilir,

```python
import pandas as pd
pd.set_option('display.max_columns', None)
```

Kolonun kendisini en büyük halde görmek için

```python
pd.set_option('display.max_colwidth',-1)
```

Excel'den okuma teknikleri icin [2]

Kaynaklar

[1] http://pandas.pydata.org/pandas-docs/stable/

[2] [Yazi](../../2012/05/python-pandas-excel.html)


