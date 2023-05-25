# Python Sözlük (Dictionary) Veri Yapısı

Python sözlük yapısı hafızada anahtar bazlı veriye direk erişim (dizi
/ vektör / sayı indisli liste erişimi kadar hızlı) sağlar.  Basit
anahtar - değer ikilisini depolayabiliriz, fakat değer olarak
depolanan şey herhangi bir obje olabilir. Bu özellik sözlüklere geniş
kullanım alanı sağlar.

En basit kullanım,

```python
# bos sozluk yarat
dict1 = {}

# depola
dict1['anahtar1'] = "deger1"

# erisim yap
dict1['anahtar1']
```

```text
Out[1]: 'deger1'
```

Herhangi bir obje depolanır demiştik,

```python
dict1['anahtar2'] = np.array([1,2,3])
dict1['anahtar2']
```

```text
Out[1]: array([1, 2, 3])
```

Tüm anahtarları göstermek için

```python
dict1.keys()
```

```text
Out[1]: dict_keys(['anahtar1', 'anahtar2'])
```

Bu liste gezilebilir, ve bu sırada anahtar kullanılıp değer ekrana basılabilir,

```python
for k in dict1: print (dict1[k])
```

```text
deger1
[1 2 3]
```

Eğer olmayan değere erişirsek hata gelir,

```python
dict1['olmayan anahtar']
```

```text

KeyErrorTraceback (most recent call last)
<ipython-input-1-90fef2a85d10> in <module>
      1 plt.figure();
      2 
----> 3 dict1['olmayan anahtar']

KeyError: 'olmayan anahtar'
```

Hata oluşturmadan eğer anahtar varsa değer yoksa `None` gelsin istersek `.get`
çağrısı var,

```python
print (dict1.get('olmayan anahtar'))
```

```text
None
```

```python
print (dict1.get('anahtar1'))
```

```text
deger1
```

Kavrama (comprehension) yani [1] tek satırda hem başka bir listeyi gezip aynı
anda başka bir liste / sözlük yaratma kabiliyeti sözlükler için de geçerli,

```python
dict2 = { i:x for i,x in enumerate(['ali','veli','ahmet']) }
print (dict2)
```

```text
{0: 'ali', 1: 'veli', 2: 'ahmet'}
```

`i:x` ile anahtar/değer ikilisini başka bir listeyi gezerken tanımladık, ve
bu tanımlar toparlanıp bir yeni sözlük yaratımı için kullanıldı.

### Sözlük İç Yapısı

Sözlüklerin iç kodlanması nasıl yapılmıştır acaba? Bundan da
bahsedelim, mülakat sorusu olabilir, zaten görünmese de bu tür
detayları bilmek faydalıdır. Bir sözlük paketini kodlamak için en baz
depolama veri yapısı olarak bir dizi (array) kullanabiliriz. Ama dizi
erişimi tam sayı bazlıdır, üstelik baştan kaç öğe olduğu bellidir,
anahtarı herhangi bir şey olabilecek sözlükleri bunun üstüne nasıl
monte edeceğiz?

Böleç (hash) kavramı burada yardıma yetişir. Harfler, alfanumerik
herhangi bir veriyi `hash` ile sayısal forma çevirebileceğimizi
biliyoruz,

```python
hash("anahtar1")
```

```text
Out[1]: 6762465535002540417
```

```python
hash("anahtar2")
```

```text
Out[1]: 1439585439643666777
```

Eğer sözlüğün deposu olarak 10 büyüklüğünde bir listeyi depolama için
kullanmak istiyorsak üstteki sayıların 10 üzerinden modülüs hesabını
yaparız,

```python
hash("anahtar1") % 10
```

```text
Out[1]: 7
```

İşte bu sayı dizide erişim için kullanılır.

Eğer dizi büyüklüğüne kıyasla çok fazla anahtar değeri var ise birden
fazla anahtar aynı dizi öğesine düşebilir, bu "çakışma (collision)"
durumu yaratır, bu durumda çakışma olan öğede bir liste yaratırız
(dizi içinde liste), ve erişim için arama önce anahtar bazlı direk
sonra liste bazlı teker teker gider.

### DiskDict

Paketten çıkan hali ile Python sözlükleri hafıza bazlıdır. Fakat eğer anahtar
erişimi bizi bir disk deposuna götürsün istersek? Dikkat tüm işlemler hafızada
yapıldıktan sonra işi bitmiş sözlüğü diske yazmaktan bahsetmiyorum, her anahtar
erişimi, her depolama işleme tekil bazlı olarak diske gitmeli diyorum. 

Bu ihtiyaç için bazı paketler var. Çoğu dış arayüzünü Python basit
sözlük ile benzer tutmuş böylece kodlayan yeni bir erişim stili
öğrenmeye mecbur değil, tüm bildik sözlük işlemleri geçerli.

Böyle bir paket `diskdict` [3]. Kurmak için kaynaklar kullanılırsa iyi
olur, önce alttakiler,

```
sudo apt install python3-leveldb libleveldb-dev libffi-dev

pip install repoze.lru structlog pycparser
```

Ardından `setup.py` ile kurulum yapılır. Örnek,

```python
from diskdict import DiskDict

dict3 = DiskDict('/tmp/diskdict')

dict3['anahtar3'] = 'deger3'
```

Bu işlemler sonrası `/tmp/diskdict` dizi altında bazı dosyaların
yaratılmış olduğunu göreceğiz. Sözlüğün disksel erişim işlemleri bu
dosyalar üzerinden yapılıyor, her erişim her depolama direk bu
dosyalara gidiyor (dosyaların ikisel formatı muhakkak hızlı erişim
için ayarlanmış halde).

Diğer Disk Bazlı Seçenekler

Eğer disk bazlı sözlük erişimini ayrı bir servise ayırıp ona REST APİ
bazlı erişim sağlasak bir nevi anahtar/değer veri tabanı (key-value
store) kavramına erişmiş olurduk. MongoDB gibi yazılımlar bir bakıma
bu servisi sağlıyorlar, daha yüksek ölçekte ek bazı servislerle beraber
fakat sonuçta yaptıkları disk bazlı bir sözlük servisi sağlamaktır.

Konu hakkında önceki bir yazı şurada [2].

### SQliteDict

Ustteki `diskdict` kurulumu problem cikartirsa erisimi bir o kadar hizli
`sqlitedict` paketi olabilir. Bu paket ile

```python
from sqlitedict import SqliteDict

dd = SqliteDict('walkdict.sqlite', autocommit=False)
```

ile bir disk bazlı, daha doğrusu sqlite taban bazlı bir sözlük
yaratıyoruz, ardından `dd['anahtar'] = 'değer'` ve `dd.commit()`
türündeki çağrılar diskteki sözlüğü güncelliyor. Erişim için
`SqliteDict` objesi aynı dosya ismi için tekrar oluşturuluyor ve
`dd[anahtar']` bize gerekli değeri geri veriyor. Arka planda paket
anahtarları muhakkak SQL tablosunda bir ana anahtar olarak kullammış,
erişimim hızlı indisler üzerinden ve her şeyi bellege taşımadan yapacaktır.

### Olağan Değerler, Sıralanma

Olağan Değerler

Pek çok ihtiyaca yardım etmeye uğraşan ek sözlük tipleri de
vardır. Bunlardan biri olmayan bir değere erişildiğinde o anahtar için
olağan (default) bir değeri otomatik yaratan bir sözlük tipidir,
`defaultdict`

```python
from collections import defaultdict

dict4 = defaultdict(int)
dict4['anahtar4']
```

```text
Out[1]: 0
```

Olağan değer sözlüklerinin en kullanışlı olduğu yer belli anahtar değerlerine
tekabül eden listelere öğe eklemek istediğimiz zamandır. Pek çok problemde
belli anahtarlara tekabül eden liste değerleri gezeriz, onları bir listeye
döngü içinde eklemek isteriz fakat her anahtar için "eğer bu anahtar varsa
listeye ekle, yoksa boş liste yarat, ondan sonra ekle" mantığı kod fazlalığı
oluşturabilir, `defaultdict(list)` kullanırsak her anahtarın olağan değeri
boş liste olacağı için bir anahtar var mı yok mu bakmadan direk o anahtarın
listesine ekleme yapabiliriz çünkü o anahtarın listesi yoksa nasıl olsa
yaratılacaktır. Örnek,

```python
from collections import defaultdict

city_list = [('TX','Austin'), ('TX','Houston'), ('NY','Albany'),
('NY', 'Syracuse'), ('NY', 'Buffalo'), ('NY', 'Rochester'), ('TX',
'Dallas'), ('CA','Sacramento'), ('CA', 'Palo Alto'), ('GA',
'Atlanta')]

cities_by_state = defaultdict(list)
for state, city in city_list:
    cities_by_state[state].append(city)

cities_by_state
```

```text
Out[1]: 
defaultdict(list,
            {'TX': ['Austin', 'Houston', 'Dallas'],
             'NY': ['Albany', 'Syracuse', 'Buffalo', 'Rochester'],
             'CA': ['Sacramento', 'Palo Alto'],
             'GA': ['Atlanta']})
```

Gördüğümüz gibi `cities_by_state[state]` üzerinde direk `.append`
yapabildik çünkü listenin orada olacağını varsayabiliyoruz.

Eklenme Sırasını Hatırlayan Sözlük

Anahtarın eklenme sırasını hatırlayıp değerler istendiğinde ona göre
listeyi döndüren bir diğer sözlük tipi `collections.OrderedDict`.

```python
import collections

dict5 = collections.OrderedDict()
dict5["A"]=1
dict5["B"]=2
dict5["D"]=4
dict5["C"]=3
for i in dict5.items(): print(i)
```

```text
('A', 1)
('B', 2)
('D', 4)
('C', 3)
```

Sonuca bakıyoruz, liste aynen eklenme sırasını yansıtıyor. Normal bir
sözlük bu sonucu garantilemez, sıralama rasgele olabilirdi.

# Anahtar Çeşitleri

"Depolanan şey herhangi bir obje olabilir" dedik, buna ek olarak,
belli şartlara uymak kaydıyla anahtar çoğu baz Python objeleri bile
olabilir. Mesela tüpler (tuples),

```python
tuple1 = ((2,3),(4,5))
tuple2 = ((8,1),(9,9))
dict6 = {}
dict6[tuple1] = "bazi degerler burada"
dict6[tuple2] = "baska degerler"
print (dict6[tuple1])
print (dict6[tuple2])
```

```text
bazi degerler burada
baska degerler
```

Anahtar olabilecek temel tipler değiştirilemeyen (immutable)
nesnelerdir, bunlar String, int, tuple tipleri. Fakat daha önce
bahsettiğimiz gibi bir anahtar sonuçta böleçlenen bir şey, o zaman bir
böleç fonksiyonu sağlayan her nesne, kullanıcı tanımlı bile olsa
anahtar olabilir.

```python
class C:
    def __init__(self, a1, a2):
        self.a1 = a1
        self.a2 = a2

    def __hash__(self):
        return hash((self.a1, self.a2))

    def __eq__(self, other):
        return (self.a1, self.a2) == (other.a1, other.a2)

object_a = C("a", 1)
object_b = C("a", 1)
object_c = C("b", 2)

a_dictionary = {object_a : 3, object_b : 4, object_c : 5}

print(a_dictionary[object_a])
```

```text
4
```

Görüldüğü gibi `object_a` objesinin tamamını bir anahtar olarak kullandık
ve doğru değere erisebildik çünkü sınıf üzerinde `__hash__` tanımlandı
perde arkasında Python sözlük kodu bu fonksiyonu çağırarak gerekli değeri
oradan alabiliyor.

Dikkat edersek `object_b` aynı değerleri taşıdığı için aynı böleci
üretir, o zaman sözlük açısından aynı anahtar değeridir. `object_c`
farklı değerleri olduğu için farklı bir anahtardır.

```python
print(a_dictionary[object_b])
print(a_dictionary[object_c])
```

```text
4
5
```

Değer `4` iki üstteki ile aynı, ama `5` farklı.

Kaynaklar

[1] <a href="../../2021/12/python-list-comprehension.html">Python Liste Kavraması (List Comprehension)</a>

[2] <a href="../../2022/11/nosql-diy-python.html">NoSQL</a>

[3] https://github.com/deep-compute/diskdict




