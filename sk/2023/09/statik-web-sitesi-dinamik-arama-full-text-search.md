# Statik Sitelerde Arama Motoru

Bu yazıdaki amacımız bir statik dizini aranır hale getirmek ve bunu
orta katman olmadan yapabilmek.

Dinamik Web siteleri üç katmana sahiptir. 1'inci katmanda istemci
vardır, müşteri tarayıcısı ile 2'inci katmandaki servise bağlanır,
müşteri isteğini (request) orada karşılamak için işlem mantığı
koşturulur, mesela 3'üncü katmandaki bir veri tabanına bağlanıp veri
alışveri yapmak gibi, ve sonuçta sayfa üretilir, kullanıcıya
gösterilir.

Tarayıcı içindeki mekanizma o kadar nötr değildir, orada da kodlar
işletilebiliyor, bu katmandaki Javascript oldukça kuvvetli bir
dil. Javascript kodları sayfa içeriği ile beraber bağlanana
gönderilir, fakat tarayıcı içinde koşturulur, ve bu tür kodlar
prezentasyon üzerinde ek işlemler yapabilir, bir sürü görsel işlem
yapılabilir, ve hatta bir süredir Ajax ile serviste direk fonksiyon
çağrısı bile yapılabilmektedir.

Orta katman da pek çok işlem, hesap yapabilir, fakat eğer gereken
kabiliyet pür okuma bazlı ise ve gerekli veri dosyalarını hızlı
erişilebilir halde önceden hazırlamışsak, Tarayıcı -> Statik Dosya
erişimi ile envai türden servisi sağlamak mümkündür.

Arama motorunu işte bu şekilde kodlayacağız. Javascript, ya da bu
yazıda onun yerine PyScript [1], kodları tarayıcıda işleyecek,
çetrefil bir arayüz mantığı orada kodlanacak.. Bu mantık gerektiği
yerde gerekli kelimeler için belli indis dosyalarını okuyacak, bu
okuduğu içeriğe ek işlemler uygulayıp birleştirecek, ve sonuçları
sunacak. Bunun servis tarafında statik, pür dosyalar ile nasıl
yapılabileceğini anlamak için önce tüm metin arama (full-text search)
teknolojisine bakalım, ve bizim yapacağımız eklerden sonra bahsedelim.

# Tam Metin Arama

Doküman arama kelime bazlı yapılır, arama için girdiğimiz kelimeler
teker teker bir indis içinde bulunarak onların bağlı olduğu dokümanlar
getirilmeye uğraşılır. Genel olarak bir dokümandaki kelimeleri almak
kolay, sonuçta döküman bir kelime listesi, doküman ID verilip döküman
bulunup içindeki kelime listesi dökülebilir. Tam metin aramanın
yaptığı bunun tersidir, kelime verilip içinde geçtiği doküman(lar)
bulunmaya uğraşılır, yani ters yönde gidiyoruz. Bunun için farklı bir
indis gerekir, bu indis yapısına tersyüz edilmiş indis (inverted
index) ismi veriliyor.

Tam metin arama için bu indisi yaratmak gerekir, o amaçla tüm
dokümanlar gezilir, gezilirken her doküman alt kelimelerine, simgelere
(token) ayrılır [4], ve bu kelimelerden içinde geçtiği dokümanlara bir
işaret konur. Bunu bir sözlük yapısı içinde gerçekleştirebiliriz,
sözlük içinde sözlük olacak, anahtar (key) bir kelime, değer (value)
ise ikinci bir sözlük, bu sözlük içinde o kelimenin hangi dokümanda
kaç kez geçtiği olacak. Mesela bütün dokümanlar içinde "araba"
kelimesi doküman1 içinde 4 kere doküman2 içinde 7 kere geçmişse,
`{"araba": {"döküman1": 4, "doküman2": 7}.... }` gibi bir sözlük
yapısı görmeliyiz.

Daha sonra arama yaparken her arama kelimesi tersyüz edilmiş indise
sorulur, o kelimenin hangi dokümanlarda kaç kez geçtiğini hemen
bulabiliriz. Eğer birden fazla arama kelimesi var ise, her kelime için
alınan doküman listelerinin bir "kesişim kümesi" hesaplanabilir
mesela, bu biraz katı bir şart olabilir tabii, tüm arama kelimelerinin
beraber geçtiği dokümanları böylece buluruz. Daha gevşek sonuç
kriterleri kullanılabilir, mesela iki kelime ile arama yapıldı ise "ya
biri ya öteki" gibi, bunun gibi farklı kriterler düşünülebilir.

Simgelere Ayırma

İlk önce belgeyi kelimelere ayırma (tokenization) işlemine bakalım. Bu
aşamada nokta, virgül ve diğer özel karakterler çıkartılabilir, tüm kelimeler
küçük hale getirilebilir, dil TR durumunda ascii çevirimi yapılabilir.

```python
import re, unidecode

WORD = re.compile(r'\w+')

def clean_text(text):
    text = unidecode.unidecode(text).lower().replace("\n"," ").replace("\r"," ")
    punc_list = '!"#$%^()*+,-./:;<=>?@[\]^_{|}~' + '0123456789'
    t = str.maketrans(dict.fromkeys(punc_list, " "))
    text = text.translate(t)
    t = str.maketrans(dict.fromkeys("'`",""))
    text = text.translate(t)
    return text

def reg_tokenize(text):
    text = clean_text(text)
    words = WORD.findall(text)
    return words
```

Ardından indisı yaratabiliriz,

```python
from collections import defaultdict
import json

text1 = "Otomobillerin genellikle otobanda ve şehirlerarası yolda seyir halinde iken karşılaştıkları sorun aracın ön camının dışarıdan gelen bir cisimle çatlamasıdır. Araba hareket halindeyken yoldan ya da öndeki araçtan ön cama gelen taş veya sert bir cisim ön camı çatlatır"
text2 = "Araç sahipleri yolda araba sorunu yasamamak için genellikle ellerinden geleni yaparlar. Çatlağın durdurulmaması görüntünün her geçen gün kötüleşmesi anlamına gelir. Bu durumu hiçbir sürücü istemez."

invidx = defaultdict(lambda: defaultdict(int))

for word in reg_tokenize(text1):
   if len(word) > 1: invidx[word]['doc1'] += 1
for word in reg_tokenize(text2):
   if len(word) > 1: invidx[word]['doc2'] += 1

print (json.dumps(invidx)) 
```

```text

{"otomobillerin": {"doc1": 1}, "genellikle": {"doc1": 1, "doc2": 1},
"otobanda": {"doc1": 1}, "ve": {"doc1": 1}, "sehirlerarasi": {"doc1":
1}, "yolda": {"doc1": 1, "doc2": 1}, "seyir": {"doc1": 1}, "halinde":
{"doc1": 1}, "iken": {"doc1": 1}, "karsilastiklari": {"doc1": 1},
"sorun": {"doc1": 1}, "aracin": {"doc1": 1}, "on": {"doc1": 3},
"caminin": {"doc1": 1}, "disaridan": {"doc1": 1}, "gelen": {"doc1":
2}, "bir": {"doc1": 2}, "cisimle": {"doc1": 1}, "catlamasidir":
{"doc1": 1}, "araba": {"doc1": 1, "doc2": 1}, "hareket": {"doc1": 1},
"halindeyken": {"doc1": 1}, "yoldan": {"doc1": 1}, "ya": {"doc1": 1},
"da": {"doc1": 1}, "ondeki": {"doc1": 1}, "aractan": {"doc1": 1},
"cama": {"doc1": 1}, "tas": {"doc1": 1}, "veya": {"doc1": 1}, "sert":
{"doc1": 1}, "cisim": {"doc1": 1}, "cami": {"doc1": 1}, "catlatir":
{"doc1": 1}, "arac": {"doc2": 1}, "sahipleri": {"doc2": 1}, "sorunu":
{"doc2": 1}, "yasamamak": {"doc2": 1}, "icin": {"doc2": 1},
"ellerinden": {"doc2": 1}, "geleni": {"doc2": 1}, "yaparlar": {"doc2":
1}, "catlagin": {"doc2": 1}, "durdurulmamasi": {"doc2": 1},
"goruntunun": {"doc2": 1}, "her": {"doc2": 1}, "gecen": {"doc2": 1},
"gun": {"doc2": 1}, "kotulesmesi": {"doc2": 1}, "anlamina": {"doc2":
1}, "gelir": {"doc2": 1}, "bu": {"doc2": 1}, "durumu": {"doc2": 1},
"hicbir": {"doc2": 1}, "surucu": {"doc2": 1}, "istemez": {"doc2": 1}}

```

Arama kodlaması oldukca basit, indise direk olarak o kelimeyi
soruyoruz, tabii arama kutusuna girilen kelimeler üzerinde de üstteki
temizleme, simgeleme yapıldıktan sonra. Bunların yapıldığını
farzedelim, ve tek bir kelimeyi soralım,

```python
invidx['araba']
```

```text
Out[1]: defaultdict(int, {'doc1': 1, 'doc2': 1})
```

Bu kadar. Birden fazla kelime için sonuç listelerinin `set.intersection` ile
kesişimini bulabiliriz, bir örnek alttadır,

```python
a = [1, 2, 3, 4]; b = [2, 3, 4, 5]; c = [3, 4, 5, 6]; d = [4, 1, 3, 9]
u = set.intersection(set(a),set(b),set(c),set(d))
print (u)
```

```text
{3, 4}
```

Sıralama amaçlı şunu yaparız, her kelime için mesela "araba" kelimesi
`doc1` içinde 2 kere, "cami" kelimesi aynı doç1 içinde 1 kere geçtiyse
`doc1` için 2+1 = 3 ağırlık vardır, `doc1` için 3 döndürürüz, ve tüm
liste üzerinde bu ağırlık bazlı sıralama yaparız.

```python
points = [0, 12, 9]
res = sorted(range(len(points)), key=lambda x: points[x])
print(res)
```

```text
[0, 2, 1]
```

### Web Kodlaması

Eğer üstteki arama metotunu uygulama servisi (app server) olmadan
kodlamak istiyorsak, yani sadece PyScript (ya da Javascript) ve servis
tarafında bir takım dosyaların olduğu yaklaşım, kodlamayı şöyle
yapabiliriz. Tersyüz edilmiş indisi dizüstü geliştirme ortamında tek
bir JSON olarak kaydedebilirdik, onu web servisine koyup, bu dosyanın
gerektiğinde istemciden `open_url` ile https adresinden alınıp sözlük
olarak tarayıcıda işlenmesi neredeyse tüm ihtiyaçları karşılardı. Tek
problem tek JSON dosyasının büyük olma ihtimali, bizim site için
mesela bu dosya 10-15 MB, çözüm şudur, indisi kelime ilk harfini baz
alarak parçalara böleriz. Mesela 'a' ile başlayan tüm kelimelerin
tersyüz edilmiş indis dosyası `invidx-a.json`, 'b' için
`invidx-b.json`, böyle gidiyor..

Bölünmüş indis dosyaları çoğunlukla 0.5 MB civarı olacaktır. Eğer iki
kelime aranıyorsa bu ortalama 1 MB JSON dosya indirilmesi demektir,
hızlı bir şekilde yapılabilir.

Bahsedilen yaklaşımı bu site için kodladık, sonuçları [2]'de görebiliriz.

Kodlar Github deposunda [3] `invidx.py` ve `sk/ara.html` dosyaları
içinde.


Kaynaklar

[1] <a href="pyscript.html">PyScript</a>

[2] <a href="../../ara.html">Blog Arama Sayfasi</a>

[3] <a href="https://github.com/burakbayramli/classnotes">Github</a>

[4] <a href="https://towardsdatascience.com/benchmarking-python-nlp-tokenizers-3ac4735100c5">Benchmarking Python NLP Tokenizers</a>

