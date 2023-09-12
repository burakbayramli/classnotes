# Statik Sitelerde Arama Motoru

Bu yazıdaki amacımız bir statik dizini aranır hale getirmek ve bunu
orta katman olmadan yapabilmek.

Dinamik Web siteleri üç katmana sahiptir. 1'inci katmanda istemci,
müşteri tarayıcısı ile 2'inci katmandaki servise bağlanır, müşteri
isteği (request) orada karşılamak için işlem mantığı koştulur, mesela
3'üncü katmandaki bir veri tabanına bağlanıp veri alışveri yapmak
gibi, ve sonuçta sayfa üretilir, kullanıcıya gösterilir.

Tarayıcı içindeki mekanizma o kadar aptal değildir, orada da kodlar
işletilebiliyor, bu katmandaki Javascript oldukça kuvvetli bir
dil. Javascript kodları sayfa içeriği ile beraber bağlanana
gönderilir, Javascript dilindeki bu kodlar prezentasyon üzerinde ek
işlemler yapabilir, bir sürü görsel işlem Javascript ile mümkün, hatta
bir süredir Ajax ile serviste direk fonksiyon çağrısı bile
yapılabiliyor.

Orta katman bir sürü işlem yapabiliyor, fakat eğer gereken kabiliyet
pür okuma bazlı ise ve gerekli veri dosyalarını hızlı erişilebilir
halde önceden hazırlamışsak, Tarayıcı -> Statik Dosya erişimi ile pek
çok işi yapmak mümkündür.

Arama motorunu işte bu şekilde kodlayacağız. Javascript, ya da bu
yazıda onun yerine PyScript [1], kodları tarayıcıda işleyecek,
çetrefil bir arayüz mantığı orada kodlanacak.. Bu mantık gerektiği
yerde gerekli kelimeler için belli indis dosyalarına bağlanacak,
onların içeriğine ek işlemler yapıp birleştirecek, ve sonuçları
sunacak. Bunun pür dosyalar ile nasıl yapılabileceğini anlamak için
önce tüm metni arama (full-text search) teknolojisine bakalım, ve bizim
yapacağımız eklerden bahsedelim.

# Tam Metin Arama

Doküman arama kelime bazlı yapılır, arama için girdiğimiz kelimeler
teker teker bir indis içinde bulunarak onların bağlı olduğu dokümanlar
getirilmeye uğraşılır. Verili bir doküman içindeki kelimeleri bulmak
mümkün, bir anlamda döküman bir kelime listesidir, ve doküman ID
verilip döküman bulunup içindeki kelime listesi dökülebilir. Tam metin
aramanın yaptığı bunun tersidir, kelime verilip içinde geçtiği
doküman(lar) bulunmaya uğraşılır, yani ters yönde gidiyoruz. Bunun
için farklı bir indis gerekir, bu indis yapısına uygun bir şekilde
tersyüz edilmiş indis (inverted index) ismi veriliyor.

Tam metin arama için tersyüz edilmiş indisi yaratmak gerekir, bunun
için tüm dokümanlar gezilir, dokümanlar alt kelimelerine, simgelere
(token) ayrılır, ve bu kelimelerden içinde geçtiği dokümanlara bir
işaret konur. Bunu bir sözlük yapısı içinde gerçekleştirebiliriz,
sözlük anahtarı kelime, değeri ise ikinci bir sözlük, bu sözlük içinde
o kelimenin hangi dokümanda kaç kez geçtiği olacak. Mesela bütün dokümanlar
içinde "araba" kelimesi doküman1 içinde 4 kere doküman2 içinde 7 kere
geçmişse, `{"araba": {"döküman1": 4, "doküman2": 7}.... }` gibi bir
sözlük yapısı görmeliyiz.

```python
import re

WORD = re.compile(r'\w+')

def clean_text(text):
    text = text.replace("\n"," ").replace("\r"," ")
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

```python
from collections import defaultdict
import json

text1 = "Search engine optimization is the process of improving the quality and quantity of website traffic to a website or a web page from search engines. SEO targets unpaid traffic rather than direct traffic or paid traffic."
text2 = "Note: This standard is not intended to dictate the internal formats used by sites for optimization, the specific message system features that they are expected to support, or any of the characteristics of user interface programs"

invidx = defaultdict(lambda: defaultdict(int))

for di,doc in enumerate([text1,text2]):
    for word in reg_tokenize(doc):
        if len(word) > 1: invidx[word][di] += 1

print (json.dumps(invidx)) 
```

```text

{"Search": {"0": 1}, "engine": {"0": 1}, "optimization": {"0": 1, "1":
1}, "is": {"0": 1, "1": 1}, "the": {"0": 2, "1": 3}, "process": {"0":
1}, "of": {"0": 2, "1": 2}, "improving": {"0": 1}, "quality": {"0":
1}, "and": {"0": 1}, "quantity": {"0": 1}, "website": {"0": 2},
"traffic": {"0": 4}, "to": {"0": 1, "1": 2}, "or": {"0": 2, "1": 1},
"web": {"0": 1}, "page": {"0": 1}, "from": {"0": 1}, "search": {"0":
1}, "engines": {"0": 1}, "SEO": {"0": 1}, "targets": {"0": 1},
"unpaid": {"0": 1}, "rather": {"0": 1}, "than": {"0": 1}, "direct":
{"0": 1}, "paid": {"0": 1}, "Note": {"1": 1}, "This": {"1": 1},
"standard": {"1": 1}, "not": {"1": 1}, "intended": {"1": 1},
"dictate": {"1": 1}, "internal": {"1": 1}, "formats": {"1": 1},
"used": {"1": 1}, "by": {"1": 1}, "sites": {"1": 1}, "for": {"1": 1},
"specific": {"1": 1}, "message": {"1": 1}, "system": {"1": 1},
"features": {"1": 1}, "that": {"1": 1}, "they": {"1": 1}, "are": {"1":
1}, "expected": {"1": 1}, "support": {"1": 1}, "any": {"1": 1},
"characteristics": {"1": 1}, "user": {"1": 1}, "interface": {"1": 1},
"programs": {"1": 1}}

```




















```python
from collections import defaultdict
from unidecode import unidecode
import re, json, glob

WORD = re.compile(r'\w+')

def clean_text(text):
    text = text.replace("\n"," ").replace("\r"," ")
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

invidx = defaultdict(lambda: defaultdict(int))
dir = "[DIR]"

files = glob.glob(dir + "/**/**/*.md")
files = sorted(files)
for file in enumerate(files):
    doc = file[1].replace(dir,"")    
    for word in reg_tokenize(open(file[1]).read()):
        word = unidecode(word).lower()
        invidx[word][doc] += 1
    print (doc)

fout = open("data/invidx.json","w")
fout.write(json.dumps(invidx))
fout.close()
```








```python
invidx = json.loads(open("data/invidx.json").read())
invidx_dict = {}

for c in  string.ascii_lowercase:
    invidx_dict[c] = defaultdict(lambda: defaultdict(int))

for k,v in invidx.items():
    first_letter = k[0]
    if first_letter in string.ascii_lowercase:
        invidx_dict[first_letter][k] = v

for k,v in invidx_dict.items():
    fout = open("data/invidx-%s.json" % k,"w")
    fout.write(json.dumps(v))
    fout.close()    
```





```python
#points = [0, 12, 9]
#res = sorted(range(len(points)), key=lambda x: points[x])
#print(res)
```

```python
#a = [1, 2, 3, 4]; b = [2, 3, 4, 5]; c = [3, 4, 5, 6]; d = [4, 1, 3, 9]
#u = set.intersection(set(a),set(b),set(c),set(d))
#print (u)

search = "green ammonia"

stok = search.split()

stok_hits = {}

results = []
for tok in stok:
    stok_hits[tok] = json.loads(open("data/invidx-%s.json" % tok[0]).read())[tok]
    results.append(set(stok_hits[tok]))
    
u = set.intersection(*results)

hits = []
for f in u:
    hits.append([f,sum([stok_hits[tok][f] for tok in stok])])
    
sorted_hits = sorted(range(len(hits)), key=lambda x: hits[x][1], reverse=True)

for i in range(20):
    print (hits[sorted_hits[i]])
```




```html
<head>
    <script defer src="pyscript.js"></script>
</head>
<body>    
      <py-script>
        from pyodide.http import open_url
        from collections import defaultdict
        import json

        search = "green ammonia"

        stok = search.split()

        stok_hits = {}

        results = []
        
        for tok in stok:
           url = 'http://192.168.43.49:5000/static/invidx-%s.json' % tok[0]
           df = open_url(url)        
           stok_hits[tok] = json.loads(df.getvalue())[tok]
           results.append(set(stok_hits[tok]))


        u = set.intersection(*results)

        hits = []
        
        for f in u:
           hits.append([f,sum([stok_hits[tok][f] for tok in stok])])
    
        sorted_hits = sorted(range(len(hits)), key=lambda x: hits[x][1], reverse=True)

        for i in range(20):
           display (hits[sorted_hits[i]])
           
      </py-script>
</body>
</html>
```






```html
<div id="output"></div>

<py-script>
  def write_to_page():
     manual_div = Element("output")
     inp = Element("search")
     manual_div.element.innerText = inp.element.value
</py-script>

<p>
 <input type="text" name="search" id="search"/>
</p>
<p>
  <button py-click="write_to_page()" id="manual">Search</button>
</p>
```







[devam edecek]

Kaynaklar

[1] <a href="pyscript.html">PyScript</a>

