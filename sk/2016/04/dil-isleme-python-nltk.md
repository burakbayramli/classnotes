# Dil Isleme, Python - NLTK

Metin bazlı dokümanlar üzerinde dil işleme yapmayı kolaylaştıran bir
kütüphane Python NLTK - isim İngilizce doğal dil işlemi araç
çantasından (natural language toolkit)  geliyor. Özellikle yapay
öğrenim algoritmaları işlemeden dil metinleri üzerinde önişleme
(preprocessing) için faydalı. Bu algoritmaların çoğu bir dokümanı bir
"kelime çuvalı (bag-of-words)" olarak temsil eder (çuval kelimesi çok
uygun çünkü kelimelerin arasındaki sıra dikkate alınmaz, belli bir
kelimeden kaç tane olduğu sayılır ve bu bilgi "çuvala" atılır), ve bu
tür işlemler NLTK ile çok hızlı şekilde yapılabilir. Kurmak için

```
pip install nltk
```

Fakat NLTK içinde pek çok ek "model" var - bu modeller dil modelleri,
mesela fiillerin nasıl şekillendiği gibi şeyler. Bu ek modeller ek
dosyalar içinde ve ilk kuruluşa dahil değiller. Onları kurmak için bir
.py dosyasinda, ya da ipython komut satırından

```python
import nltk
nltk.download()
```

işletmek lazım, bu bir GUI başlatır. GUI içinde mesela Models | punkt
seçilir ve Install tıklanınca bu model indirilecektir.

Basit örnek

```python
import nltk, string
doc = 'The swimmer likes swimming so he swims.'
tokens = nltk.word_tokenize(doc.lower())
print tokens
```

Sonuç

```
['the', 'swimmer', 'likes', 'swimming', 'so', 'he', 'swims', '.']
```


Eğer nokta, virgül vs çıkartmak istersek,

```python
tokens = [i for i in tokens if i not in string.punctuation]
```

```
['the', 'swimmer', 'likes', 'swimming', 'so', 'he', 'swims']
```

Kök bulmak (stemming) denen bir işlem mesela fiillerden çekimi
çıkartır, böylece "yüzmek (swimming)" fiilinin tüm çekimleri aynı
yüzme eylemine eşlenir. Bu çok faydalı bir özellik. 

```python
stemmer = nltk.stem.porter.PorterStemmer()

def stem_tokens(tokens):

    return [stemmer.stem(item) for item in tokens]

    

tokens = stem_tokens(tokens)    
```

Sonuc

```python
[u'the', u'swimmer', u'like', u'swim', u'so', u'he', u'swim']
```

Kelime Çiftleri (Bigrams)

Bir dokümanda arka arkaya gelen ikili, üçlü, vs. kelime dizilerine
bakmak o dokümanı anlamak için çok faydalı oluyor, mesela açık
kelimesi var, yazılım kelimesi var, bu kelimeler ayrı ayrı pek çok
dokümanda geçiyor olabilirler, fakat "açık yazılım" kelime çifti arka
arkaya daha özel bir anlam taşır. NLTK ile bu çiftleri üretmek için
kod altta. Üretim ardından çiftlerin her biri dokümanı temsil eden
kelime çuvalında bir kolon haline gelir.

```python
bigrams = nltk.bigrams(tokens)
for x in bigrams: print (x)
```

```
('the', 'swimmer')
('swimmer', 'likes')
('likes', 'swimming')
('swimming', 'so')
('so', 'he')
('he', 'swims')
('swims', '.')
```









