# String Yapısı ve Python

Metin bilgisini depolayabilen Python `str` yani kelime dizisi string
yapısına bakalım. Bu tip temel olarak değiştirilemez (immutable) bir
tiptir, yani bir kere bir kelime, cümle bir obje haline geldiğine
içeriği değiştirilemez, ona yapılan ekler, işlemler *yeni* bir `str`
yaratacaktır.

### String Birleştirmek

Python ile String parcalarini biraraya getirmenin birkac yolu
var. Bunlardan en basiti ve ilk akla geleni + isaretini kullanmaktir:

```python
s1 = "aaa" + "-" + "bbb" + "-" + "ccc"
print (s1)
```

```text
aaa-bbb-ccc
```

Diğer bir yöntem yerine geçen String (subsitution) yöntemi. Bu yöntem
aslında C/++ dilinde bilinen `sprintf` mantığına benziyor. Format
belirleyen bir kısım var, bir de formatın tanımladığı yerlere yeni
değerler parametre olarak geçilen değerler var, ve bu değerler ile
yeni bir String oluşturuluyor. Örnek:

```python
s2 = "%s-%s-%s" % ("aaa","bbb","ccc")
print (s2)
```

```text
aaa-bbb-ccc
```

Bu kullanımın birkaç avantajı var, String tipini temsil eden `%s`
yerine diğer tipler de kullanılabilir, mesela float'ları temsil eden
`%f`. O zaman hem String birleştirme hem de tiplere göre formatlama
aynı anda yapılabilecektir.

Örnek

```python
s3 = "Burada bir float deger var: %f" % (3.43455)
print (s3)
```

```text
Burada bir float deger var: 3.434550
```

`%f` daha sofistike şekilde de kullanılabilir. Mesela: `s = "Burada
bir float değer var: %3.2f" % (3.43455)`. Bu formatlamaya göre float
değerini noktadan sonra sadece 2 basamak olacak şekilde ayarladık. O
zaman sonuç: `Burada bir float değer var: 3.43` olacaktır

### Eşitlik

Bir `str` diğerine ne zaman eşittir? Eğer içerikleri aynı ise,

```python
s5 = "bir kelime"
s6 = "bir kelime"
print (s5 == s6)
```

```text
True
```

```python
print (s5 == "baska bir kelime")
```

```text
False
```

Bazen pür eşitlik aşırı olabilir, bir kelimeyi diğeri içinde varsa eşit kabul
etmek için `in` kullanılabilir,

```python
print ("bir" in s5)
```

```text
True
```

### Bir Dizin Olarak String

Fakat sonuçta `str` tipinin bir harf dizini olduğunu unutmayalım. Eğer bu dizini
aynen bir listeyi gezer gibi gezmek istersek bunu yapabiliriz,

```python
for s in s5: print (s)
```

```text
b
i
r
 
k
e
l
i
m
e
```

Ve doğal olarak indis bazlı erişim de işleyecektir,

```python
s5[4]
```

```text
Out[1]: 'k'
```

```python
s5[-1]
```

```text
Out[1]: 'e'
```

### Harf Bloklarını Değiştirmek

Bu işlem için `replace` çağrısı yeterli,

```python
s4 = "bir berber bir berbere ne demis"
s4.replace("ne demis","gel beraber berber dukkan acalim demis")
```

```text
Out[1]: 'bir berber bir berbere gel beraber berber dukkan acalim demis'
```

### Harf(lerin) Yerini Bulmak

```python
s4.find("berber")
```

```text
Out[1]: 4
```

Bu çağrı aranan metnin ilk görüldüğü yerin indisini döndürdü. Diğer
`berber` kelimelerini bulmak için `find` çağrısını bir aralık üzerinde
işletmek mümkündür, yani başlangıç, bitiş değerleri verebiliriz,
üstteki örnekte ilk bulunan kısmı atlayarak oradan itibaren arama
yaparsak, bir sonraki uyumun yeri döndürülür,

```python
s4.find("berber",5,len(s4))
```

```text
Out[1]: 15
```

Aslında sadece başlangıç değeri verilebilirdi, bitiş verilmezse kelime sonu
kabul edilir,

```python
s4.find("berber",5)
```

```text
Out[1]: 15
```

### ASCII Haline Getirmek

Her dilden farklı karakterleri ascii haline çevirmek için, `unidecode`
paketi var,

```python
from unidecode import unidecode
print (unidecode(u"çagrı"))
```

```text
cagri
```

olacak, yani TR karakterleri İngilizce harflerden en yakın olanlarına
tercüme edilecek. Bu niye yapılır? Belki metin arama yaparken lazım
olabilir.



