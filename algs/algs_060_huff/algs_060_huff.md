# Huffman Kodlamasi ile Veri Sıkıştırma (Compression)

Huffman kodlaması bir **veri sıkıştırma** yoludur.  1952 yılında David
A. Huffman tarafından geliştirildi, **kayıpsız veri sıkıştırma**
algoritmasıdır. Yani, sıkıştırılan veriler açıldığında orijinal
haliyle tamamen aynı olur; hiçbir bilgi kaybı yaşanmaz. Bu yöntem,
verilerdeki karakterlerin veya sembollerin frekanslarına
(sıklıklarına) dayalı olarak çalışır.

Peki nasıl? Huffman algoritması, metindeki veya verideki her bir
karaktere farklı uzunluklarda ikili (binary) kodlar atar. Sık
kullanılan karakterlere daha kısa kodlar, daha az kullanılan
karakterlere ise daha uzun kodlar atanır. Bu sayede, genel veri
boyutunda önemli bir azalma sağlanır.

Huffman kodlamasının temel çalışma prensibi bir **Huffman ağacı**
oluşturmaya dayanır:

1.  **Frekans Sayımı:** İlk olarak, sıkıştırılacak verideki her
karakterin kaç kez geçtiği sayılır ve frekansları belirlenir.

2.  **Düğüm Oluşturma:** Her karakter ve frekansı için bir "yaprak"
düğüm oluşturulur.

3.  **Ağaç İnşası:** En düşük frekansa sahip iki düğüm birleştirilerek
yeni bir "iç" düğüm oluşturulur. Bu yeni düğümün frekansı, birleşen
iki düğümün frekanslarının toplamıdır. Bu işlem, tek bir kök düğüm
kalana kadar tekrarlanır.

4.  **Kod Ataması:** Ağacın kökünden yapraklara doğru ilerlenerek her
karaktere bir ikili kod atanır. Sola gidildiğinde '0', sağa
gidildiğinde '1' değeri verilir. Bu işlem sonucunda, sık kullanılan
karakterler kısa kodlara, az kullanılanlar ise uzun kodlara sahip
olur.

Örneğin, "AAABBC" gibi bir metni ele alalım:

* A: 3 kez (yüksek frekans)
* B: 2 kez (orta frekans)
* C: 1 kez (düşük frekans)

Huffman kodlaması ile 'A'ya kısa bir kod (örneğin "0"), 'B'ye biraz
daha uzun (örneğin "10"), 'C'ye ise en uzun kod (örneğin "11")
atanabilir. Böylece, orijinal "AAABBC" metni daha kısa bir ikili
diziye dönüşür ve depolama alanı azalır.

Neden Önemli?

Huffman kodlaması, özellikle metin dosyaları, görüntü formatları
(örneğin JPEG'in bazı aşamalarında) ve ses dosyaları gibi birçok
alanda yaygın olarak kullanılır. Önemli avantajları şunlardır:

* **Kayıpsız Sıkıştırma:** Veri kalitesinden ödün vermeden dosya
    boyutunu küçültür.

* **Verimli Alan Kullanımı:** Depolama alanından tasarruf sağlar.

* **Hızlı Veri Transferi:** Daha küçük dosyalar, ağ üzerinden daha
    hızlı transfer edilebilir.

* **Geniş Uygulama Alanı:** Çeşitli dosya türleri ve uygulamalar için
    uygundur.

Huffman kodlaması, dijital dünyada veri yönetimini kolaylaştıran ve
kaynakları daha verimli kullanmamızı sağlayan basit ama güçlü bir
algoritmadır. Gelişen teknolojiyle birlikte veri miktarının artması,
bu tür sıkıştırma tekniklerinin önemini her geçen gün daha da
artırmaktadır. Bir dahaki sefere bir dosya sıkıştırdığınızda, arka
planda çalışan bu akıllı algoritmaları hatırlayın!

Algoritma [1, sf. 159]

```python
from heapq import heapify, heappush, heappop
from itertools import count
def huffman(seq, frq):
    num = count()
    trees = list(zip(frq, num, seq))
    heapify(trees)
    while len(trees) > 1:
       fa, _, a = heappop(trees)
       fb, _, b = heappop(trees)
       n = next(num)
       heappush(trees, (fa+fb, n, [a, b]))
    return trees[0][-1]

seq = "abcdefghi"
frq = [4, 5, 6, 9, 11, 12, 15, 16, 20]
tree = huffman(seq, frq)
print (tree)
```

```
[['i', [['a', 'b'], 'e']], [['f', 'g'], [['c', 'd'], 'h']]]
```

```python
def codes(tree, prefix=""):
    if len(tree) == 1:
        yield (tree, prefix)                    # A leaf with its code
        return
    for bit, child in zip("01", tree):          # Left (0) and right (1)
        for pair in codes(child, prefix + bit): # Get codes recursively
            yield pair
	    
print (list(codes(tree)))
```

```
[('i', '00'), ('a', '0100'), ('b', '0101'), ('e', '011'), ('f', '100'), ('g', '101'), ('c', '1100'), ('d', '1101'), ('h', '111')]
```

Kaynaklar

[1] Heatland, *Python Algorithms*


