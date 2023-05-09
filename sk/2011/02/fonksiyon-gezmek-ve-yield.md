# Fonksiyon Gezmek ve Yield

Bir vektörün, listenin elemanlarını gezebildiğimizi (iteratation) biliyoruz.

Python'un üretici (generatörs) özelliği ile bir fonksiyonu gezmek te mümkün.

Bu nasıl oluyor? Hokus pokus yield adı verilen bir komutta.

Bir fonksiyon return çağrısı ile bir değer döndürebilir. Fakat o
noktada o fonksiyondan dışarı çıkmış oluruz yani o fonksiyon ile
işimiz biter. Kıyasla yield ile bir değer döndürürüz, geriye bir sonuç
gelir ama fonksiyonun yield anındaki hali dondurulur -- yani
unutulmaz. Fonksiyondan dışarı çıkmış olsak bile geri dönebiliriz, ve
lokal değişkenleri oldukları halde kullanmaya devam edebiliriz:
Alttaki fonksiyona bakalım:

```python
def fib():
a, b = 0, 1
while 1:
  yield b
  a, b = b, a+b
```

Bu fonksiyon Fibonaccı sayılarını üretir, Fibonaccı sayıları 1,1,2,3
diye gider, bir sonraki sayı her zaman önceki iki sayının
toplamıdır. Görüldüğü gibi yield ile en son toplamı donduruyoruz,
fonksiyondan çıkıyoruz, fakat gezme işlemi bir sonraki değeri
istediğinde fib()'e geri dönülüyor ve işlem yield'den bir sonraki
satırdan, yani kaldığı yerden devam ediyor. Fonksiyon içindeki tüm
lokal değişkenler hala değerlerini koruyorlar.

Kullanmak için `fib()` çağrısını sanki bir listeymiş gibi çağırabiliriz:

```python
for x in fib(): print x
```

Yukarıda anlatılan türden değer gezme olanağının faydası nedir? Kod
düzenlemesi açısından getirdiği faydalar ötesinde (temizlik gibi,
geçici bir liste objesine gerek yok), performans bakımından faydası
olabilir. Bir şekilde hesaplanan ve üretilen elemanları gezmek için
onların önceden bir listeye doldurulmuş olması gerekmiyor -- her
eleman fonksiyon tarafından "gerektiği anda" hesaplanıp teker teker
alınıyor, ve bu bir tür "tembel (lazy)" hesaplama olarak
görülebilir. CPU bedelini her Fibonaccı sayısını üretirken sadece o
sayı için ödüyoruz. 10 tane önceden üretip bir yerde bekletmiyoruz.

Üreticilerin ne kadar kuvvetli özelliklerinin olduğunu tekrar
belirtmek iyi olur: içinde yield içeren bir fonksiyonu aslında bir
bakıma konumu olan bir obje gibi bile düşünebiliriz, daha doğrusu
onlar bir bakıma "konumlu fonksiyonlar" oluyorlar. Mesela azar azar
işlem yapan ortamlarda tipik bir ihtiyaç bir dosyanın açılması, ve o
dosyadan azar azar satır alınabilmesi.

Üretici yaklaşımı ile bu iş şöyle yapılır, in.csv dosyası şöyle olsun

```
y,x1,x2
1,6,10
2,5,20
3,4,30
4,3,40
5,2,50
```

Kod


```python
import csv

def get_row(cols):

    with open("in.csv", 'r') as csvfile:

        rd = csv.reader(csvfile)

        headers = {k: v for v, k in enumerate(next(rd))}

        for row in rd:

            label = float(row[headers['y']])

            # sadece istenen kolonlari al

            rrow = [row[headers[x]] for x in headers if x in cols]

            yield rrow, label
```

Bu akıllı fonksiyon aynen bir class gibi yaratılıyor, sonra
çağrılıyor. Mesela ilgilendiğimiz belli bir kolon listesini verip
geziciyi oluşturabiliriz, sonra ondan bir satır isteyebiliriz,


```python
getter = get_row(['x1','x2'])

Y,x = getter.next() 

print Y,x
```

Şu sonucu görürüz,


```
['10', '6'] 1.0
```

Şu anda fonksiyon bekler durumda. Biz başka çağrılar yapabiliriz,
gidip kahve içebiliriz, geri geldiğimizde

```python
Y,x = getter.next() 

print Y,x

Y,x = getter.next() 

print Y,x
```

çağrıları bize iki yeni satır verir. 


```
['20', '5'] 2.0

['30', '4'] 3.0
```

Elimizdeki fonksiyon göstergecini başka birini versek o next() çağırsa
o da aynı yerden devam ederdi. Ama biz tekrar başa dönmek istiyoruz,
problem değil, yeni bir gezici fonksiyon yaratırız, 


```python
getter = get_row(['x1','x2'])

Y,x = getter.next() 

print Y,x
```

```
['10', '6'] 1.0
```


Başa dönmüş olduk.

Üreticilerin gezme işlemini başlatmadan önce bazı hazırlık işleri,
atamaları yapmaları en önemli özelliklerinden. Aynen bir obje
başlatıldığında onun kurucusunda başlangıç kodları işletilebildiği
gibi, ilk yield anına kadar işletilen her şey bir fonksiyonda hazırlık
aşaması sayılabilir, ondan sonra bir döngüye girilip daha az mıktarda
kod sürekli işlenip yield ile dönülecektir. Üstteki örnekte dosyanın
açılması, başlık kolonların okunması, vs gibi işlemlerin hepsi bu
başlangıç aşamasında. Bir gezici fonksiyonu ismiyle ve istenen
parametreleri ile yarattığımız zaman bu hazırlık kodları işleyecek. 

Kaynaklar

http://www.python.org/dev/peps/pep-0255/






