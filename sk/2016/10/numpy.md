# Numpy

Matris işlemleri için kullanılan Numpy teknikleri altta. Yazıya ekler olacak

Filtreli Erisim

Numpy dizinleri içeriğine göre filtrelemek isteniyorsa, bunun için öz,
kısa bir sözdizim var. Diyelim ki şöyle bir dizin var:

```python
import numpy
a = numpy.array([10, 20, 30, 40, 50])
```

Bu dizin içindeki 20 değerinden büyük değerleri alıp ekrana basmak için

```python
print a[a>20]
```

yeterli. Filtre şartlarını birbirine zincirlemek için, mesela 10'dan
büyük 50'den küçük a elemanları:

```python
print a[(a > 10) & (a < 50)]
```

Normalde Python "ve" icin "and" kelimesini kullanir, ama burada '&'
isaretini kullanmislar.

Bir dizin içindeki filtreleme ibareleri, bir dizinden alinip, başka
bir dizin üzerinde de uygulanabilir. Dizin 'a' üzerindeki filtreleri
'b' dizinine uygulamak için

```python
b = numpy.array([110, 120, 130, 140, 150])
print b[a>20]
print b[(a > 10) & (a < 50)]
```

Buradan gelecek sonuç

```
[130 140 150]
[120 130 140]
```

İçinde İstenen Sayı ile Başlatmak

Bu iş için özel bir sözdizim yok, ama şu kullanım ile aynı sonuç elde
edilebilir:

```python
a = np.nan * numpy.ones((N,N))
```

Bu komut içinde N x N boyütünde bir matrisi içinde nan değerleri
olacak şekilde oluşturacaktır. Herhangi başka bir sayı
kullanılabilirdi.

Boyut Eklemek

Mevcut bir array tipine boyut eklemek için, mesela 4 öğe içeren bir
vektörü (1,4) boyutunda bir matris yapmak için

```python
a = numpy.array([3,4,5,6])
print a[None,:]
```

Erişim indeksi yerine 'None' kullanınca ekstra bir boyut eklenmiş oluyor.

Kopyalayarak Büyütmek

Bir dizini, matrisi belli bir yönde "kopyalayarak büyütmek"
istiyorsak, resize komutu kullanılabilir.

```python
b = numpy.array([3,4,5,6])
print numpy.resize(b, (4,4))
```

Sonuc

```
[[3 4 5 6]
[3 4 5 6]
[3 4 5 6]
[3 4 5 6]]
```

Polyfit

from numpy import *

```python
x = array([1,2,3,4,5])
y = array([6, 11, 18, 27, 38])
print polyfit(x,y,1)
```

Sonuc

```
[ 8. -4.]
```

Numpy Matrix

Matris kesitleri (slices) üzerinde çok işlem yapıyorsak, numpy.array
yerine numpy.matrix kullanmak daha iyi olabilir; bu obje, Matlab'deki
matrix objesi gibi davranıyor, kesitler üzerindeki boyutlar lineer
cebire uygun şekilde veriliyor. Mesela,

```python
a = np.array([[1,1,1],
              [2,2,2],
              [3,3,3]])
print a[:,1], a[:,1].shape
```

```
[1 2 3] (3,)
```

sonucunu verir. Yani 2. kolonu okumak istedik ve bize sadece
"büyüklüğü 3 olan" bir vektör geri geldi. Halbuki numpy.matrix
kullansak,

```python
b = np.matrix(a)
print b[:,1], b[:,1].shape
```

```
[[1]
 [2]
 [3]] (3, 1)
```

Yani (3,1) boyutunda bir matris parçası geldi. Bu farklılık yanlış
hesaplara bile yolaçabilir. Mesela

```python
print np.dot(a[:,1],np.ones((3,3))*2)
```

```
[ 12.  12.  12.]
```

sonucunu verir. Aslında bu çarpım yapılamamalıydı çünkü (3,1) boyutu
ile (3,3) boyutu matris çarpımına sokulamaz. Herhalde np.dot(..)
kafasına göre 3 büyüklüğünü sağdaki çarpılan (3,3) boyutuna
uydurulabileceğini farz etti. Aslında bu işlemden hata gelmeliydi,

```python
print np.dot(b[:,1],np.ones((3,3))*2)
```

```
ValueError: shapes (3,1) and (3,3) not aligned: 1 (dim 1) != 3 (dim 0)
```

ile olduğu gibi. Yapılabilecek tek çarpım 

```python
print np.dot(b[:,1].T,np.ones((3,3))*2)
```

ile yapılacaktı.

Not: Biraz konu dışında ama eğer çarpımın her iki tarafındaki matris
aynı boyutta ise onları hücre bazında çarpmak için (elementwise
multiplication) bu iki matrisi numpy.array yapıp * operatörünü
kullanmak gerekebilir.

Vektorize Etmek

Python kültürü fonksiyonel kodlamaya yakındır, ayrıca Numpy
matematiksel hesapları gerektirdiği için o dünyada fonsiyonel
çağrıların matematiksel fonksiyon olarak görme hem kodda hem tasarıda
işlerin temiz olmasını sağlar. Mesela bir Numpy vektörünün tüm
öğelerini toplamak için for x in vec: sum += x gibi bir döngü
 yazmayız, direk sum(vec) çağrısı yaparız. Numpy tüm öğeler üzerinde
işleyecek bir metot sağlamıştır.

Eğer kendi yazdığımız bir fonksiyonun bu şekilde işlemesini
istiyorsak, onu "vektorize (vectorize)" edebiliriz. Mesela öyle bir
matris var ki içinde string öğeleri var, bu öğelerden sayıya
benzeyenleri float yapacağız, gerisini sıfırlayacağız. Bu işlem
matrisin her hücresine uygulanacak, ve sonuç olarak aynı boyutta ama
float içerikli yeni bir matris ortaya çıkacak.

```python
import numpy as np

def f(arg): 
    if "." in arg: 
        return np.float(arg)   
    else: 
        return 0

data = np.array(['elma','armut','23.42','99.9'])
f = np.vectorize(f,otypes=[np.float])
print f(data)
```

Sonuc

```
[  0.     0.    23.42  99.9 ]
```

Matris Gezmek

Numpy matrisi üzerindeki her değere birer birer uğrayıp, aynı anda
üzerinde olduğumüz indis değerleri ile beraber matris içeriğini görmek
istersek, np.ndenümerate tavsiye edilir.

```python
A = [[1,2],[3,4]]

for i val in np.ndenumerate(A):
   print i, "deger", val
```

Ek bazı ilerlemeler:

İndis i içine gelen aslında bir Python tüple; yani bir tür
liste. Tüple yapısının özelliği onun kolayca içeriğinin tekil
değişkenlere atanabiliyor olması (unpacking). Fonksiyonlardan birden
fazla değer geri döndürürken zaten bu özellilten
faydalanıyoruz. Çağrıyı yapan, çağrının eşitlik tarafında birden fazla
değişken tanımlayınca o değişkenlere otomatik olarak geri dönülen
değerler atanmış oluyor.

O zaman indis değerini de anında "paketten çıkartabiliriz":

```python
import numpy as np

A = [[1,2],[3,4]]

for (x,y), val in np.ndenumerate(A):
   print x, y, "deger", val
```

Burada x,y'a i içinde olan x ve y kordinat değerleri taşınıyor olacak.

1xM Çarpı MxM Çarpı Mx1, N Kere

Eğer üstte belirtilen gibi bir çarpımı tek bir kere yapıyor olsaydık
sonuç 1x1 olurdu. Ya bu işlemi N tane satır için yapmak istersek?
Aklımıza bir fikir gelir, satırları üst üste koyup bir matris içinde
verirsem, aynı sonuç alırım! Fakat bu durumda NxM çarpı MxM çarpı MxN
sonuç NxN olacaktır! Aradığımız sonuçlar sonuç matrisin
köşegenindedir, evet, bu işlem çok külfetlidir, gereksizdir, ve N
büyük ise çok yer israf eder. Çözüm üstte belirtilen caprimi N kere
yapmak. `einsum` burada ise yarar,

```python
r = np.array([[1,2]]).T
R = np.array([[2,2],[2,2]])
```

```
[[18]]
```

```python
rs = np.array([[1,2],[4,4]]).T
print (np.einsum('ij,ji->i',np.dot(rs.T,R),rs))
```

```
[[ 18  128]]
```

Iki Dizini Ust Uste Dizmek

İki dizini üst üste dizmek (staçking) için `vstack` (dikey) ve `hstack`
(yatay) fonksiyonları var. Bu fonksiyonlar ilginç şekillerde
kullanılabiliyor: Mesela [100,100] 2 boyutlu başlangıç noktasından
x-kord. ikiser ikiser, y-kord. üçer üçer büyüyecek şekilde 5 tane veri
noktası üretmek istesek:

```
x0 = array([100, 100])
xs = vstack((arange(5)*3, arange(5)*2)).T + x0
```

yeterli. arange(N) O..N-1 arasında sayıları üretir. Bu sayıların
hepsini 3 ile çarpıyoruz. Sonra aynısını yapıp 2 ile çarpıyoruz. Bu
iki dizini üst üste "yiğiyoruz", ve .T çağrısı ile devriğini
(transpose) alıyoruz, böylece [5,2] boyutlu veri noktalarını elde
ediyoruz. Tüm bunlara x0 başlangıç değerini ekleyince istediğimiz sonuç
geliyor.

```
[[100 100][103 102][106 104][109 106][112 108]]
```

Bir dizini "kesmek" icin slice() fonksiyonu var.

```
a = [1,2,3,4,5,6,7,8,9]
sl = slice(2,8,2)
```

Üstteki slice tanımı 2. öğe ile 8. öğe (hariç olmak üzere) arasındaki
tüm elemanları geri getirir. Eğer son 3. parametre "2" verilirsek, bu
"ikiser ikiser git" anlamına geliyor, yani bir öğe sürekli
atlanır. print a[sl] çağrısı bize [3, 5, 7] sonucunu döndürecek.Aynı
çağrı print a[2:8:2] şeklinde de gerçekleştirilebilir. Bazen
değişkenler kullanılarak slice() objeleri yaratmak gerekebiliyor, bu
durumlarda slice() çağrısı tercih edilmekte.

### Loadtxt ve Converter

CSV dosyası diye bilinen boşluk, virgül, vs. ile ayrılmış satırsal,
düz text bazlı dosyaları Loadtxt ile yüklüyoruz. Eğer bu yükleme
sırasında mesela bir float olması gereken bir hücrede 'NA' gibi bir
string bazlı değer varsa, loadtxt bu değeri float yapamadığı için
şikayet edecektir. NA bilindiği gibi "değer yok" anlamına gelen "not
applicable" kelimesinden geliyor; Numpy içinde buna tekabül eden bir
float değeri var (Nump.nan diye erışılıyor), o zaman String görünce bu
değere dönüşümü bizim yapmamız lazım.

Alttaki kod neş.dat adlı bir dosyayı okur, 1. satırı atlar, ve sadece
11 ve 32. kolonlarını çekip çıkartır, bu kolonların her ikisinde de
bazen NA metni görülürse, onu `converter()` fonksiyonu üzerinden
numpy.nan değerine çevirir.

```python
import numpy as np

def converter(x):
  if x == 'NA':
      return np.nan
    else:
       return float(x)
       
nes = np.loadtxt("nes.dat", skiprows=1, usecols = (11,32), converters={11:converter, 32:converter})
```

Ekler

[Grafiklemek, Meshgrid](../../2020/02/grafiklemek.html)

[Histogram Numaraları](../../2015/10/histogram-numaralari.html)

[Sembolik Matematik - Sympy](../../2011/04/sympy.html)

[Ubuntu Server ve Matplotlib](../../2010/11/ubuntu-server-ve-matplotlib.html)

[Grafiklemek, Matplotlib, Pandas](../../2020/02/grafiklemek.html)



