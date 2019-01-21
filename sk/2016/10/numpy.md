# Numpy


Numpy




Matris islemleri icin kullanilan Numpy teknikleri altta. Yaziya ekler olacak

Filtreli Erisim

Numpy dizinleri icerigine gore filtrelemek isteniyorsa, bunun icin oz, kisa bir sozdizim var. Diyelim ki soyle bir dizin var:

import numpy
a = numpy.array([10, 20, 30, 40, 50])

Bu dizin icindeki 20 degerinden buyuk degerleri alip ekrana basmak icin

print a[a>20]

yeterli. Filtre sartlarini birbirine zincirlemek icin, mesela 10'dan buyuk 50'den kucuk a elemanlari:

print a[(a > 10) & (a < 50)]

Normalde Python "ve" icin "and" kelimesini kullanir, ama burada '&' isaretini kullanmislar.

Bir dizin icindeki filtreleme ibareleri, bir dizinden alinip, baska bir dizin uzerinde de uygulanabilir. Dizin 'a' uzerindeki filtreleri 'b' dizinine uygulamak icin

b = numpy.array([110, 120, 130, 140, 150])
print b[a>20]
print b[(a > 10) & (a < 50)]

Buradan gelecek sonuc

[130 140 150]
[120 130 140]

Icinde Istenen Sayi ile Baslatmak

Bu is icin ozel bir sozdizim yok, ama su kullanim ile ayni sonuc elde edilebilir:

a = np.nan * numpy.ones((N,N))

Bu komut icinde N x N boyutunde bir matrisi icinde nan degerleri olacak sekilde olusturacaktir. Herhangi baska bir sayi kullanilabilirdi.

Boyut Eklemek

Mevcut bir array tipine boyut eklemek icin, mesela 4 oge iceren bir vektoru (1,4) boyutunda bir matris yapmak icin

a = numpy.array([3,4,5,6])
print a[None,:]

Erisim indeksi yerine 'None' kullaninca ekstra bir boyut eklenmis oluyor.

Kopyalayarak Buyutmek

Bir dizini, matrisi belli bir yonde "kopyalayarak buyutmek" istiyorsak, resize komutu kullanilabilir.

b = numpy.array([3,4,5,6])
print numpy.resize(b, (4,4))

Sonuc

[[3 4 5 6]
[3 4 5 6]
[3 4 5 6]
[3 4 5 6]]

Polyfit

from numpy import *

x = array([1,2,3,4,5])
y = array([6, 11, 18, 27, 38])
print polyfit(x,y,1)

Sonuc

[ 8. -4.]

Numpy Matrix

Matris kesitleri (slices) uzerinde cok islem yapiyorsak, numpy.array yerine numpy.matrix kullanmak daha iyi olabilir; bu obje, Matlab'deki matrix objesi gibi davraniyor, kesitler uzerindeki boyutlar lineer cebire uygun sekilde veriliyor. Mesela,

a = np.array([[1,1,1],
              [2,2,2],
              [3,3,3]])
print a[:,1], a[:,1].shape

[1 2 3] (3,)

sonucunu verir. Yani 2. kolonu okumak istedik ve bize sadece "buyuklugu 3 olan" bir vektor geri geldi. Halbuki numpy.matrix kullansak,

b = np.matrix(a)
print b[:,1], b[:,1].shape

[[1]
 [2]
 [3]] (3, 1)

Yani (3,1) boyutunda bir matris parcasi geldi. Bu farklilik yanlis hesaplara bile yolacabilir. Mesela

print np.dot(a[:,1],np.ones((3,3))*2)

[ 12.  12.  12.]

sonucunu verir. Aslinda bu carpim yapilamamaliydi cunku (3,1) boyutu ile (3,3) boyutu matris carpimina sokulamaz. Herhalde np.dot(..) kafasina gore 3 buyuklugunu sagdaki carpilan (3,3) boyutuna uydurulabilecegini farz etti. Aslinda bu islemden hata gelmeliydi,

print np.dot(b[:,1],np.ones((3,3))*2)
..

ValueError: shapes (3,1) and (3,3) not aligned: 1 (dim 1) != 3 (dim 0)

ile oldugu gibi. Yapilabilecek tek carpim 

print np.dot(b[:,1].T,np.ones((3,3))*2)

ile yapilacakti.

Not: Biraz konu disinda ama eger carpimin her iki tarafindaki matris ayni boyutta ise onlari hucre bazinda carpmak icin (elementwise multiplication) bu iki matrisi numpy.array yapip * operatorunu kullanmak gerekebilir.

Vektorize Etmek

Python kulturu fonksiyonel kodlamaya yakindir, ayrica Numpy matematiksel hesaplari gerektirdigi icin o dunyada fonsiyonel cagrilarin matematiksel fonksiyon olarak gorme hem kodda hem tasarida islerin temiz olmasini saglar. Mesela bir Numpy vektorunun tum ogelerini toplamak icin for x in vec: sum += x gibi bir dongu  yazmayiz, direk sum(vec) cagrisi yapariz. Numpy tum ogeler uzerinde isleyecek bir metot saglamistir.

Eger kendi yazdigimiz bir fonksiyonun bu sekilde islemesini istiyorsak, onu "vektorize (vectorize)" edebiliriz. Mesela oyle bir matris var ki icinde string ogeleri var, bu ogelerden sayiya benzeyenleri float yapacagiz, gerisini sifirlayacagiz. Bu islem matrisin her hucresine uygulanacak, ve sonuc olarak ayni boyutta ama float icerikli yeni bir matris ortaya cikacak.

import numpy as np

def f(arg): 
    if "." in arg: 
        return np.float(arg)   
    else: 
        return 0

data = np.array(['elma','armut','23.42','99.9'])
f = np.vectorize(f,otypes=[np.float])
print f(data)

Sonuc

[  0.     0.    23.42  99.9 ]

Matris Gezmek

Numpy matrisi uzerindeki her degere birer birer ugrayip, ayni anda uzerinde oldugumuz indis degerleri ile beraber matris icerigini gormek istersek, np.ndenumerate tavsiye edilir.

A = [[1,2],[3,4]]

for i val in np.ndenumerate(A):
   print i, "deger", val

Ek bazi ilerlemeler:

Indis i icine gelen aslinda bir Python tuple; yani bir tur liste. Tuple yapisinin ozelligi onun kolayca iceriginin tekil degiskenlere atanabiliyor olmasi (unpacking). Fonksiyonlardan birden fazla deger geri dondururken zaten bu ozellilten faydalaniyoruz. Cagriyi yapan, cagrinin esitlik tarafinda birden fazla degisken tanimlayinca o degiskenlere otomatik olarak geri donulen degerler atanmis oluyor.

O zaman indis degerini de aninda "paketten cikartabiliriz":

import numpy as np

A = [[1,2],[3,4]]

for (x,y), val in np.ndenumerate(A):
   print x, y, "deger", val

Burada x,y'a i icinde olan x ve y kordinat degerleri tasiniyor olacak.

1xM Carpi MxM Carpi Mx1, N Kere

Eger ustte belirtilen gibi bir carpimi tek bir kere yapiyor olsaydik sonuc 1x1 olurdu. Ya bu islemi N tane satir icin yapmak istersek? Aklimiza bir fikir gelir, satirlari ust uste koyup bir matris icinde verirsem, ayni sonuc alirim! Fakat bu durumda NxM carpi MxM carpi MxN sonuc NxN olacaktir! Aradigimiz sonuclar sonuc matrisin kosegenindedir, evet, bu islem cok kulfetlidir, gereksizdir, ve N buyuk ise cok yer israf eder. Cozum ustte belirtilen caprimi N kere yapmak. einsum burada ise yarar,

r = np.array([[1,2]]).T
R = np.array([[2,2],[2,2]])

[[18]]

rs = np.array([[1,2],[4,4]]).T
print np.einsum('ij,ji->i',np.dot(rs.T,R),rs)

[[ 18  128]]








