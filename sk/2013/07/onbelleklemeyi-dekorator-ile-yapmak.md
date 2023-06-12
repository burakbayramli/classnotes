# Dekoratörler, Önbellek Kodlaması, Fonksiyon Değiştirmek

Bir fonksiyonu çağırıyoruz, ona bazı parametreler geçiyoruz, bu
parametrelerle fonksiyon bir hesap yapıyor, bize sonucu veriyor.
Mesela bir sayıya kadar olan tüm tam sayıları toplayan bir fonksiyon
olsun,

```python
def n_topla1(N):
   tmp = list(range(N+1))
   print (tmp)
   res = np.sum(tmp)
   return res

print (n_topla1(10))
print (n_topla1(5))
print (n_topla1(10))
```

```text
[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
55
[0, 1, 2, 3, 4, 5]
15
[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
55
```

Toplam 1 + 2 + 3 + .. + 10 diye 10'a kadar olan sayıları topladı ve
döndürdü.  Sonra 5 ve tekrar 10 için aynı teknik kullanıldı.

Fakat diyelim ki bu fonksiyonu pek çok kez ardı ardına çağırmak
gerekiyor, ve çağrıların çoğu benzer parametreleri kullanacak, mesela
10'a kadar olan toplam pek çok kez yapılabilecek.. Acaba üstteki
toplam işlemini bir kez yapıp ikinci, üçüncü çağrılarda aynı hesabı
döndürsek olmaz mı?

Koda böyle bir ek yapılabilir. Mesela `n_topla1` fonksiyonunda daha
başka bir şey yapmadan önce parametreleri biraraya koyarak bir tür
anahtar oluşturabiliriz, bu anahtarı bir sözlükte arama için
kullanırız, eğer değer bulunursa birisi önceden o hesabı yapıp oraya
koymuş demektir, fonksiyonda devam etmek yerine sözlükteki değeri
döndürürüz, hesaba gerek kalmaz. Tabii ki eğer sözlükte o değer yoksa,
hesabı yapıp sözlüğe bizim koymamız gerekir, böylece bir sonraki çağrı
yapan o değerleri bulsun.

"Parametrelerden anahtar oluşturmak", "varsa döndürmek yoksa oraya
koymak" - burada bir sürü hamaliyesi fazla kodlama var. Bu kodları bir
paket üzerinden, hatta bir fonksiyon başına koyulacak bir etiket /
işaret / dekoratör üzerinden Python'a yaptırsak iyi olacak.

### Hazır Paket Kullanarak

Paket `cachetools` içinde böyle kodlar var [1]. Toplam foksiyonunu etiketleyelim,

```python
from cachetools import cached

@cached(cache={})
def n_topla2(N):
   tmp = list(range(N+1))
   print (N, "=>", tmp)
   res = np.sum(tmp)
   return res

print (n_topla2(5))
print (n_topla2(10))
print (n_topla2(10))
```

```text
5 => [0, 1, 2, 3, 4, 5]
15
10 => [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
55
55
```
10 toplamı önbellekten geldi, onun listesi basılmadı dikkat edersek
çünkü fonksiyonun o kısmına gidilmesi gerekmedi.

Önbellekleme etiketi `@cached` ile kullanılacak sözlük tipini de
tanımlayabiliyoruz, üstteki örnekte standart bir Python sözlüğü `{}`
kullanıldı. Başka türlü sözlükler de var, bu sözlükler ayrıca
önbellekleme stratejisini değiştirmemize yarıyor. Mesela "sadece en
son 2 konulan öğe hatırlansın" istiyorsam, yani büyüklüğü 2'den fazla
olmasın, ve üçüncü öğeyi koymaya çalışırsam ilk eklediğim atılsın
istiyorsam, bu bir ilk giren ilk çıkar (first in first out -FIFO-)
mantığıdır, ve böyle bir sözlük tipi vardır, `FIFOCache`.


```python
from cachetools import FIFOCache

@cached(cache=FIFOCache(maxsize=2))
def n_topla2(N):
   tmp = list(range(N+1))
   print (N, "=>", tmp)
   res = np.sum(tmp)
   return res

print ('5 Toplami', n_topla2(5))
print ('10 Toplami',n_topla2(10))
print ('20 Toplami',n_topla2(20))

# tersten sor, son girenleri bulalim
print ('20 Toplami',n_topla2(20))
print ('10 Toplami',n_topla2(10))
print ('5 Toplami', n_topla2(5))
```

```text
5 => [0, 1, 2, 3, 4, 5]
5 Toplami 15
10 => [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
10 Toplami 55
20 => [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20]
20 Toplami 210
20 Toplami 210
10 Toplami 55
5 => [0, 1, 2, 3, 4, 5]
5 Toplami 15
```

20,10 toplamları hatırlandı, ama 5 için tekrar hesap yapıldı çünkü büyüklüğü 2
olan `FIFOCache` o sonuçları atmıştı.

Diğer bazı önbellek tipleri mesela `LRUCache` en az kullanılan
objeleri atar. `TTLCache` ise konulan her obje üzerinde bir zaman
aşımını kontrol eder, bunu `ttl` parametresi ile saniye üzerinden
kullanıcı ayarlayabilir, mesela `ttl=600` ile objeler konulduktan 10
dakika sonra onbellekten çıkartılırlar, "eskimiş" olurlar.

Diğer önbellek tipleri için [2].

### Kendi Kodumuz İle

Önbellek kullanımını kendi kodumuz ile de ekleyebiliriz. Mesela kare
almakla yükümlü bir fonksiyonumuz var diyelim (altta ise yaramaz bir
değişken dummy ekledik, bazı püf noktaları gösterebilmek için),

```
def kare(dummy, a):    return a*a
```

Bu fonksiyonu

```python
kare("filan", 3)
```

diye çağırıyoruz ve sonuç olarak 9 gelmesini bekliyoruz. 

Önbellekleme için, diyelim ki, eğer a değeri önceden görülmüşse, kare
işlemi sonucunun tekrar hesaplanmasını istemiyoruz, onu onbellekten
bulup hızlı bir şekilde geri döndürmek tercihimiz (tabii çarpım işlemi
de çok hızlı işler, ama bu örnek için yavaş olabileceğini hayal
edelim).

Bu kod uzerinde onbelleklemeyi eski usulle yapsaydik, kod suna
benzerdi:

```python
cache = {}
def kare(dummy, a):
    if not a in cache: cache[a] = a*a
    return cache[a]
```

Değişken cache bir Python sözlüğüdür ve onbelleğimiz onun üzerinde
duruyor. Görüldüğü gibi kod biraz kalabalıklaştı. Onbellek objesi
alanen ortada, ayrıca ıf gibi çok ciddi bir ibareyi koda sokuşturmak
zorunda kaldık. Genellikle bu ifade önemli bir işlem mantığı var ise
kullanılır - en azından kod okunabilirliği açısından böyle olması daha
iyidir. Peki bu isi daha temiz bir sekilde yapamaz miydik?

Python dekoratör fonksiyonları işte tam burada ise yarar. Bir
dekoratör bir fonsiyonu "sarmalayabilir (wrap)", ve o fonksiyona giren
çıkan tüm değerler üzerinde işlem yapabilir, ve onları istediği gibi
değiştirebilir, bu sayede o fonksiyona "çaktırmadan" ek özellikler
verebilir. Sözdizim açısından da temiz dururlar, çünkü dekoratör
fonksiyon üzerinde '@' ile tanımlanan bir şeydir, başka bir eke
ihtiyaç yoktur. O zaman (önce dekoratörün kendisi)

```python
def cache(function):  memo = {}
  def wrapper(*args):
    if args[1] in memo:
      print "cache hit"
      return memo[args[1]]
    else:
      print "cache miss"
      rv = function(*args)
      memo[args[1]] = rv
      return rv  return wrapper
```

Üstteki kod ana kodunuzdan ayrı bir yerde, bir dosyada durabilir
mesela, sadece bir kere yazılır zaten, ve kullanılması gerektiği zaman
şu ibare yeterlidir,

```
@cache
def kare(dummy, a):
   return a*a
```

Görüldüğü gibi gayet temiz. Onbellek kodu hiç etrafta gözükmüyor, bu
da kod bakımını daha rahatlaştıran bir özellik. Böylece kare
fonksiyonunu normalde olması gerektiği gibi yazıyoruz, kod onbellek
olsa da olmasa da aynı şekilde yazılıyor, sadece çarpım için gereken
işlem mantığını içeriyor.

Not: dummy değişkenini dekoratör içinde istediğimiz herhangi fonksiyon
argümanı ile iş yapabileceğimizi göstermek için kullandık, args[1] ile
sadece ikinci argümana baktık mesela.

Koda Ekler Enjekte Etmek

Diyelim ki mevcut bir kod parcasi var,

```python
import randomclass Foo:
    def f(self,x):
        x = random.random()
        return xo = Foo()
    
```

Biz bu kodun `f()` çağrısını "yakalayıp" ona ek bir şeyler yaptırtmak
istiyoruz, ve mevcut koda hiç dokunmadan bunu yapmak istiyoruz. Belki
f() çağrısı bir başka yazılım paketi içinde, vs. Bu fonsiyonu dekore
ederek bunu yapabiliriz, fakat mevcut fonksiyon koduna dokunmak
istemediğimiz için metot üstünde @birdekoratör gibi bir kullanım
yapamayız. Bu durumda başka bir seçenek sudur,

```python
def decorated_f(fn):
    def new_f(*args, **kwargs):
        res = fn(*args, **kwargs)
        setattr(args[0], "was", res)
        return res
    return new_f
Foo.f = decorated_f(Foo.f)
```

Simdi

```python
print "o", o.f(0)
print "was", o.was
```

Yeni ekleri de işletecek, bu ek Foo üzerinde yeni bir öğe yarattı, ve
bu öğeye `o.was` diye erişiyoruz.


Kaynaklar

[1] https://pypi.org/project/cachetools/

[2] https://cachetools.readthedocs.io/en/latest/

