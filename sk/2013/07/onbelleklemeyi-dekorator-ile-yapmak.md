# Dekoratörler, Onbellek Kodlaması, Fonksiyon Değiştirmek

Bir fonksiyona ya da kod parçasına onbellek kullanımı eklemek kolaydır
fakat bu tür ekler kodun okunabilirliğini de azaltabilirler. Mesela
kare almakla yükümlü bir fonksiyonumuz var diyelim (altta ise yaramaz
bir değişken dummy ekledik, bazı püf noktaları gösterebilmek için),

```
def kare(dummy, a):    return a*a
```

Bu fonksiyonu

```
kare("filan", 3)
```

diye çağırıyoruz ve sonuç olarak 9 gelmesini bekliyoruz. 

Onbellekleme için, diyelim ki, eğer a değeri önceden görülmüşse, kare
işlemi sonucunun tekrar hesaplanmasını istemiyoruz, onu onbellekten
bulup hızlı bir şekilde geri döndürmek tercihimiz (tabii çarpım işlemi
de çok hızlı işler, ama bu örnek için yavaş olabileceğini hayal
edelim).

Bu kod uzerinde onbelleklemeyi eski usulle yapsaydik, kod suna
benzerdi:

```
cache = {}def kare(dummy, a):
    if not a in cache: cache[a] = a*a
    return cache[a]
```

Değişken cache bir Python dictionary'dir ve onbelleğimiz onun
üzerinde  duruyor. Görüldüğü gibi kod biraz kalabalıklaştı. Onbellek
objesi alanen ortada, ayrıca ıf gibi çok ciddi bir ibareyi koda
sokuşturmak zorunda kaldık :) Genellikle bu ifade önemli bir işlem
mantığı var ise kullanılır - en azından kod okunabilirliği açısından
böyle olması daha iyidir.

Peki bu isi daha temiz bir sekilde yapamaz miydik?

Python dekoratör fonksiyonları işte tam burada ise yarar. Bir
dekoratör bir fonsiyonu "sarmalayabilir (wrap)", ve o fonksiyona giren
çıkan tüm değerler üzerinde işlem yapabilir, ve onları istediği gibi
değiştirebilir, bu sayede o fonksiyona "çaktırmadan" ek özellikler
verebilir. Sözdizim açısından da temiz dururlar, çünkü dekoratör
fonksiyon üzerinde '@' ile tanımlanan bir şeydir, başka bir eke
ihtiyaç yoktur. O zaman (önce dekoratörün kendisi)

```
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
@cachedef kare(dummy, a):    return a*a
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

```
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

```
def decorated_f(fn):
    def new_f(*args, **kwargs):
        res = fn(*args, **kwargs)
        setattr(args[0], "was", res)
        return res
    return new_f
Foo.f = decorated_f(Foo.f)
```

Simdi

```
print "o", o.f(0)
print "was", o.was
```

Yeni ekleri de işletecek, bu ek Foo üzerinde yeni bir öğe yarattı, ve
bu öğeye `o.was` diye erişiyoruz.





