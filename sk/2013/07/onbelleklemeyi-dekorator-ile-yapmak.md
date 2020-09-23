# Dekoratörler, Onbellek Kodlaması, Fonksiyon Değiştirmek

Bir fonksiyona ya da kod parcasina onbellek kullanimi eklemek kolaydir
fakat bu tur ekler kodun okunabilirligini de azaltabilirler. Mesela
kare almakla yukumlu bir fonksiyonumuz var diyelim (altta ise yaramaz
bir degisken dummy ekledik, bazi puf noktalari gosterebilmek icin),

```
def kare(dummy, a):    return a*a
```

Bu fonksiyonu

```
kare("filan", 3)
```

diye cagiriyoruz ve sonuc olarak 9 gelmesini bekliyoruz. 

Onbellekleme icin, diyelim ki, eger a degeri onceden gorulmusse, kare
islemi sonucunun tekrar hesaplanmasini istemiyoruz, onu onbellekten
bulup hizli bir sekilde geri dondurmek tercihimiz (tabii carpim islemi
de cok hizli isler, ama bu ornek icin yavas olabilecegini hayal
edelim).

Bu kod uzerinde onbelleklemeyi eski usulle yapsaydik, kod suna
benzerdi:

```
cache = {}def kare(dummy, a):
    if not a in cache: cache[a] = a*a
    return cache[a]
```

Degisken cache bir Python dictionary'dir ve onbellegimiz onun
uzerinde  duruyor. Goruldugu gibi kod biraz kalabaliklasti. Onbellek
objesi alanen ortada, ayrica if gibi cok ciddi bir ibareyi koda
sokusturmak zorunda kaldik :) Genellikle bu ifade onemli bir islem
mantigi var ise kullanilir - en azindan kod okunabilirligi acisindan
boyle olmasi daha iyidir.

Peki bu isi daha temiz bir sekilde yapamaz miydik?

Python dekorator fonksiyonlari iste tam burada ise yarar. Bir
dekorator bir fonsiyonu "sarmalayabilir (wrap)", ve o fonksiyona giren
cikan tum degerler uzerinde islem yapabilir, ve onlari istedigi gibi
degistirebilir, bu sayede o fonksiyona "caktirmadan" ek ozellikler
verebilir. Sozdizim acisindan da temiz dururlar, cunku dekorator
fonksiyon uzerinde '@' ile tanimlanan bir seydir, baska bir eke
ihtiyac yoktur. O zaman (once dekoratorun kendisi)

```
def cache(function):  memo = {}
  def wrapper(*args):
    if args[1] in memo:
      print "cache hit"
      return memo[args[1]]
    else:
      print "cache miss"
      rv = function(*args)
      memo[args[1]] = rv
      return rv  return wrapper
```

Ustteki kod ana kodunuzdan ayri bir yerde, bir dosyada durabilir
mesela, sadece bir kere yazilir zaten, ve kullanilmasi gerektigi zaman
su ibare yeterlidir,

```
@cachedef kare(dummy, a):    return a*a
```

Goruldugu gibi gayet temiz. Onbellek kodu hic etrafta gozukmuyor, bu
da kod bakimini daha rahatlastiran bir ozellik. Boylece kare
fonksiyonunu normalde olmasi gerektigi gibi yaziyoruz, kod onbellek
olsa da olmasa da ayni sekilde yaziliyor, sadece carpim icin gereken
islem mantigini iceriyor.

Not: dummy degiskenini dekorator icinde istedigimiz herhangi fonksiyon
argumani ile is yapabilecegimizi gostermek icin kullandik, args[1] ile
sadece ikinci argumana baktik mesela.

Koda Ekler Enjekte Etmek

Diyelim ki mevcut bir kod parcasi var,

```
import randomclass Foo:
    def f(self,x):
        x = random.random()
        return xo = Foo()
    
```

Biz bu kodun f() cagrisini "yakalayip" ona ek bir seyler yaptirtmak
istiyoruz, ve mevcut koda hic dokunmadan bunu yapmak istiyoruz. Belki
f() cagrisi bir baska yazilim paketi icinde, vs. Bu fonsiyonu dekore
ederek bunu yapabiliriz, fakat mevcut fonksiyon koduna dokunmak
istemedigimiz icin metot ustunde @birdekorator gibi bir kullanim
yapamayiz. Bu durumda baska bir secenek sudur,

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

Yeni ekleri de isletecek, bu ek Foo uzerinde yeni bir oge yaratti, ve
bu ogeye o.was diye erisiyoruz.






