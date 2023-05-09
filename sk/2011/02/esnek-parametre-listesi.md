# Esnek Parametre Listesi

Bir fonksiyona geçilecek parametre listesi önceden belli değilse,
yıldız ısareti, mesela *liste gibi bir kullanımla parametre listesi
esnek hale getirilebilir. Mesela geçilen parametreleri sadece ekrana
basan bir fonksiyon yazsak:

```
def f(*list):
   for x in list: print x
```

Fonksiyon `f()`'de geçilen parametreler esnek olarak tanımlanmış. Bu
fonksiyonu çağırmak için `f(3,4,5,6)` ya da `f(3,4)` kullanılabilir.


