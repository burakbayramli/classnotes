# Zaman Kolonlari Arasindaki Farki Gun Olarak Gostermek

Postgresql tabaninda timestamp tipindeki iki tarih (kolonu) arasindaki
farki hemen eksi (-) isareti kullanarak hesaplamak mumkundur. Bu
yapildigi zaman sonuc gayet detayli bir sekilde 'su kadar yil, su
kadar gun, .. ' gibi bir tanimi iceren bir sonuctur.  Yani en basit
ornek soyle:

```
select tarih1::timestamp - tarih2::timestamp from ..
```

Eger zamanlar arasindaki farki tek bir sayiya indirgemek istiyorsak, o
zaman farki epoch (saniye) tipine, ve oradan dakika, saat, vs gibi
bolum yaparak istedigimiz sayiya cevirmemiz gerekli. Mesela gun
istiyorsak,

```
select cast(extract('epoch' from tarih1::timestamp - tarih2::timestamp) as numeric) / 60 / 60 / 24 
from .. 
```





