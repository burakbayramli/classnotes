# Postgis ve Mesafe Hesabi

Enlem ve boylam iceren iki kordinate arasindaki mesafeyi Postgis'e
nasil hesaplattiririz? Diyelim ki enlem (latitude) ve boylam
(longitude) numerik bazli iki ayri kolonda duruyor. Bu durumda bu iki
kolonu alip onu PG'in anlayabilecegi bir kordinat formatina cevirmek
lazim, sonra st_distance ile mesafe hesabi yapmak.

```
select ..., 
st_distance(GeomFromEWKT('SRID=4326;POINT(' || lat1 || ' ' || lng1 || ')'),
   
        GeomFromEWKT('SRID=4326;POINT(' || lat2 || ' ' || lng2 || ')'))
from tablo ...
```

Sonuc kilometre olarak gelecek. Konu hakkinda bazi hizli bilgiler:
kordinatlardan en azindan kuzey yarimkure icin 40'li, 50'li olan sayi
enlem (latitude) icindir. Ingilizce olan kelimeyi hatirlamanin bir
yolu "lower your latitude to change your attitude (guneye gittikce
havanin sicaklasmasindan hareketle)". Neyse -  eger bir kordinati
map.google.com uzerinde hemen kontrol etmek isterseniz, oraya boylam,
enlem sirasinda girmek gerekiyor. 
