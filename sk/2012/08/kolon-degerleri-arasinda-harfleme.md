# Postgresql ve Levenhstein

Kolon Degerleri Arasinda Harfleme (Spelling) Hata Farki Var mi?

Diyelim ki bir adres kolonundaki (cadde, numara, apartman) degerleri
girilirken hata yapilip yapilmadigini kontrol etmek
istiyorsunuz. Acaba tabanda birbirine benzeyen ve arasinda sadece
harfleme hatasi kadar fark olan iki adres var mi? Kontrol etmek icin
onceki yazida gordugumuz levenshtein uzakligini kullanabiliriz. Isin
guzel tarafi bu fonksiyonu direk Postgresql icinden kullanabiliriz.

Aktive etmek icin, tabana baglanin ve

# CREATE EXTENSION fuzzystrmatch;

Test etmek icin

# select levenshtein('elma','alma');

Sonuc

 levenshtein 
-------------
           1
(1 row)

Yani elma ve alma arasindaki fark 1 'degisimdir'. Levenshtein uzakligi
her turlu degisime bakar, harf eklemek, silmek gibi operasyonlar hep
uzakligi arttirir.

Sorguya donelim. Once ilgilendigimiz kolonu ve onun tablosunu
kendisiyle birlestiririz (join), yani tum adresleri tum diger
adreslerle yanyana getirmeye ugrasiyoruz, bunu yapmanin en kolay yolu
da SQL'deki join operasyonudur. Bilindigi gibi iliskisel teoride join
bir kartezyen birlesimidir, iki tablo birlestirilince her satir, her
diger satirla yanyana gelmis olur, yani tum mumkun kombinasyonlari
elde ederiz. Yeni ANSI SQL icinde bunu ozellikle belirtmek gerekiyor,
bunun icin cross join adli komut var.

select adres, sub1.adresfrom tablocross join ( select adres from
tablo ...) as sub1 where levenshtein(adres, sub1.adres) between 1 and
3...

Bu komutla arasinda 1 ila 3 fark olan adresleri bulmus olacagiz. 0
uzakligi kullanmadik, bu iki adresin ayni olmasi demek olurdu. Onlarla
ilgilenmiyoruz. 3'ten fazla uzaklik ise herhalde o adresin hakikaten
degisik bir fiziki adres oldugu anlamina gelirdi. 


