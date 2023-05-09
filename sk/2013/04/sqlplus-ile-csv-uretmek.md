# Sqlplus ile CSV Uretmek

Komut satirindan sqlplus ile CSV uretmek icin

```
set echo offset verify offset
termout onset
heading offset
pages 50000
set feedback offset
newpage noneset
linesize 250
set colsep '|'spool out.csv;
select 
kolon1 || '|' ||
kolon2 
from tablo where filtre = 'filan';spool off;exit;
```

Bu sql dosyasini mesela test.sql olarak kaydedersek,

```
sqlplus64 user/pass@db @test.sql
```

ile isletebiliriz, ve ciktiyi out.csv icinde goruruz. Kolon degerleri
| ile ayrilmis olacaktir. Okunan tablo ismi goruldugu gibi tablo,
zaten ciktiyi uretecek SQL ozellikle belirtiliyor.

SQLPlus'un CSV üretmek için pek iyi bir ortam olduğu söylenemez. Zaten
bu araçın "şekli olarak veri üretmek" olduğu söyleniyor, tabii
70-80'li yılların text bazlı ortamı için kolonsal şekiller üretmek
anlamında.. Bu şekli eğilim sadece veriyi çekip çıkartmak isteyenler 
için problem olabiliyor. Mesela linesize parametresine dikkat, bu
parametre bir satırda kaç karakter olacağını tanımlar, eğer veri
satırında o kadar karakter yoksa, o zaman linesize boşluk karakteri
ile doldurulur (daha fazla veri gelmişse de fazla olan karakterler
kesip atılır! dikkat). Bu boşluk karakteri satırdaki en son kolonu
okurken problem çıkartabilir, Pandas ile mesela skipinitialspace=True
ile bu boşluktan kurtulmak gerekiyor.

Kolonlari yanyana ve | ile ayrilacak sekilde biz yanyana getiriyoruz,
cunku baska turlu tum bilgilerin ardi ardina sirilanmasi mumkun
olmuyor (colsep ile set edilen ayarin pek bir anlami yok yani).

Ayrica SQLPlus kolon isimlerini otomatik olarak vermiyor. Biz bu
isimleri Pandas ile okurken kendimiz veriyoruz.


