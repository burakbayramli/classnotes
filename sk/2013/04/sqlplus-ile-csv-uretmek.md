# Sqlplus ile CSV Uretmek

Komut satirindan sqlplus ile CSV uretmek icin

```
set echo offset verify offset termout onset heading offset pages 50000set feedback offset newpage noneset linesize 250set colsep '|'spool out.csv;select 
kolon1 || '|' ||
kolon2 
from tablo where filtre = 'filan';spool off;exit;
```

Bu sql dosyasini mesela test.sql olarak kaydedersek,

sqlplus64 user/pass@db @test.sql

ile isletebiliriz, ve ciktiyi out.csv icinde goruruz. Kolon degerleri
| ile ayrilmis olacaktir. Okunan tablo ismi goruldugu gibi tablo,
zaten ciktiyi uretecek SQL ozellikle belirtiliyor.

SQLPlus'un CSV uretmek icin pek iyi bir ortam oldugu soylenemez. Zaten
bu aracin "sekli olarak veri uretmek" oldugu soyleniyor, tabii
70-80'li yillarin text bazli ortami icin kolonsal sekiller uretmek
anlaminda.. Bu sekli egilim sadece veriyi cekip cikartmak isteyenler 
icin problem olabiliyor. Mesela linesize parametresine dikkat, bu
parametre bir satirda kac karakter olacagini tanimlar, eger veri
satirinda o kadar karakter yoksa, o zaman linesize bosluk karakteri
ile doldurulur (daha fazla veri gelmisse de fazla olan karakterler
kesip atilir! dikkat). Bu bosluk karakteri satirdaki en son kolonu
okurken problem cikartabilir, Pandas ile mesela skipinitialspace=True
ile bu bosluktan kurtulmak gerekiyor.

Kolonlari yanyana ve | ile ayrilacak sekilde biz yanyana getiriyoruz,
cunku baska turlu tum bilgilerin ardi ardina sirilanmasi mumkun
olmuyor (colsep ile set edilen ayarin pek bir anlami yok yani).

Ayrica SQLPlus kolon isimlerini otomatik olarak vermiyor. Biz bu
isimleri Pandas ile okurken kendimiz veriyoruz.


