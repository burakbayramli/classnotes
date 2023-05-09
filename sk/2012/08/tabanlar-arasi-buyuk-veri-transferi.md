# Tabanlar Arasi Buyuk Veri Transferi, pg_dump

Bir projemiz icin birkac tabanin icindeki (ayni semaya sahip)
tablolarin icerigini nihai, bir baska taban icinde birbirine ekleyerek
yeni, birlesmis tablolar olarak yaratmamiz gerekti. Bunun icin basta
dblink kullandik fakat hafiza kullanimiyla alakali bazi hatalar ortaya
cikti.

Biz de dblink yerine pg_dump kullanmaya karar verdik.

Diyelim ki taban1,taban2,taban3 icinde her tabanda ayni semaya sahip
tablo1,tablo2 var. Script soyle:

```
TABLES="-t tablo1 -t tablo2"

pg_dump $TABLES -f /tmp/xfer.db -c taban1
psql -f /tmp/xfer.db sonuc
pg_dump $TABLES -f /tmp/xfer.db -a taban2
psql -f /tmp/xfer.db sonuc
pg_dump $TABLES -f /tmp/xfer.db -a taban3
psql -f /tmp/xfer.db sonuc
```

Dikkat edelim, ilk pg_dump -c ile veri aliyor, mevcut tablolari
silecek komutlari da urettiriyoruz boylece. Daha sonraki pg_dump
komutlari sadece veriyi aliyor, -a ile. Yani ilk basta tablolari,
semasi ile yaratip ilk veriyi ekliyoruz, sonra sadece veri
ekliyoruz. Boylece birlestirme islemini otomatik olarak
gerceklestirmis oluyoruz. Surekli ayni veri dosyasini kullandik,
xfer.db adinda, yer israfi olmamasi icin.

pg_dump kullaniminin bir avantaji daha var. Uretilen xfer.db icine
bakarsaniz, set komutlari ve analyze ibarelerini
goreceksiniz. Postgresql analyze komutu bir tabanin "ic
istatistiklerini" guncellemeye yarar, boylece sorgulayici daha guncel
veriye sahip olur. Ozellikle buyuk veri transferlerinden sonra bunun
yapilmasi tavsiye edilir. Eh pg_dump bunu kendiliginden yapiyor iste.

