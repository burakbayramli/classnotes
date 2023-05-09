# Python ile Kayıtlı Prosedürler (Stored Procedures) - plpython

Kayıtlı prosedürler (stored procedures) veri tabanlarına fonksiyonel
bazlı çetrefil ek mantık eklemek gerektiğinde gündeme
gelirler. Çoğunlukla bir sorgudan gelen sonucu satır satır işleyerek
ek mantık uygulamak gerektiğine kullanılırlar, SELECT vb. sorgu
komutları, her ne kadar kuvvetli özelliklere sahip olsalar da, bu
bağlamda yetersiz kalabilirler.

Kayıtlı prosedürleri kodlamak için her tabanın kendine has bir dili
var çoğunlukla, Oracle PL/ŞQL kullanır, MySql başka, vs. Hepsi de
koşul (ıf), döngü (loop) için farklı komutlar kullanırlar. Postgresql
da kayıtlı prosedür içinde Python kullanmak mümkün. Bu büyük rahatlık
çünkü farklı bir dil öğrenmeye gerek kalmıyor. Kurmak için

```
sudo apt-get install postgresql-plpython-9.1

sudo -u postgres createlang plpythonu [taban] -U postgres
```

Ornek bir prosedur

```
CREATE FUNCTION pymax (a integer, b integer)
RETURNS integer
AS $$
if a > b:
  return a
return b
$$ LANGUAGE plpythonu;
```

Test etmek icin

```
select pymax(2,4);
```

Sonucun 4 geldiğini göreceksiniz.

Bir plpython kodunun kullanması gereken yardımcı kodları var
diyelim. Fakat plpython sonunda Postgres tarafından, onun süreç
alanında, hesabında işletilen bir şey olduğu için, yardımcı kodu
plpython fonksiyonu ile "aynı dizine koymak" tekniği ise
yaramaz. Yardımcı kodu bir paket haline getirmek gerekiyordu, yani
kodun her süreç, her Ünix kullanıcısı tarafından bulunabilecek merkezi
(ya da sanal ortama) bir yere koyulması gerekiyordu. Paketleme
yazısı surada.

Python bazlı PG kayıtlı işlemleri her türlü Python paketini import
edebilirler. Yani select üzerinde çağırdığınız bir işlem arka planda
Numpy, Scipy kullanıyor bile olabilir! Burada müthiş potansiyel
var. Raporlama motorunuz (mesela Pentaho) hiçbir şeyden habersiz
"basit" SQL işletiyor olabilir, ama arka planda o basit çağrılar
sofistike analitik işlemleri yapıyor olabilirler.

Plpython fonksiyonlari icinden logging mumkun, basta

```
import logging
LOG_FILENAME = '/tmp/plpython.log'
logging.basicConfig(filename=LOG_FILENAME,level=logging.DEBUG)
tanimlanirsa,
logging.debug("mesaj")
```

ile istenilen cikti ustte tanimli dosyaya yaziliyor.

Notlar

plpython içinden kullanılan python, ya da sisteminizde kurulu aynı
Python'un kullanılış şekli biraz daha seçici olabilir. Mesela ıf
'kelime' in değişken gibi bir ifade eğer değişken Nöne ise dışarıdan
bir test ile problemsiz çalışsa da Postgresql içinden problem
çıkarttı. Bu tür farklılıklar olabilir.

Python kayitli islemler ile veri islemenin ornegi

```
CREATE or REPLACE FUNCTION test ()
RETURNS text
AS $$
import sys
class WritableObject:
   def __init__(self):
         self.content = ''
   def write(self, string):
         self.content = self.content + string + "\n"
outbuffer = WritableObject()
rv = plpy.execute("select a from TABLO");
for y in rv:
 outbuffer.write(y['a'])
return outbuffer.content
$$ LANGUAGE plpythonu;
```

```
select * from test()
```

Sondaki select işlemi bu sql dosyasını her işlettiğimizde hem kodun
değişmesi hem de test edilmesi için eklendi. Sonuç ortamı için bu
select ifadesi çıkartılmalı.

Kodun yaptığı bir sorgu işletip sonucu kayıtlı işlemden döndürmek, ve
SELECT üzerinden bu sonucun ekrana basılmasıdır.

Başka bir örnek, plpython kodu içinden SELECT'vari kolon bazlı
satırlar döndürmek. Bunun için önce bir "tip" yaratılması gerekli,
daha sonra bu tip üzerinden hafızada bir Python listesi olarak
oluşturulan sonuçlar geriye döndürülebiliyor. Bu sonuç çağrı yapan
tarafa aynen bir SELECT çıktısı gibi gözüküyor.

```
CREATE TYPE kayit_tipi AS (
 how text,
 who text
);

CREATE or replace FUNCTION veri_getir()
 RETURNS SETOF kayit_tipi
AS $$
 return ( [ "xxx", "World" ], [ "yyy", "PostgreSQL" ], [ "zzz", "PL/Python" ] )
$$ LANGUAGE plpythonu;

select * from veri_getir();
Ayni kodun yield kullanan sekli
CREATE TYPE bizim_tip AS (
 how text,
 who text
);

CREATE or replace FUNCTION veri_getir ()
 RETURNS SETOF greeting
AS $$
 for val in [ "World", "PostgreSQL", "PL/Python" ]:
   yield ( val, val )
$$ LANGUAGE plpythonu;

select * from veri_getir();
```





