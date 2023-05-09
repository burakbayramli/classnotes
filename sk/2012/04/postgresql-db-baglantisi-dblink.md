# Postgresql DB Baglantisi (dblink)

Oracle'daki database bağlantısı (db link) özelliğinin Postgresql'da
karşılığı nedir? Bilindiği gibi dblink sayesinde yerel tabanımız
içinde uzaktaki bir tabanının tablolarını sanki yerelmiş gibi
kullanabiliyoruz. Postgres ile uzak tabanların birleşim (join) için
kullanılabildiği şüpheli, fakat en azından düz SELECT kullanımı db
link ile mümkün. Veri ambarı ortamında uzaktaki tabloların kopyasına
ihtiyacımız var sadece / zaten, sofistike tüm sorgular yerel (olmalı).

```
sudo apt-get install postgresql-contrib-9.1
```

Sonra psql icinden

Simdi

```
CREATE EXTENSION dblink;
```

komutu sadece bir seferlik isletilir. Bundan sonra uzaktaki makine uzerineki sorgular su sekilde isletilir ([tablo]'dan sadece name adli bir kolonu aliyoruz)

```
SELECT name FROM dblink('host=[host] user=[sifre] password=[sifre] dbname=[taban]', 
'SELECT name FROM [tablo]') 
as 
t1(name text)
```

Bu ozelligin begenmedigimiz tarafi kullanici, sifre gibi detaylarin
sorgu icinde olmasi. Bu detaylar baska bir ayar dosyasinda olsaydi
daha iyi olurdu. Ayrica "as .." sonrasi sema bilgisini SELECT sonrasi
bizim vermemiz gerekiyor. Bu bilgi select ile kendiliginden alinsa
daha iyi olurdu. Veri ambari kodlamasi genellikle db link uzerinden
CREATE TABLE AS (SELECT ..) gibi bir kullanimla uzaktaki tablolari
yerel ortama tasir. Ustteki yontemde tanim elle eklendigi icin bu pek
kullanisli olmaz.

Faydali bir script / metot altta. Baglanti kurmak istenilen (hedef)
taban, ve tablo icin

```
select column_name,udt_name from information_schema.columns where
table_name = '[TABLO]' order by ordinal_position
```

isletilir, cikti bir text dosyasina yazilir. Sonra

```
import os, csv, sys
schema = csv.reader(open("/tmp/link.out", 'rb'), delimiter=',')
schema.next() # skip header
print "drop table if exists %s ;" % [TABLO]
print "create table %s as (" % [TABLO]
print "SELECT * FROM "
print "dblink('host=localhost user=postgres password=postgres dbname=[KAYNAK TABAN]', " % countryprint "'SELECT * FROM %s') " % tableprint "as t1"print "("i = 0list = list(schema)l = len(list)-1
for line in list:
    if i < l:
        print "%s %s, " % (line[0], line[1])
    else:
        print "%s %s " % (line[0], line[1])
    i+=1
    print "));\n"
```

script'i hedef tablo icin gereken create table as (select ...)
ifadesini o tablonun semasina bakarak otomatik olarak uretecektir. Bu
ifadeyi alip direk DDL olarak kaynak taban uzerinde isletebilirsiniz.

Fakat Postgres uzerinde dblink buyuk veri setleri icin guvenilir
olmayabiliyor. Biz her nedense hafizanin tukendigini soyleyen "out of
memory" hatalari almaya basladiktan sonra dblink kullanmaktan
vazgectik.





