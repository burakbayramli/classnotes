# PostgreSql

Bir süredir PostgreSql tabanı ile çalışmıyorduk, iş için gerekli oldu,
kuvvetli bir taban. Ubuntu 11'de apt-get ile gelen en son versiyon 9.1
Bu taban MySql gibi açık kaynak, ve anlatıldığına göre amaçları
tamamen Oracle'ın yerine geçebilmek. O yüzden komutları Oracle
komutlarına birebir uyumlu yaratıyorlar. Kurmak için

`sudo apt-get install postgresql`

Ama aslında versiyon numaralı kurmak daha iyi, bende işleyen versiyon

`sudo apt-get install postgresql-9.5`

Faydalı bazı komutlar şurada bulunabilir. PG postgres adlı bir Unix
kullanıcısı için kurulacak o yüzden shell bazlı tüm PG komutları
başında `sudo -u postgres` gerekli. Paket kurulunca bilgisayarınız
başlayınca otomatik olarak başlamak üzere ayarlı olacak, açıp,
kapatmak için

`sudo service postgresql [komut]`

ki `[komut]` `stop`, `start`, `restart` olabilir.

Komut satırından `psql` kullanmak için

```
sudo -u postgres createuser --superuser $USER

sudo -u postgres createdb $USER
```

Bu işlemden sonra üzerinde olduğunüz Ünix kullanıcısı ile `psql` ile
tabanın komut satırına girebiliriz. Burada ek kullanıcılar yaratmak mümkün, mesela

```
CREATE USER user1 with PASSWORD 'user1pass';
```

Şimdi `psql` e üstteki kullanıcı ile giriş yapabiliriz, fakat `psql -U
user1` ile mesela problem çıkarsa,

```
psql user1 -h 127.0.0.1 -d [taban ismi]
```

ile giriş yapabiliriz, üstteki stil ile Postgres'i network üzerinden
bağlanmaya zorlamış olduk, diğer şekilde `user1` in Ünix üzerinde de
olması şartı aranabiliyor.


PG komut satırı içinde taban yaratmak,

```
create database TABAN111;
```

Tabana bağlanmak için `\c [taban ismi]`.

Bir tabandaki tüm tabloları görmek için

```
\dt
```

CSV

Postgres'den veri çekip veri yüklemenin en hızlı yollarından biri CSV
temelli. Veri almak için

```
psql [taban ismi]  -h [makina] -p [port] -U [kullanici] -c "COPY (SELECT * from [tablo] where [sart]) TO stdout " > [csv dosya ismi]
```

Ya da sql dosyasi icinde

```
copy( .. SELECT .. ) to '/tmp/out' with csv header
```

gibi bir kullanim olabilir. Dikkat: bu durumda komut postgres
kullanicisi tarafindan isletilecektir, bu kullanicinin sizin `$HOME`
dizininize erisimi olmayabilir, o yuzden ustte /tmp/
kullanildi. Uzaktaki bir makina uzerinde SQL komut satirinda

```
copy( .. SELECT .. ) TO STDOUT with CSV HEADER > /tmp/out
```

ta kullanilabilir.  Ama en iyisi herhalde

```
psql [taban]  -h [makina] -p [port] -U [kullanici] -c "COPY (`cat komut.sql`) TO stdout with delimiter ',' CSV HEADER "  > /tmp/out
```

Bu komut uzaktaki bir taban üzerinde bir SQL dosyası işletilir ve
çıktıyı CSV olarak kaydeder. Parametrize edilmiş halde biz run_csv.sh
adlı bir dosya içinde

```
psql taban  -h localhost -p 5432 -U postgres  -c "COPY (`cat $1`) TO stdout with delimiter ',' CSV HEADER "
```

kullanırız, ve `run_csv.sh dosya.sql > out`

şeklinde bunu kullanabiliriz.

Dosya yüklemek için

```
psql [taban] -c "COPY [tablo] FROM '[dosya ismi]'";
```

Uzak tabanlara bağlanmak: Eğer `-h` ile uzaktaki (remote) tabanlarda sIk
olarak psql komutları veriyorsanız, ve habire şifre sorulmasından
kurtulmak istiyorsanız, `.pgpass` tekniğini kullanabilirsiniz. Bu `$HOME`
dizininizde olması gereken bir dosyadır, ve

```
makina:port:db:kullanici:sifre
```

formatındadır. Dosyanın `chmod 0600` yapılması gerekiyor. Not: eğer
`psql` komutunu `sudo -u postgres` ile işletiyorsanız (ki uzaktaki bir
makina için gerekli değil) o zaman .pgpass dosyasının sizin
makinanızdaki postgres kullanıcısının ana dizininde olması gerekiyor,
bizim makinamızda bu `/var/lib/postgres` diye bir yerdeydi (şu -
`postgres` ile o kullanıcıya girerek kontrol edebilirsiniz). Tabii ki
dosya `chown` ile `postgres`'e ait yapılmalı.

Ben PG ile çalışırken çoğunlukla `run.sh` adlı bir script'i aynı dizinde
tutarım. Bunun içinde mesela

```
psql dellstore2 -h localhost -p 5432 -U postgres < $1
```

gibi bir satır olabilir, taban önceden tanımlanmıştır, çünkü
genellikle geliştirme sırasında tek bir taban ile çalışırım, ve
geliştirirken,

```
sh run.sh test.sql
```

gibi komutlar işletirim. Böylece üstteki sql dosyası direk belirtilen
taban üzerinde işletilir. .pgpass içinde de

```
localhost:5432:dellstore2:postgres:[sifre]
```

ibaresi vardir.

Eger `hstore` adli bir veri tipini kullanmak istiyorsaniz,

```
sudo -u postgres psql [taban]
```

ile girdikten sonra

```
CREATE EXTENSION hstore;
```

komutunu kullanabilirsiniz.

Eğer yeni Ünix kullanıcılarına Postgres üzerinde admin hakları vermek
istiyorsanız (ki bu eğer admin'şeniz kışisel hesabınız için iyi olur,
yoksa pg_dump komutu bile kullanılamıyor), sudo -u postgres psql ile
girdikten sonra

```
CREATE ROLE unix_kullanici_isminiz PASSWORD 'bir sifre' SUPERUSER CREATEDB CREATEROLE INHERIT LOGIN;
```

ile kullanıcınızı Postgres üzerinde süper kullanıcı yapabilirsiniz.

Silmek

Postgresql'i `apt-get remove` ile silmek ise yaramayabilir (hala PG
süreçlerini işler gördük, silme işleminden sonra bile). Daha kuvvetli
bir silme işlemi için

```
sudo apt-get --purge remove postgresql\*
```

Dış dünyaya PG bağlantısını 5432 portunu açmak istiyorsak,
`/etc/postgresql/9.1/main/postgresql.conf` içinde

```
listen_addresses = '*'
```

tanımlanmış olmalı.

GUI

Sema, ana / yabancı anahtar, tablolar hakkında detaylı bilgiler, SQL
işletmek için faydalı bir GUİ,

[Squirrel](squirrel-sql.sourceforge.net).

Diğer Postgres Yazıları

[Tabanlar Arasi Buyuk Veri Transferi, pg_dump](../../2012/08/tabanlar-arasi-buyuk-veri-transferi.html)

[Postgresql ve Levenhstein](../../2012/08/kolon-degerleri-arasinda-harfleme.html)

[Postgres First_Value, Sum, Partition](../../2012/05/postgres-firstvalue-sum-partition.html)

[Postgresql ile Yuzdelik (Percentile) Hesabi](../../2012/09/postgresql-ile-yuzdelik-percentile.html)

[Postgresql DB Baglantisi (dblink)](../../2012/04/postgresql-db-baglantisi-dblink.html)

[Python ile Kayıtlı Prosedürler (Stored Procedures) - plpython](../../2012/04/plpython.html)

[Enlemler, Boylamlar ve PostgreSql PostGis](../../2012/06/enlemler-boylamlar-ve-postgresql.html)


