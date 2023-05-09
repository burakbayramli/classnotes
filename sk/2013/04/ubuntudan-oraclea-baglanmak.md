# Ubuntu'dan Oracle'a Baglanmak

Şu sayfadaki tarif takip edilebilir

https://help.ubuntu.com/community/Oracle%20Instant%20Client

Şu adrese girilir ve bağlanan sisteme tekabül eden bağlantı tıklanır

http://www.oracle.com/technetwork/database/features/instant-client/index-097480.html

Bizim için bu Linux x86-64 idi mesela. Buradaki listelenen şeyler
içinde Basic, SQL*Plus, JDBC Supplement için gösterilen rpm
indirilir. "Lisans Kabulü" için en üst soldaki radyo düğmesinin
seçilmiş olması gerekiyor.

Bu noktada Oracle sitesi "üyelik" şartını arar, email, sıfre vs verip
üyelik yapılır, konfirmasyon email'ine cevap verildikten sonra üstteki
rpm dosyaları indirilecektir.

Sonra 

```
apt-get install alien
```

kurulur ve

```
sudo alien -i oracle-instantclient11.2-*
```

ile indirilen dosyalar kurulur. Eğer paylaşılan kütüphane (shared
library, so) hataları var ise, .başhrç içine

```
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/lib/oracle/11.2/client64/lib
```

yazınız. Bir kere `sudo ldconfig` yapmanız da gerekebilir. Ayrıca

```
sudo apt-get install libaio1
```

Bundan sonra sqlplus64 isleyecektir. Simdi makina, db isimlerini nasil
tanimlayacagimiza gelelim.

```
sudo mkdir /etc/oracle
```

ve bu dizinde

```
sudo gedit /etc/oracle/tnsnames.ora
```

Bu dosyada db, makina isimleri, ip adresleri yeralir. En son .bashrc icinde

```
export TNS_ADMIN=/etc/oracle
```

Artık `sqlplus64 user@db` gibi bir ifade ile bağlantı yapılabilir.

Eger Python icinden baglanmak istersek,

http://tshepang.net/accessing-oracle-db-using-python-in-debian

cxOracle lazim, mesela 64 bit Python 2.7 Oracle 11 icin CentOS 5
x86_64 RPM (Oracle 11g, Python 2.7) baglantisina tiklanir. Indikten
sonra

```
sudo alien --install cx_Oracle-5.1.2-11g-py27-1.x86_64.rpm
sudo easy_install cx_oracle
sudo ln -s /usr/lib/python2.7/site-packages/cx_Oracle.so /usr/lib/python2.7/lib-dynloadsudo ln -s /usr/lib/oracle/11.2/client/lib/libnnz11.so /usr/libsudo ln -s /usr/lib/oracle/11.2/client/lib/libclntsh.so.11.1 /usr/lib
```

Kod ornegi

```
import pandas as pd]
import pandas.io.sql as sql
import cx_Oracle as odb
conn = odb.connect("[kullanici]/[sifre]@[taban]")
df = sql.read_frame("select .. from", conn)
```

ile Oracle verisi bir Dataframe içine alınmış olur.






