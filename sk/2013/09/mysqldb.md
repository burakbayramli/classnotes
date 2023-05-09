# MySQLdb, Pandas

Ubuntu uzerinde kurmak icin 

```
sudo pip install pip --upgrade

sudo easy_install -U distribute

sudo apt-get build-dep python-mysqldb

sudo pip install MySQL-python
```

Ornek kod 

```
import MySQLdb, os, sys

conn= MySQLdb.connect(host='makina',
                      port=3306,user='user', passwd='sifre',
                      db='test')
```

Bir Pandas dataframe'i alarak veri tabanina yazmak muthis kolay:

```
pd.io.sql.write_frame(df, "tablo1", conn, flavor='mysql')
```

Ustteki ifade df icindeki dataframe'i alip veri tiplerini ayarlayip
Mysql'de tablo yaratacak ve veriyi tabloya ekleyecektir. Eger tablo
mevcut ise ikinci isletim hata verebilir, tablo yokedilip tekrar
bastan yaratilsin istersek,

```
pd.io.sql.write_frame(df, "tablo1", conn, flavor='mysql', if_exists='replace')
```

Eger dataframe verisi mevcut tabloya "eklensin" istiyorsak,

```
pd.io.sql.write_frame(df, "tablo1", conn, flavor='mysql', if_exists='append')
```

Basit bir SQL (DDL usulu, mesela bir DELETE komutu)

```
sql = "delete from tablo where kolon1 = %d and kolon2 = %d" % (arg1,arg2)
conn.cursor().execute(sql)
conn.commit()
```








