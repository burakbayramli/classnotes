# pyscopg2, Python ile APİ Bazlı Postgresql Erişimi

Gerekli paket `pyscopg`. Python DB arayüzünü gerçekleştiren (implement)
bir paket, yani MySql tabanlara bağlantı için kod yazanlar için
çağrılar tanıdık gelecektir. En basit örnek `test.py` adlı bir dosya
olsun,

```
import psycopg2

conn = psycopg2.connect("dbname=[taban] user=postgres password=postgres")
cur = conn.cursor()
cur.execute("SELECT * from tablo limit 1")
print cur.fetchone()
```

Isletmek icin

```
sudo -u postgres python test.py
```

