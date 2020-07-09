# Python ile API Bazli Postgresql Erisimi

Gerekli paket `pyscopg`. Python DB arayuzunu gerceklestiren (implement)
bir paket, yani MySql tabanlara baglanti icin kod yazanlar icin
cagrilar tanidik gelecektir. En basit ornek test.py adli bir dosya
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


