# pyscopg2

Postgresql Erişimi

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

Veri Taban İçeriğini Gezmek

Eğer veri tabanı, içindeki tabloları, bu tablolardaki bazı verileri
görmek istersek, GUİ araçları yerine bir jupyter not defteri üzerinden
kod işletmeyi seçebiliriz. Taban erişimi için psycopg2 iş
görür. Bağlantı aldıktan sonra (Postgresql olsun),

```
import psycopg2

conn = psycopg2.connect(
     host='[makina]',
     database='[taban]',
     user='[kullanici]',
     password='[sifre]',
     port=port
)
```

şu metotlarla taban içeriği gösterilebilir

```
import psycopg2

sql = "SELECT table_name FROM information_schema.tables WHERE table_schema = 'public'"


def get_tables(con):
    """
    Tum tablolari goster
    """    
    cursor = con.cursor()
    res = []
    cursor.execute(sql)
    for table in cursor.fetchall():
        res.append(table)
    return res


def get_table_col_names(con, table_str):
    """
    Tablo kolon isimlerini al
    """
    col_names = []
    try:
        cur = con.cursor()
        cur.execute("select * from " + table_str + " LIMIT 0")
        for desc in cur.description:
            col_names.append(desc[0])        
        cur.close()
    except psycopg2.Error as e:
        print (e)
    return col_names


def get_sample(con, table_str):
    """
    Tablodan rasgele satirlar goster
    """
    res = []
    try:        
        cur = con.cursor()
        cur.execute("select * from " + table_str + " ORDER BY RANDOM() LIMIT 10")
        for x in cur: res.append(x)
        cur.close()
    except psycopg2.Error as e:
        print (e)
    return res


def get_count(con, table_str):
    """
    Tabloda kac satir var
    """
    res = []
    try:        
        cur = con.cursor()
        cur.execute("select count(*) from " + table_str)
        res = cur.fetchone()
        cur.close()
    except psycopg2.Error as e:
        print (e)
    return res
```

Jupyter not defterinde bildiğimiz gibi etkileşimli (interactive) bir
şekilde kod yazılabiliyor, bir hücrede yazılan kodu hemen işletip
doküman içinde görüyoruz.. Eh böyle olunca üstteki çağrıları
istedigimiz tablo üzerinde yapınca sanki GUİ araçından taban içeriğini
gezmis oluyoruz.

Jupyter kurmak istemeyenler, direk markdown md dosyaları içinden
üstteki işlemleri yapmak isteyenler (ve Emacs kullanıcıları) bizim
eklentiyi kullanabilir.



