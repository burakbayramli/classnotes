# SQL

SQL ilişkisel tabanları sorgulamak için kullanılır. İlişkisel model,
ve giriş bilgileri [1]'de bulunabilir.

Chinook

Bu tabanın sqlite ortamında nasıl kurulacağını [2]'de
anlattık. Chinook iTunes gibi bir dijital medya satış şirketi için
hazırlanmış bir taban, verilerin bazıları gerçek iTunes'dan alınmış,
müşterilerle alakalı kısımları yapay.

![](chinook_er.jpeg)

Varlık-İlişki (Entity-Relationship -ER-) diyagramına bakınca ana
ilişkileri görebiliyoruz. Bazı yardımcı fonksiyonlar,


```python
import sqlite3, pandas as pd

pd.set_option('display.width', 1000)
pd.set_option('display.max_columns', 10)

DB = '/tmp/chinook.db'

def runsql(sql):
    conn = sqlite3.connect(DB)
    c = conn.cursor()
    rows = c.execute(sql)
    for row in rows: print (row)
    conn.close()
    
def psql(sql):
    conn = sqlite3.connect(DB)
    c = conn.cursor()
    rows = c.execute(sql)
    df = pd.DataFrame(rows.fetchall())
    return df

```


Hangi ülkenin müşteri en çok ödeme yaptı? (Chinook-SQL-Exercise/top_country.sql)


```python
sql = """
SELECT "Country", MAX("Total Sales For Country") as "Total Spent"
FROM 
  (SELECT BillingCountry as "Country" , SUM(Total) as "Total Sales For Country"
FROM Invoice 
GROUP BY BillingCountry);
"""
runsql(sql)
```

```text
('USA', 523.0600000000003)
```


```python
runsql("SELECT LastName, Title FROM Employee limit 5")
```

```text
('Adams', 'General Manager')
('Edwards', 'Sales Manager')
('Peacock', 'Sales Support Agent')
('Park', 'Sales Support Agent')
('Johnson', 'Sales Support Agent')
```

Pandas ile

```python
psql(sql)
```

```text
Out[1]: 
         0                    1
0    Adams      General Manager
1  Edwards        Sales Manager
2  Peacock  Sales Support Agent
3     Park  Sales Support Agent
4  Johnson  Sales Support Agent
```

Şarkılar, Türler 

Basit bir birleştirim (join) ile başlayalım. Tüm şarkılar `Track`
tablosunda, o şarkının hangi türe ait olduğu `Genre`
tablosunda. Aradaki bağlantı `Track` üzerinde duran bir yabancı
anahtar, `GenreId`. O zaman her şarkının ait olduğu tür için `GenreId`
üzerinden bir birleştirme gerekiyor,


```python
psql("""
SELECT t.Name AS track_name,
       g.name AS genre_name
  FROM Track t
  JOIN Genre g
    ON t.GenreId = g.GenreId
 LIMIT 5""")

```

```text
Out[1]: 
                                         0     1
0  For Those About To Rock (We Salute You)  Rock
1                        Balls to the Wall  Rock
2                          Fast As a Shark  Rock
3                        Restless and Wild  Rock
4                     Princess of the Dawn  Rock
```

Sonuçları `LIMIT 5` ile sınırladık, yoksa tüm kayıtlar geri gelirdi.

Biraz önce bir iç birleşim (inner join) yapmış olduk. Bu tür
birleşimde eğer üzerinden birleşim yapılan kimlik iki tarafta da
yoksa, sonuca alınmaz.

Fakat bu derece harfiyen bir uyum olmasını her zaman
istemeyebilirdik. Diyelim ki şarkıları o şarkının ait olabileceği
(dikkat, olabileceği) bir fatura detay `InvoiceLine` satırıyla eşlemek
istiyoruz. Eğer bir şarkı hiçbir zaman satılmadıysa fatura detayında
olmayabilir. Ama biz tüm şarkıları yine de görmek istiyoruz, ve
faturalamanın bizi sınırlamasını istemiyoruz. Bu durumda bir sol
birleşim `LEFT JOİN` yaparız, bu durumda soldaki tablo asal tablo
olur, onun tüm satırları her zaman geri döndürülür, ama sağda uyum
yoksa fatura detay için boş değer gelir.

```python
psql("""
SELECT t.name, t.composer, i.InvoiceLineId
  FROM Track t
  LEFT JOIN InvoiceLine i
  ON t.TrackId = i.TrackId
LIMIT 8""")
```

```text
Out[1]: 
                                         0                                                  1       2
0  For Those About To Rock (We Salute You)          Angus Young, Malcolm Young, Brian Johnson   579.0
1                        Balls to the Wall                                               None     1.0
2                        Balls to the Wall                                               None  1154.0
3                          Fast As a Shark  F. Baltes, S. Kaufman, U. Dirkscneider & W. Ho...  1728.0
4                        Restless and Wild  F. Baltes, R.A. Smith-Diesel, S. Kaufman, U. D...     2.0
5                     Princess of the Dawn                         Deaffy & R.A. Smith-Diesel   580.0
6                    Put The Finger On You          Angus Young, Malcolm Young, Brian Johnson     3.0
7                          Let's Get It Up          Angus Young, Malcolm Young, Brian Johnson     NaN
```











Referans

[1] Veri Tabanları, Kurumsal Java 2005 Kitabından, [PDF](https://github.com/burakbayramli/classnotes/raw/master/sk/2012/03/db-kj.pdf)

[2] [sqlite](../../2018/03/sqlite-basit-sekilde-hzl-diske-deger-yazma.md)

[3] [psycopg2](../../2012/06/psycopg2-python-ile-api-bazli-postgresql-erisimi.md)

[4] https://github.com/Olamiotan/PythonStarter

[5] https://database.guide/2-sample-databases-sqlite/

[6] https://data-xtractor.com/knowledgebase/chinook-database-sample/

[7] https://github.com/KAPrueved/Chinook-SQL-Exercise

[8] Mando Iwanaga

* https://medium.com/@mandoiwanaga08/introduction-to-sql-beginner-level-7acb59286e7b
* https://medium.com/@mandoiwanaga08/sql-continued-3ff70f613d96
* https://medium.com/@mandoiwanaga08/sql-part-3-a1a3730b7624
* https://medium.com/@mandoiwanaga08/sql-part-4-1d61ada63b5
* https://github.com/mandoiwanaga/sql_practice
* https://github.com/mamineofficial/Query-a-Digital-Music-Store-Part-I-SQL

[9] https://shichaoji.com/2016/10/10/database-python-connection-basic/

