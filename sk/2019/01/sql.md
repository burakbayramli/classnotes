# SQL

İlişkisel veri tabanlarını sorgulamak için SQL dilini
kullanırız. Keşfedildiği 70'li yıllardan beri gündemde kalabilmesinin
sebebi temelinde bir teori olması. İlişkisel çalcülüs denen kavram
sorguları veriden 'ne istediğine' odaklıyor, verinin nasıl alınacağı
ile ilgilenilmiyor.

Bu yazıda örnek olarak SQLite tabanını kullanacağız. Bu taban gayet
basit tek bir dosyada tüm bilgiyi tutar, Postgres gibi daha çetrefil
tabanlarda olduğu gibi ayrı servis yapıları yoktur. İlk ihtiyaçlarımız
için bu yeterli olur.

İlk önce tabloyu yaratalım. Araba (çar) adlı tabloda her satır araba
kayıtları olacak. Şimdilik bu tabloda bir kimlik ve tanım için bir
isim olacak.

```python
import sqlite3
conn = sqlite3.connect('/data/data/com.termux/files/home/Downloads/test.db')
c = conn.cursor()
```

```python
c.execute('''DROP TABLE IF EXISTS CAR; ''')
c.execute('''CREATE TABLE  CAR (id INT, name TEXT); ''')
```









