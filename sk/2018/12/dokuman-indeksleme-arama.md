# Doküman İndeksleme, Arama

Veri tabanlı indeksleme, arama mimarileri rekabetçi olmaya devam
ediyor. Hard diskimde tuttuğum pdf, epub kitapları indekslemek için
`recoll` yerine bir aracı kendim yazmak istediğimde ilk öne çıkan
`whoosh` adlı bir kütüphane idi. Paket `whoosh` içerik indeksler, ve
bir indeks dizini altında gerekli dosyaları yazar. Bu dosyalar
`whoosh` için anlam ifade eden ikisel dosyalar, bu aslında bir
negatif. Bu dosyaların idare edilmesi gerekli. Ayrıca whoosh ile
güncelleme işlemleri biraz problemli.

Sqlite3 içinde işleyen bir diğer çözüm, fts3 daha
kullanışlı. İndeksleme için bir kolonu `fts3` tipinde tanımlamak
yeterli, artık bu kolona konan her veri doküman arama yaklaşımıyla
aranabilir.

 
```
import os

db = "/tmp/books.db"
if os.path.isfile(db): os.remove(db)
conn = sqlite3.connect(db)
c = conn.cursor()
res = c.execute('''CREATE VIRTUAL TABLE BOOKS USING fts3(path TEXT PRIMARY KEY, content TEXT); ''')
```

```
insert = "INSERT INTO BOOKS(path,content) VALUES('%s','%s');"
c.execute(insert % ("book1.pdf",'SQLite is a software...'))
c.execute(insert % ("book2.pdf",'All SQLite is source code...'))
c.execute(insert % ("book2.pdf",'All is code...'))
conn.commit()
```

```
print ('ara 1')
res = c.execute('''SELECT * FROM BOOKS WHERE content MATCH 'sqlite'; ''' )
for x in res: print (x)
print ('ara 2')
res = c.execute('''SELECT * FROM BOOKS WHERE content MATCH 'source'; ''' )
for x in res: print (x)
print ('ara 3')
res = c.execute('''SELECT * FROM BOOKS WHERE content MATCH 'is code'; ''' )
for x in res: print (x)
```

```
ara 1
('book1.pdf', 'SQLite is a software...')
('book2.pdf', 'All SQLite is source code...')
ara 2
('book2.pdf', 'All SQLite is source code...')
ara 3
('book2.pdf', 'All SQLite is source code...')
('book2.pdf', 'All is code...')
```

Veri taban semantiği aslında döküman indeksleme için çok uygun. Bir
"dokümanı" indeksten çıkartmak için o dokümanın olduğu satırı tabandan
bildik SQL komutları ile sileriz. Ya da güncelleriz.

Bir tabanı temsil eden tek bir dosyayı kopyaladığımda indeks bilgisi o
taban ile beraber gider. Whoosh ile ayrı dizindeki (bozulmaya açık)
bir takım anlamsız  dosyalar yok.

Performans oldukca iyi. Bu şekilde on milyonlarca satırı, belgeyi
indekslemiş projeler var.

Sqlite3 direk Python 3 içine dahil, `pip install` ile kurmaya bile gerek yok.

Not: kendi kullanımım için yazdığım arama programı "loogle"
[şurada](https://github.com/burakbayramli/kod/tree/master/loogle).

Not: Bu fts3 yaklaşımı bir zamanlar MySQL'de sevilen `FULLTEXT`
kolon kullanımı ile benzeşiyor.


Kaynak

[https://www.sqlite.org/fts3.html](https://www.sqlite.org/fts3.html)


