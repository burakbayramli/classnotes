# MongoDB

Json / doküman bazlı çalışan, SQL desteklemeyen NoSQL denen
tabanlardan en ünlüsü şu anda MongoDB. Mongo birleştirme (join)
komutunu desteklemez, dokümanlara tekil olarak erişmeyi ya da
sorgulamayı destekler, Json formatında objeleri tabana alıp
verebilir. Sorgulama şöyledir - eğer dokümanın Json'ı içinde hazı
öğeler set edilmiş, diğerleri edilmemiş ise, edilmiş olan ögeler
üzerinden bir filtre yaratılabiliyor. Büyüktür, küçüktür filtre
durumları içinde yine Json üzerinden bir sorgulama formatı var.

Mongo ile "sema değiştirmek" için `ALTER TABLE` gibi bir komuta gerek
yoktur; yeni bir öğe (kolon) gerekirse, o öğe dokümana eklenir, ve
tabana yazılır. Yani felsefe olarak ilişkisel tabanlardan çok farklı
bir yaklaşımı var. Bu felsefede sema önceden tanımlanan bir şey
değildir. Bunun sayesinde "sema değişimi" sonrası eski verinin
yapısının değiştirilmesine gerek yoktur. Tabii bu özellik uygulama
yazılımcılarına bazı ek sorumluluklar yükleyebilir; belki dökümanları
versiyonlamak gerekecektir vs.

Ölçekleme açısından Mongo'nun ilginç bazı özellikleri var. RDBMS
durumunda bilinebileceği gibi ölçeklemenin standart yollarından biri
master/slave (usta/çırak -bu tercüme daha iyi-) sistemi. Usta/çırak
ortamında ekleme, güncelleme tek bir makina olan ustaya gider, yeni
veri arka planda çıraklara dağıtılır, okuma çıraklardan yapılır. Usta
tek makinadır, çıraklar pek çoktur, "çoğunlukla okuyan (read-mostly)"
uygulamalarda bu sistem iyi işler (mesela YouTube gibi bir uygulama,
insanlar çoğunlukla video seyrederler, daha az yüklerler).

Bu sistemin zayıf noktası ustanın çökme durumunda sistemin çökmekten
kurtulmasının zorluğu. MongoDB'de kopya kümeleri (replica set) kavramı
vardır; Yazım sırasında usta Mongo kümesi içinde oy verilerek seçilir
(tabii ki ustanın içeriği çıraklara dağılıtır), ve eğer usta çökerse
oy birliği ile başka bir çırak hemen usta haline getirilir. Bu özellik
MD'nin paketten çıktığı haliyle sahip olduğu bir özelliktir.

Kurmak

`sudo apt-get install mongodb-org`

Başlatmak, durdurmak

`sudo systemctl start mongod`

`sudo systemctl stop mongod`

Eğer belli bir dizin altına taban dosyaları yazılacak şekilde başlatmak istersek,

`mongod --dbpath /tmp/mdb`

gibi bir komut olabilir. Taban dosyaları `/tmp/mdb` altına gidecektir. 

Python ile erişim için

`sudo pip install pymongo`

Bir doküman yazan örnek kod

```
import sys
from datetime import datetime
from pymongo import MongoClient
from pymongo.errors import ConnectionFailure

def main():
    try:
        c = MongoClient(host="localhost", port=27017)
    except ConnectionFailure, e:
        sys.stderr.write("Could not connect to MongoDB: %s" % e)
        sys.exit(1)
    dbh = c["test_db"]
    assert dbh.connection == c
    user_doc = {
        "username" : "janedoe",
        "firstname" : "Jane",
        "surname" : "Doe",
        "dateofbirth" : datetime(1974, 4, 12),
        "email" : "janedoe74@example.com",
        "score" : 0
        }

    dbh.users.insert(user_doc, safe=True)
    print "Successfully inserted document: %s" % user_doc
    print 'id is', user_doc['_id']
    
    
if __name__ == "__main__":
    main()
```

Önemli bir noktaya dikkat: üstte yazım öncesi taban filan yaratmadık,
peki yazım nasıl işledi? Eğer yazım sırasında hedef taban ortada yoksa
MD onu otomatik olarak yaratır! Bu yaklaşım da RDBMS durumundan
oldukca farklı.

MD tabana asenkron yazımı destekler - eğer üstte `safe=True` seçeneği
verilmezse, yazımın sonucu beklenmez, çağrı hemen geri döner. Bu tür
bir yazım ne zaman gerekli olur? Belki MD loglama için kullanılıyor,
bu durumda hızlı şekilde bilgiyi yazmak önemli, pek çok diğer kullanım
da olabilir.

Ayrıca MD `insert` sırasında mesela w=2 gibi bir seçenek ile, "kopya
kümesi içinde kesinlikle iki makinaya yazım yapılmasını istiyorum"
gibi bir şart getirebilir. Bu makinalardan biri usta olacak herhalde,
diğeri de onun kopyasını içeren çıraklardan biri.

Diğer pek çok özellik var. İyi bir referans kaynağı N. O'Higgins'in
MongoDB & Python kitabı.

GUI

"Acaba vs vs veri tabana yazılmış mı?" gibi incelemeler için MD komut
satırından sorgulama yerine, Robomongo iyi bir araç

[http://robomongo.org](http://robomongo.org)

Linux için olan versiyonu indirdik, `sudo dpkg -i` ile
kurulur. Bağlanmak için makina localhost, port olağan port 27017, ve
olağan (default) taban ise incelenecek taban ismi our. Bağlantı
kaydedilir ve connect üzerinden tıklanarak taban incelenmeye
başlanır. Araç Squirrel, Toad gibi bir araç.

Örnekler

Mongo listeler (collections) üzerinden çalışır. Bir obje
yarattığımızda onu direk yaratmayız, bu objelerin olduğu bir *listeye*
yeni bir objeyi ekleriz.

Üstte kimlik üretilmesini gördük, kimliği kendimiz de tanımlayabilirdik,


```python
from pymongo import MongoClient

connection = MongoClient()
stocks = connection['db1'].stocks
```

```python
sym = "APL"; dt = 20100101; open=11; low=10; price=12
new_row = {"_id": {"sym": sym, "dt": dt },"o": open,"l": low,"p": price}
stocks.save(new_row)
stocks.save(new_row)
```

```text
Out[1]: {'dt': 20100101, 'sym': 'APL'}
```

```python
stocks.count()
```

```text
Out[1]: 1
```

`_id`'yi kendimiz verdik. Böyle olunca ardı ardına yapılan aynı
kimlikli `.save` tek bir obje yarattı. İkinci çağrı birinci üzerine
yazdı.

Sorgulama, Sınırlama

Sıralama ve sonuçları sınırlama örneği. Bazı ek satırlar ekleyelim,

```python
sym = "APL"; dt = 20100101; open=11; low=10; price=12
new_row = {"_id": {"sym": sym, "dt": dt },"o": open,"l": low,"p": price}
stocks.save(new_row)
sym = "APL"; dt = 20100102; open=11; low=10; price=12
new_row = {"_id": {"sym": sym, "dt": dt },"o": open,"l": low,"p": price}
stocks.save(new_row)
sym = "APL"; dt = 20100103; open=11; low=10; price=12
new_row = {"_id": {"sym": sym, "dt": dt },"o": open,"l": low,"p": price}
stocks.save(new_row)
stocks.count()
```

```text
Out[1]: 3
```

Altta tarihle büyükten küçüğe gidilecek şekilde sıralama yaptık (-1
ile) ve `limit(1)` ile alınan sonuçları bir tane ile sınırladık, bu
tabii ki bize en tepedeki sonucu vermiş oldu ki bu tarihlerin en
büyüğüne ait satır idi.

```python
q = { "$query" : {"_id.sym": sym}, "$orderby":{"_id.dt" : -1} }
res = stocks.find(q).limit(1)
res = list(res)
print (res)
```

```text
[{'l': 10, 'o': 11, '_id': {'sym': 'APL', 'dt': 20100103}, 'p': 12}]
```

Sorgularda büyüktür, küçüktür kullanımı için belli bir tarihe eşit ve
ondan büyük olan satırları alalım,

```python
q = { "$query" : {"_id.dt": {"$gte": 20100102 }} }
for x in list(stocks.find(q)): print (x)
```

```text
{'l': 10, 'o': 11, '_id': {'sym': 'APL', 'dt': 20100102}, 'p': 12}
{'l': 10, 'o': 11, '_id': {'sym': 'APL', 'dt': 20100103}, 'p': 12}
```

Silmek

Eğer bir sorgu kriterine uyan objeleri silmek istersek,

```python
stocks.remove( {"_id.dt": {"$gte": 20100102 } } )
stocks.count()
```

```text
Out[1]: 1
```

Tum dokumanlari silmek icin

```python
stocks.remove( {  } )
stocks.count()
```

```text
Out[1]: 0
```

Kaynaklar

[Digital Ocean](https://www.digitalocean.com/community/tutorials/how-to-install-mongodb-on-ubuntu-16-04)
