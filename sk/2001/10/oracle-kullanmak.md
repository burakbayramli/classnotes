# Oracle Kullanmak

Oracle, belli başlı veri tabanlarından biri. Veri tabanlarının nasıl
çalıştığını öğrenmek için Oracle biçilmiş kaftan. Geliştirme ortamında
Oracle kullanmak için para odemeye gerek yok. Oracle sitesinden
programı indirmeniz mümkün. Uzun zaman sürecek, buna hazır olun. 1
Gigabayt civarı bir program.  Programı indirdikten sonra, nasıl
kontrol edeceğimize gelelim.  Veri tabanı yaratmak, kullanıcı
yaratmak, tabii ilk önce Oracle'a bağlanmak yapmamız gereken işler
arasında.  Veri bakıcısı olarak bağlanmak için şunu yapın.  sqlplus
/nologin Arkasindan, Oracle sistem idarecisi olarak bağlanmak için
sunu isletin

SQLPLUS> CONNECT INTERNAL;

Sifre girmeniz gerekebilir.  Artik Oracle'i indirip kaldirma islemleri
yapabilirsiniz.

SQLPLUS> startup;

Yukaridaki komut, Oracle baslatmak için kullanilir.

SQLPLUS> shutdown;

Yukaridaki komut, Oracle'i (normal sartlarda) indirmek için.

SQLPLUS> shutdown abort;

Yukarıdaki komuta çok dikkat edin. Mecbur olmadıkça katiyen
kullanmayın. Bu komut Oracle isler kodunda geri dönülmez bir hata
olduğunda işletilir. Bazen Oracle'in temizlik yaptığı sıralarda, veya
Unix sistem idarecilerinin yanlışlıkla Oracle'ın kullandığı dosyaları
silmesi sebebi ile program donabiliyor. Bu gibi hallerde, shutdown
komutu işe yaramayabilir. Gene de shutdown'a 2-3 dakika süre
verin. Eğer hala işlemiyor ise, shutdown abort yapmanız gerekecek.
Kullanıcı Yaratmak Kullanıcılar, şematik ile aynı kavram sayılırlar,
ve yaratmak için 2 bilgi gerekir.

* Kullanıcı için ayrılmış parola

* Kullanıcı olağan bölgesi Parola'nin ne işe yaradığını
biliyoruz.

Olağan bölge şu ise yarar: Eğer kullanıcı Oracle nesneleri yaratırken,
hangi çizelge alanında yaratılacağını belirtmedi ise, varsayılan
çizelge alanına olağan çizelge alanı diyoruz. Sonucta her nesnenin bir
yerde muhafaza edilmesi gerekir, degil mi?

```
CREATE USER hasan IDENTIFIED BY hasan_slx DEFAULT TABLESPACE alan_1;
```

Veri Tabani Yaratmak

Veri tabani yaratmak için komut satırını kullanabilirsiniz. Fakat,
veri tabanı yaratma işi Oracle'da oldukca karışıktır. Eğer elinizde
daha önce işlemiş bir programcık yok ise, en iyisi dbassist adlı
görsel programı kullanmak. Sitemizde genelde görsel tıklanan türden
programları pek tasvip etmiyoruz, fakat dbassist programi bir çok seyi
arka planda otomatik olarak yaptığından yeni baslayanlar için tercih
sebebi olabilir. dbassist, ayrıca, veri tabani çıkartmak yerine, "veri
tabanini yaratabilen" programcık da üretebilir. Bu programcığı örnek
alarak kendi sürümünüzü yaratabilir, ve komut satırından birçok defa
çalıştırabilirsiniz.





