# Oracle Kavramları

Oracle kurmak ve bakımını yapması gereken Veri Tabanı Sorumluları
için, işe yarayabilecek bazı kavramlardan bahsetmemiz
gerekiyor. Programcılar için de yararlı olacağını
zannediyorum. Yazdığınız programın sonuç ortamında nasıl çalışacağı
detaylı Oracle bilgisini gerektirebilir.  İyi bilmemiz gereken 3
kavram var. Şematik (schema), Çizelge alanı (tablespace), SID ve bu
kavramların birbiri olan ilişkisi.  SID 'Veri tabanı' kelimesini
kullandığımızda genelde birçok şeyden aynı anda bahsediyoruz. Oracle
paket programının tamamına, içinde bilgi depolayan kayıtları,
kullanıcı isimlerinin toplamina aynı anda veri tabani deniyor. Fakat,
Oracle dunyasinda, bu kavramlari daha da berraklaştırmamız
gerekecek. SID = Veri tabanı diyeceğiz, ve, veri tabanı Oracle paket
programı değildir gibi bir tanım yapacağız.  Peki o zaman, veri tabanı
nedir?  Oracle'a göre veri tabanı, hakkında konuşabileceğimiz en büyük
kayıt birimidir. İçinde çizelgeler, onların yazıldığı dosyalar, bu
çizelgelere erişecek kullanıcı isimleri, ve paraloların toplamına veri
tabanı diyoruz.

Bir proje icinde şu kelimeleri duyabilirsiniz.

* Hangi veri tabanına bağlandın, çizelge bilmemneyi bu tabanda
bulamıyorum...

* Benim kullanıcı ismimi bu veri tabanında da yaratır mısın? Kullanıcı
şifrem kabul edilmiyor

* Fazla veri tabanı yaratmaya gerek yok, bir tane üzerinde çalışsak
olmaz mı?

* Hayır olmaz, çünkü veri tabanı idarecileri iki veri tabanı olursa
idaresi kolaylaşır diyorlar.

Bunları tanımladıktan sonra, SID'e dönelim.

SID, veri tabanına erişmemizi saylayacak bir isimden ibarettir. Örnek
olarak SATIS, BORDRO, MUSTERI gibi veri tabanı isimleri olabilir.
Çizelge Alanı Çizelge alanı, çizelgelerin üzerinde depolanacağı
'dosya' ismidir. Buna çok dikkat edelim. Dosya derken
/usr/dizin1/dizin2/cizelge_alan_1.dbf gibi bir gerçek Unix dosyasından
bahsediyorum. Yani, Oracle veri tabanının, gerçek dünya ile (işletim
sistemi) buluştuğu yere çizelge alanı diyoruz.  Çizelge ile çizelge
alanlarının bağlantısı, alan yaratılırken sâdece bir kere
yapılır. Ondan sonra ne zaman bu çizelgeye erişseniz, önceden
tanımlanmış olan dosyadan oku/yaz otomatik olarak yapıyorsunuz
demektir.

Çizelge alanı yaratmak için, şöyle bir komut işletebilirsiniz.  CREATE
TABLESPACE cizelge_alan_1 DATAFILE
'/usr/local/dizin1/oracle/cizelge_alan_1.dbf' SIZE 200M; Şematik
Şematik = kullanıcı diyelim, fakat Oracle dünyasında şematik biraz
daha güçlü bir kavramdır. Eğer daha basit veri tabanlarına alışık
iseniz, herhalde her kullanıcının, her çizelgeyi görmesine
alışıksınız. Oracle için çizelgelerin erişilip erişilmeyeceği hem
şematik bazında yapılmıştır, hem de, sahiplenme mekanizması yani,
hangi çizelgenin kime 'ait' olduğu da şematik bazında yapılır. Bir
veri tabanına (SID=ORNEK1) bağlandığınız zaman, tabanda bazı
çizelgeleri göremeyebilirsiniz.

Görmek için, belli bir şematik kullanarak bağlanmanız
gerekecektir. Hattâ ve hattâ, aynı isimdeki iki çizelgeyi değişik
şematiklerde yaratabilirsiniz; Oracle bundan yakınmaz. Yani, SID'den
sonra Oracle'ı paylaştırmanın/bölmenin ikinci bir yolu şematik'tir.
Oracle idarecileri (yukarıdaki proje konuşmalarına o kadar aldırmayın)
az miktarda SID ve paylaştırmak için çok miktarda şematik yaratmayı
tercih ederler.  Şematik yaratmak için kullanıcı yaratmamız
yeterlidir.  CREATE USER hasan IDENTIFIED BY hasanslx DEFAULT
TABLESPACE alan1; Ek Bilgiler Kullanıcı MEHMET olarak SID'e
bağlandıysanız, ve CREATE TABLE komutunu işletip bir çizelge
yarattıysanız, bu çizelge MEHMET şematiğine ait olacaktır.  Bu
çizelgeyi başkaları göremez. Görmesi için özel izin 'vermeniz'
gerekir.  Eğer MEHMET kullanıcısı olarak bir çizelge yarattıysanız ve
detay belirtmediyseniz, bu çizelge DEFAULT TABLESPACE diye yukarıda
belirttiğimiz yokluk değeri (default) alan1 altında yaratılır.





