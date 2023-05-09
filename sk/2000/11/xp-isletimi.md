# XP İşletimi

Her ikili takım kendine ait kartlardaki geliştirme kalemlerini teker
teker kodlamaya başlar. Her geliştirme kalemi bitince, bu kalemin
'gerçekte' ne kadar zaman aldığını ikili takım not etmelidir. Çünkü bu
ölçü, baştaki tahminlerinizin ne kadar doğru olduğunu kontrol için
önemli bir sayıdır. Takım lideri olarak siz, bu rakamın tahmine ne
kadar uyup uymadığına takımınıza hiçbir sitem, alay, iğneleme
yapmamalısınız.

XP, programcıya tamamen 'güvenen' bir ortamda olmalıdır. Çıkış amacı
ve temeli bunun üzerine kuruludur.  Gerçek zaman not edildikten sonra
özellik, şöyle gözükebilir.

Özellik: Kullanıcı, banka hesapları arasında para transferi yapmak
istiyor.

Programcılar: Ahmet, VeliGeliştirme kalemleri:- Görsel arayüzü JSP/HTML
ile kodla. (Tahmin: 1 gün, Gerçek: 1 gün)- Kullanıcı hesap nosuna
kullanıcı kimlik nosu ile veri tabanındaneriş. (Tahmin: 0.5 gün,
Gerçek: 1 gün)- Aynı işlem (transaction) altında bir hesaptan al,
öteki hesaba yaz,işlem bitir. (Tahmin: 0.5 gün, Gerçek: 0.5 gün)

Her kartın kodlaması bittikten sonra, ikili takım müşteriye giderek
özelliği 'kabul testinden' geçirmesini ister (acceptance test). Kabul
testi, şöyle olabilir.

Özellik: Kullanıcı, banka hesapları arasında para transferi yapmak
istiyor.--Kabul Testleri--Test 1:- Sisteme şifre ile gir.- Transferi
başlatacağı hesabı seç. Hesapta 2,000 olduğunu kontrol et.- Transferin
yapılacağı hedef hesap nosunu seç.- Hesap miktarı 1,000 TL gir.-
Mevcut para miktarı 1,000'e düştü mü?- Evet ise, test geçtiTest 2:-
Sisteme şifre ile gir.- Transferi başlatacağı hesabı seç. Hesapta
2,000 olduğunu kontrol et.- Transferin yapılacağı hedef hesap nosunu
seç.- Hesap miktarı 3,000 TL gir.- Hata mesajı geldi mi? (çünkü mevcut
hesap miktari 2,000 idi)......

Bu kabul testi ideal olarak, planlama toplantısından önce hazırlanırsa
iyi olur. Çünkü bu şekilde hazır olan bir kabul testi, o özelliği
tarif eden 'en iyi' şekildir. Çok katı bir şartlar oluşturduğu için,
programcılar için özellik kodlamasının ne zaman bitmiş olduğunu
anlamak bakımından, kabul testleri onlara yararlı olacaktır.  Fakat
kabul testi yok ise, müşteri özelliği aklında kaldığı kadarı ile
testten geçirecektir.  Kabul testleri için güzel bir yöntem, birim
testler gibi, otomatik şekilde çalıştırılabilen kabul testleri
olabilir. Java Swing için yazılmış olan bir kabul test altyapısını
sitemizden paylaşmıştık. Swing tıklayıcı alyapısı, XML ile tanımlanan
kabul testlerini, otomatik şekilde tıklayarak sonuçlari e-posta ile
bildirebiliyordu.  Bu şekilde bir sistem başta çok zahmetli ise, metin
şeklinde yazılı testler yeterli olacaktır.

Müşteri özelliği kabul ettikten sonra, kart biten kartlar kefesine
konur, ikili takım dağılır ve başka bir ikili takım altında başka bir
kartı kodlamaya başlarlar.  Birim Testleri Kabul testleri özellik
seviyesinde olan şeyler ise, birim testleri nesne (kod) seviyesindeki
testlerdir diyebiliriz. Birim testler, banka hesabından 1,000 TL
düşülüp düşülmediğini kontrol eden bir kod parçası olabilir. Banka
nesnesi üzerinde hesaptanDüş() işlevini test eden, birim testidir.
Fakat yukarıdaki 'kabul testinde aynı şekilde bir test
görmüştük. Demek ki bazı durumlarda, kabul testleri ile birim testleri
arasındaki çizgi bulanıklaşabiliyor. Yani, eğer elimizde 10-15 diğer
işlevi çağıran bir işlem mevcut ise, ve bu işlevi birim test ile
çağıriyor isek, bu test birim testinden ziyade bir kabul testine
benzemeye başlayabilir. Bu gibi şartlarda hangi testi kullanacağınız
size kalmış. Fakat prensip olarak birim testlerini tek bir işlem ve
ufak bir ölçüde yapmaya gayret edin.





