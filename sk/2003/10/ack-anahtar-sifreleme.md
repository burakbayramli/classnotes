# Açık Anahtar Şifreleme

Şifreleme tarihi oldukça eskidir. Millattan önce eski Yunan
generalleri ve kıralları, birbirlerine haber iletmek icin şifreleme
yöntemlerini kullanmişlardı. Şifre yöntemlerinin ilk önce askeri
amaçlar icin kullanıldığı muhakkaktır.  İlk kullanilan sifre
sistemlerinden biri şöyleydi. Mesela, aşağıdakine benzer bir tablo
çıkartılır ve bu tablo mesajı alan/veren arasında tutulurdu. Gizli
mesaj göndermek için, mesajdaki her harfin X ve Y ekseni değerini
bulup, o harf yerine bu deger konulurdu.

Yani, "merhaba" kelimesi 32-15-42-23-11-12-11 sayı dizisi
kullanılacaktı.  1 2 3 4 51 A B C D E2 F G H I/J K3 L M N O P4 Q R S T
U5 V W X Y

Z Bu örnek, "gizli anahtar" yöntemi için örnektir (tabloyu bir nevi
anahtar olarak düsünebilirsiniz). Bu yöntem açik anahtar stilinden
biraz farklıdır. Gizli anahtar yönteminde, anahtar tektir, ve
saklanması gerekir. Çünkü mesajı şifre/deşifre etmek icin bu anahtar
gereklidir.

Fakat, tek anahtar yönteminin sorunları var:

* Anahtar, baskaları tarafından mesajın içeriğine bakarak bazen tahmin
edilebilir.

* Deşifre yapabilmek icin, anahtar öteki tarafa lazım oldugundan,
anahtar verilirken başkalarına kaptırılabilir Bu yüzden alternatif
metodlara ihtiyaç olmuştur.

Açık anahtar, ya da öteki ismi ile, "ikili anahtar" sistemi bu
sorunları çözer. Sistemin temeli şudur.  Mesaj kodlamak isteyen, iki
anahtar çıkartır. Bu anahtarlardan biri saklı tutulur, öteki herkese
gösterilir. Bu iki anahtarı program kullanarak çıkartmanız gerekir,
çünkü anahtarlar birbirine matematiksel olarak bağlıdır. Detayına
inmeyeceğim, fakat bu bağlantı öyledir ki "bir anahtarı kullanıp
şifrelerseniz, öteki anahtarı kullanıp bu mesajı deşifre etmeniz
mümkün olur".  Ne kadar ilginç degil mi? Bu yöntemi kullanarak
yapabileceklerimize gelelim:

* Gizli anahtarı kimseye göndermeniz gerekmiyor artık. Açık olan
anahtarı paylaşmanız yeterli, ve bu anahtarı herkes bilse de önemli
değil.  * Sadece sizin okuyabileceğiniz bir mesaj göndermek
isteyenler, açık anahtarınızı bulup şifreler ise, siz gizli
anahtarınızla şifreyi açabilirsiniz. Böylece size özel mesajları
sadece sizin okumanız mümkün olur.

* İki anahtar arasındaki matematiksel bağlantı öyledir ki, bir
anahtardan ötekini tahmin etmek imkansıza yakındır. Hele hele büyük
anahtar kullanırsanız, şifrenizi çözmek icin bir süper bilgisayarın
yıllarca calışması gerekecektir.

* Siz eğer mesaja geri cevap vermek istiyorsanız, karşı tarafın açık
anahtarını alıp, mesajı şifrelersiniz. Böylece cevabınız istediğiniz
kişi tarafından okunmus olacaktır.

Daha çetrefilli kavramlar

Ana teknolojiden bahsettikten sonra, günlük hayatınızda şifrelerin
sizi nasıl etkilendiğine gelelim. Internet'te gezinirken bazen
farkettiyseniz, bütün siteler kredi kart numarası gibi bilgileri
alırken korunan bir sayfadan bunu yaparlar. Yani, gönderdiğiniz
bilgiler iki anahtar tarafından şifre/deşifre ediliyor
olacaktır. Böylece kredi kart numaranız İnternet'te gezinirken
şifrenizi çözüp numaranızı alamazlar. Numaranızı sadece gönderdiğiniz
site alacaktır.

Bu nasıl oldu? Tabii ki gonderdiginiz sitenin acik anahtarini
kullandiginiz icin. Isleri biraz zorlastiralim hadi. Ya size verilen
anahtar o sitenin degil ise? Mesela bir korsan programci o siteye
girdi, ve acik anahtari kendininkiyle degistirdi. Ve bu korsan, ayni
zamanda bilgi ağınızı bir şekilde koklamanın yolunu buldu (sniff) ve
kredi kartinizin şifresini kırıp ele gecirdi.  Çok uzak bir ihtimal
ama mümkün.  Bu tip problemlere engel olmak icin, İnternet'te
"Güvenlik Otoriteleri' kurulmuştur. Örnek olarak Verisign şirketi
(www.verisign.com) böyle bir otoritedir. Bu kurum, başvuran siteleri
uzun incelemeden geçirdikten sonra, onların anahtarını "kendi anahtarı
ile" şifreler, ve listesinde tutar. Boylece tarayıcınız, herhangi bir
sitenin soyledikleri gibi ABC sitesi olduklarini kontrol etmek icin,
ABC sitesinin anahtarını alır, Verisign listesinden kitli olan aynı
anahtarı alır, Verisign'in kendi anahtarı ile deşifre edip, iki site
anahtarını karşılastırır.  Bütün bunlar otomatikman oluyor tabii,
birşey yapmanıza gerek yok.

Kullanıcının duruma dahil olması bazen lazım olabilir, mesela, "ABC
sitesi güvenilir site değil" diye bir hata mesajı görürseniz. Bu gibi
durumlarda arkanıza bakmadan kaçmanızı öneriyorum, yani o siteden
alışveriş yapmayın.  Bir soru daha duyar gibi oluyorum.. Ya peki
"Verisign'in anahtarını" birisi degistirirse, mesela korsanlar
verisign sitesine girmisse? vs..  Merak etmeyin, Verisign anahtarını
hiç bir yerden "almanız" gerekmiyor. Netscape ve Internet Explorer
programlarının içinde bu anahtar zaten var. Paket icinden bu
anahtarlar ile çıkıyor yani. Boylece, kimse taklit anahtarı kimseye
yutturmaya kalkamıyor.  Not: Tarayıcınızın otorite listesine kendiniz
eklemeler yapabilirsiniz. Yanlız çok dikkatli olun. Size verilen
anahtardan %100 emin olmadan, sakin otorite listenize
almayin. Tarayıcı ile beraber gelen liste zaten yeterli olacaktir.


