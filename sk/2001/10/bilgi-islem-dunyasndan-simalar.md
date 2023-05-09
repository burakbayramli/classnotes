# Bilgi İşlem Dünyasından Simâlar

Bu yazıda, ABD'de bilgi işlem danışman/finans şirketlerinde
karşılaştığımız programcı ve teknik liderlerin bir profilini ve eğitim
olarak nereden geldiklerini listelemeye çalışacağız.

Üniversite eğitimi, bir başlangıç noktası olarak bilinir. ABD'de
söylendiğine göre, okul mezuniyet derecesi/ortalaması "sadece o ilk
iş" için gereklidir, özellikle yazılım sektörü için bu çok
doğrudur. Bunun sebepleri yazılımın daha bir bilimden ziyade, sanatı
andırmasıdır; Ortada halâ tekraredilebilir sonuçlar verebilecek, sapı
samandan ayıracak bir yazılım bilimi olmadığı için (kod idaresi,
doğruluğu bakımından) bir mühendislikten de bahsedilemez, bu yüzden
her alandan insan rahatlıkla bu sektöre girebilmektedir.  Ve her
değişik eğitim geçmişi, mezunu, bilgi işlem dünyasına daha değişik bir
katkı/fark/güç getirir.

Benim şimdiye kadar tanıdığım simâlar, bilgisayar mühendisliği
(donanım), bilgisayar bilim, DBA (veri tabanı admini), Unix sistem
idarecisi (sysadmin) geçmişleridir. Bu arkadaşların hepsi ile birebir
çalıştım ve yakınen gözleme şansına eriştim.

Şimdi gelelim bu simaların eğitim ve geçmiş durumuna:

Bilgisayar Mühendisleri

Bu, bizim de dahil olduğumuz gurup... Yorumda daha fazla ilerlemeden,
öncelikle bilgisayar mühendisliğinin ne olduğunu iyi bir tanımlamamız
gerekiyor. Türkiye'de Bilgisayar Mühendisliği kategorisi altılnda
"yazılım" öğretildiğini görüyoruz. ABD'de Bilgisayar mühendisliği pür
donanım üzerine odaklanmış bir eğitimdir. Bilgisayarı çipinden, bilgi
yolları (bus) ve bunların birbiri ile nasıl bağlanacağından başlayarak
öğrenirsiniz. Bilgisayar donanım mimarisini (anakart yani) anlamak,
baş görevdir. Kucağınıza atılan bir takım temel elektronik parçalar
ile bilgisayar yapabiliyor olmanız gerekir.

Bu bölümden gelen arkadaşlar iyi programcı olurlar. Birleştirmenin
verdiği bir alışkanlıkla direk "entegrasyona" odaklanırlar, bu yüzden
bilgi işlem sektörünün danışmanlık kolu için kullanılan "sistem
birleştiricisi" görevine şıp diye uyarlar. Tanıdığım tüm CompEng
(bilgisayar mühendisliği kısaltması) bu tür görevlere çok
uygundular. Her nasılsa içimizden bilgi işlemde teknik lider olan bir
tek kendimi biliyorum, fakat bu herhalde yeteri kadar CompEng
tanımadığım içindir :)

Bilgisayar Bilim

Bu bölüm, bilgisayar mühendisliğinden apayrı, hem "pratikte" hem de
"teoride" yazılım ile uğraşan öğrencilerdir. Teorik yazılım, çok aktif
ve zevkli bir konudur, sonuçta uğraştığınız matematiksel ispatlar,
diller, düzenli ifadeler gibi kavramlardır. Ayrıca, Veri Yapıları ve
Algoritmaları I ve II no'lu dersleri mecburidir, bu derste, adı
üstünde orta ve üst karar çetrefil olan her uygulamanın kullanacağı
algoritmalar ve veri yapıları öğretilir. Stack, Queue, Graph ile
beraber, sıkıştırma (compression), en kısa yol algoritmaları, dizme
(sorting) gibi algoritmalar vardır. Teorik bağlamda, "bir programın
durması garanti midir?" gibi teorik konular ile uğraşırsınız, ve bu
sorunun matematiksel bir ispatı vardır.

Bu guruptan çok iyi programcı ve lider çıkmaktadır. Tanıdığım çoğu
teknik lider arkadaş bu guruptandır, tabii bunun sektöre giren
insanların sayısıyla da bir bağlantısı var.  İyi olmalarının
sebepleri: Yani, efendim, bir ödev olarak bir "derleyici" yazması
gereken öğrenci, büyük bir yazılımda nelerin işlediğini nelerin
işlediğini çok çabuk anlayacaktır. Ayrıca, CS'çiler problemleri "dil"
olarak gördükleri için, komut satırı ve Unix programları, felsefesi
ile çok yakınen alakadar olurlar. Bir CS mezunu arkadaşım, bir
kalıcılık aracı yazması gerektiğinde, bir dil tanımlayıp Lex/Yacc ile
(zamanında) şırak diye bir derleyici yazıvermişti. Bunu CS bilgisine
borçludur.  DBA Bir DBA'in bilgi işlem projesi içinde genel itibariyle
tablo yapısı (şema) üzerinde pervane çizeceğini
düşünebilirsiniz.

Fakat bu tipik olmayabilir. Tanıdığım bir DBA lider, bir projede
şemaları atmış yerine UML denen bir görsel dil ile "nesneleri"
almıştı. Ama UML dilin ve kullandığımız metadoloji o zaman çok gençti,
(hâlen de öyledir) ve bu proje uzaya uzaya 1.5 seneye ulaştı. Bu benim
de ilk projemdir, ama daha o zaman bile bariz bir şekilde tüm
programın kağıt üzerinde modellenebileceğine inanmıyordum (hâlen de
öyledir).

En azından ilk modelin ne derece değişebileceğini bildiğim için,
teknik liderle aramızda komik şeyler olurdu. Çizdiği bir modeldeki üç
seviye kalıtımda (inheritance) orta katmanı gayri ihtiyari
karaladığımı gördükten sonra (çünkü gereksizdi), niye karaladığımı
öğrenmek için uzun süre konuştuğunuzu hatırlıyorum. Çok basit: Orta
katmadaki nesne üzerinde tek bir ek özellik, metot yoktu!  Bu arkadaşa
buradan selam yolluyorum. Kendisinden Oracle hakkında çok şey
öğrendim.

Bir başka DBA arkadaş, tam veri tabanına sadık bir arkadaştı. Projemiz
çamura sapladığında "kurtarıcı" olarak gelen özel timin bir
parçasıydı. Amaç: Projenin ilk safhasında yazılmaya uğraşılan
neredeyse Uygulama Servisi'ne dönüşen kodlardan ve mentaliteden
kurtulmak. Kendi işlem (transaction), kitleme, havuzlama kodlarımızı
yazdığımızı düşünürseniz, bu normaldir. İkinci DBA geçmişli arkadaş,
mimarinin tartışıldığı bir toplantıda, tahtada çizilen veri tabanı
şeklinin yeteri kadar büyük olmaması sebebiyle, birine onu sildirip,
tekrar daha büyük çizdirmişti. Mesaj netti. Herşey için veri tabanının
özellikleri sonuna kadar kullanılacak.

Unix Sistem İdarecileri

En iyi anlaştığım gurup. :) Niye bilmiyorum, herhalde bir zaman
gizliden gizliye bir sysadmin olmayı istediğim içindir :) Nokta com
çözümleri üreten bir şirkette, sysadmin temelli teknik liderler
sayısında enflasyon yaşayan bir şirkette çalıştım. Hepsi de
projelerini zamanında bitirmişler ve yüksek performanslı sistemler
kurmuşlardır. Kod temizliğine verdikleri önem, kişiden kişiye
değişmektedir, fakat çıkacak kod için şunu söyleyebilirsiniz. Yük
testi yapılmış olacaktır.  Sistemleri çok iyi anladıkları için,
ölçekleme konusunu iyi bilirler, ve senkron, asenkron kavramları ile
düşünebilirler. Parallellik, betikleme, geliştirme ortamı kurma
(genelde Unix üzerinde) hiç problem olmaz. Müşteri ile yapılan ve
donanımın tanımlandığı toplantı herhalde en sevdikleri toplantı
olacaktır. Zamanında bluelight.com (K-Mart'a bağlı, günde aldığı
ziyaret milyonları bulacak) sitesinin teknik ağ mimarisinden bahseden
sysadmin temelli teknik liderin, mimariyi ağzı sulanarak anlattığını
hatırlıyorum. "Şuradan bir Cisco switch girdik, fail-over 4 node
koyduk", vs gibi.  Karakterler bunlar.  Hiç elektronikçi ile
çalışmadım, fakat CompEng'e benzeyeceğini düşünüyorum. Eğitim olarak
müfredatlar birbirine yakındır.


