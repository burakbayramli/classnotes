# Proje Voldemort - II

Yeni nesil Internet sitelerinin gereksinimleri sebebiyle iliskisel
modelden bir uzaklasma oldugunu onceki yazida belirttik. Buyuk olcekte
calisan Google ve Amazon sirketlerinin sirasiyla Bigtable ve Dynamo
teknolojilerini gelistirmis olmalari raslanti degil herhalde. Bu
ihtiyac, yeni bir trend, ve anahtar-deger veri tabani gelistirme
alaninda bir patlama yaratti - herkes, pek cok dilde, pek cok
sekillerde kendi anahtar-deger veri tabani uzerinde calisiyor.Dogal
olarak, toz bulutu yere indiginde, kazananlar, kaybedenler daha
belirginlesecek.

Bu tür geleceği olan paketi seçmek İT programcısı için önemli; işten
ise, projeden projeye geçerken, yaninda götürebileceği alet kutusunda
daha az değisen araç kümesi taşıyabilir böylece. Bu daha az eğitim
masrafı demektir. Bir araç üzerindeki tecrübelerini üst üste koyarak,
büyütüp, derinleştirebilir.Voldemort'a dönersek:VM onbellekleme işini
tamamen kendi üzerine alıyor. Yeni nesil bir tabandan beklediğimiz
zaten budur; Veri dağılımı, onbellekleme, çöküşten kurtulma gibi
"fiziksel" işleri üstüne alması. Bu tür programlama ile ilgisi olmayan
(olmaması gereken) özellikler için bizim yeni taklalar atmamızın
gerekmemesi.Veri dağılımı hakkında önemli bir nokta, veriyi tekrar
dengeleme (rebalancing) konusu. VM bir anahtara baktığı anda, onun
hangi bölüme (partition) gideceğini biliyor.

Fakat bunu yapabilmesi için bölüm sayısının bir kere set edildikten
sonra hiç değişmemesi gerekli. Eğer 20 tane bölümüm var diyorsanız, bu
database yapısı, hep 20 bölüm ile çalışmalı. Bunun fiziksel makinalar
ile alakası ne?Kurallar şöyle.  1) İsterseniz birden fazla bölümü aynı
makinaya eşleyebilirsiz. 2) Bir bölüm sadece belli bir makina üzerinde
olabilir, başka makina üzerinde olamaz.Burada amaçlanan şudur. Kümenin
fiziksel yapısı değiseceği için anahtardan makinaya gitme kavramı,
anahtardan bölüme gitmeye çevrilmiş, böylece sistemin değişmeyen bir
şey ile çalışabilmesi sağlanmış, fakat siz arka planda makina başına
kaç bölümün olacağına admin seviyesinde karar veriyorsunuz (tekil
makina seçimini VM arka planda kendisi yapıyor). Sisteminizin
ihtiyaçları büyüdükça aynı makina üzerinde daha az bölüm gitmeye
başlıyor.

En son ölçekleme noktasında, artık tek bölüm tek makina üzerinde
oluyor. Bu sistemin gelebileceği en son nokta, bundan sonra
makinaların daha fazla kapasiteye sahip olması gerekli.Bu pek
kısıtlayıcı değil bence, son derece esnek bir yapı. Dikkat edelim,
bölüm sayısı "soft" bir ayar ve herhangi (makul) bir sayı atamanın az
sayıda makine ile çalışırken bile hiçbir ek performans bedeli yok. O
zaman projemizin ilk açılisinda DB bölüm sayısı 30 diye
başlayabiliriz, tek bir fiziksel makina olabilir, sonra daha fazlası
eklenir ve bölümler değişmez. 1-15 arası makina 1'e, 16-30 arası
makina 2'ye gitmeye başlar.Tekrar dengeleme (rebalancing) işlemi işte
budur. Bunun dinamik şekilde yapılması üzerinde çalışılıyor. Şahsen
benim için nihai bağlamda önemli bir özellik, ama yine de projeme hala
bu olmadan başlayabilirim.

En kotu durumda, Amazon EC2'de yeni bir database kumesi baslatirim,
birinci kumeme baglanip tum verileri teker teker okuyup ikinci kumeye
toptan sekilde yazarim. Sonra uygulama kodlarimi birinci kumeden
ikinciye isaret ettiririm. EC2'de birinci kumenin makinalarini
kapatirim. Bu kadar.Onbellekleme hakkinda fazla detaya gerek yok. Bir
buyukluk ayarliyorsunuz, ve islemeye basliyor. Kendi isini kendi
yapiyor.Depolama: Voldemort farkli "depolama" formatlari ile
calisabiliyor.

BDB dosya formatı bunlardan biri, diğeri ise MySQL. Bu noktada MySQL
aptal bir veri kutusu olarak kullanılıyor sadece, biz kullanmayacağız
fakat bu özelliğin olması iyi. Eğer şirkette mevcut MySQL admin
bilgisi var ise, VM ham verisinin yedeklenmesi, vs. bağlamında, bu
bilgiler MySQL seviyesinde devreye sokulabilir. Biz yedeklemeyi BDB
dosya seviyesinde, rsynç ile yapacağız. Çöküşten kurtulma için VM'in
çözümü bir nodun verisini kısmen "yedek olarak" diğer kümelerde
tutmaktır. Eğer bir nod çökerse, aynı bölüm, başka makinadan çalışmaya
devam eder.

Çöken makina geri gelince bölüm bilgisi geri alınır, işler eskisi gibi
devam eder. Bu özellik test edilmiş ve işliyor.Mimari olarak VM
"hiçbir şey paylaşmayan (share nothing)" mimarisidir (çöküşten
kurtulma durumunda olan kopyalamanın "yedekleme" amaçlı olduğunu
hatırlatırım). İnternet'te yüksek ölçeği başka türlü karşılamak mümkün
değil zaten... Veri yatay olarak bölündü mü, tam bölünmeli. Ahmet,
Bora makina 1'de ise, Can, Doğan makina 2 üzerindedir. Bu kişilerin
yan bilgilerinin "tamamı" da aynı makinalar üzerindedir. Hiçbir şey
paylaşılmaz. Böylece isteklerin "çoğunlukla" sadece tek bir makinaya
giderek işlerini bitirebilmeleri sağlanır. Bu paralelizasyonun iyi
işlediği anlamına gelir. Her istek, tüm makinaları, sürekli meşgul
ediyorsa, bölünme iyi yapılmamış demektir. Bu durumda normal RDBMS
yapısına geri dönülmüştür. Hiçbir avantaj sağlanmamıştır.





