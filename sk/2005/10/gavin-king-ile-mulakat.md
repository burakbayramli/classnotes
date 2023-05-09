# Gavin King İle Mülâkat

![](KingBig.jpg)

Gavin King, Java için bir nesne/ilişkisel serbest yazılım ürünü olan
Hibernate projesinin kurucusudur. Gavin, Melbourne'da yaşamaktadır, ve
burada uzun süredir J2EE danışmanlığı yapmakta, açık yazılım ve çevik
metodları yaymak için uğraşmaktadır. Gavin şu anda Expert IS
şirketinde çalışıyor.

Gavin, bize biraz kendinden ve uğraştığın projeden bahsedebilir misin?

Hibernate projesinin kurucusuyum. Şu anda bâzen öteki projelere de
girmem gerekiyor, çünkü bu projeler Hibernate'in de kullandığı
araçlar; XDoclet ve Middlegen gibi. Bu tür yardımlara devam etmeyi
planlıyorum, ve tabii ki Hibernate ile tam zamanlı uğraşıyorum ve
gelişmesi için uğraşıyorum. Fakat çok fazla değişik proje ile
uğraşmamaya özen gösteriyorum, çünkü temel işler ve başladığımız temel
misyona en fazla zamanı ayırmak istiyorum.

Hibernate projesi nasıl başladı?

Hibernate'i başlattım çünkü EJB CMP ve EJB 1.1 ile
uğraşırken, en önemli olan iş mantığı ile uğraşmak yerine, CMP/EJB
teknolojisinin boşlukları/eksikliklerinin etrafından dolaşmak ile
uğraştığımı farkettim. Çok fazla altyapı kodlaması yapıyordum ve
bundan hoşnut değildim, ve bütün bu Entity Bean'ler yüzünden
kaybedilen efordan sonra bile baktım ki eldeki kod halâ bir şeye
benzemiyor, taşınabilir değil, tekrar kullanılır hâlde değil. Uygulama
servisi dışında kullanılır değil, başka bir kalıcılık mekanizması ile
kullanılabilir durumda değil, ve test edilebilir durumda hiç değil. Bu
tür bir sürü problem... Bundan önce, TheServerSide sitesindeki
forumlarda bu tür problemlerin varlığını hep işitirdik, şimdi bu
sorunların gerçek olduğunu tecrübeme dayanarak söyleyebilirim.  Bu
sorunlar gerçek. Bu kadar insan Entity Bean teknolojisini bilmediği
için bunu söylemiyor, bu teknolojinin (Entity Bean) yapısal
problemleri var, ve bunları söyleyenler uzun süredir Java servis
tarafı uygulamaları geliştiren insanlar.  En son sürümler EJB 2.0 ve

EJB 2.1 bu problemlerin bazılarını çözmedi mi?

Evet ilerleme var, fakat Entity Bean'lerin en temel problemi olan
geniş arayüzlü olan bir bileşen ile, iş alanı (domain) parçası bir
kalıcı (persistent) nesnesinin birbirine uydurulmaya çalışılması
problemi halen mevcut. Ve evet, EJB 2.0 bir ilerlemedir, fakat halâ
koca bir özellik eksik: Kalıtım. Röportajın daha ilersinde herhalde
öteki EJB eksikliklerine gireriz, biraraya toplama (aggregation) ve
izdüşüm (projection) ve diğer eksikler. Bence yapılması gereken
ilişkisel model ve nesnesel modelin biraz daha birleştirilmesi, ve iki
model ile aynı anda çalışabilmemizin sağlanması.

Sizce Hibernate neden bu kadar başarılı oldu?

Bunu bizim Wiki sistemi üzerinde bir liste olarak yazmaya çalıştık,
çünkü bu konu hakkında hakikaten kafa yorduk. Liste, "bir açık
yazılımını başarılı yapan faktörler" olarak geçiyordu. Bu listede
birçok şey var, fakat bence en önemlisi bir yazılımın etrafında bir
topluluk, bir cemaat oluşturabilmemizdir. Öyle ki, kullanıcıların
isteklerine kulağımız açık olması çok önemliydi ve yazılımda sorun
çıktığında tamir edilerek çabuk bir şekilde kullanıcın problemine
yardım edilmesi isabetli oldu. Kullanıcılarımız bir tavsiyede
bulunduğu zaman da, ciddiye alınmış olmaları yazılım etrafındaki güçlü
topluluk ruhunu teşvik etmiştir zannediyorum. Topluluk ruhu bu
projenin etrafını sarmalayan güzel bir şey, ve proje bu sayede
topluluğun gurur duyacaği bir eser haline geliyor. Bir başka yazılım
projesinde, mesela, belki çok fazla akıllı insan olabilir ve bütün bu
programcıların hepsinin çok güçlü fikirleri olabilir, ama bu fikirler
çok çatışır ve anlaşmazlık olursa, beraberlik bozulabilir. Anlaşmazlık
tabii ki normal, değişik fikirler de öyle, fakat bir açık yazılım
geliştirmek istiyorsanız bence biraz birlik halinda olmanız
lazım. Birşeyleri beraber yaptığımız hissini yaşıyor olmamız lazım.
İkincisi, proje belgelerimiz (documentation). Revaçtaki çok kaliteli
olan birçok açık yazılım projesi, eğer belgelemeyi şöyle bir gurur
ruhu ile yapabilselerdi ne kadar daha çok daha başarılı ve proje
olarak daha iyi olurlardı! Bu benim için yaptığım işten gurur duymak
gibi bir şey. Güzel bir iş yapınca, bunu belgeleyip herkes ile
paylaşmak istemez misiniz? Herkes Hibernate'in belgelerinden iyi
şekilde bahsediyor, çünkü bu belgeler ile gurur duyuyoruz. Bunu
herkesin duymasını istiyoruz.  Üçüncü olarak, ve bu kanımca diğer açık
yazılım projeleri tarafından da uygulanmalı, bu projeye başlarken
şöyle dedim: "Kalıcılık konusunda uzman değilim, bütün cevapları
bilmiyorum, ama kullanıcılar ile konuşarak, istediklerini öğrenerek
onları tatmin edecek bir çözümü sağlamaya çalışacağım". İlginçtir ki
şu anda Hibernate, benim başta hiç luzumlu olacağını zannetmediğim
özelliklere sahip. Hatta bu özelliklerin bazıları Hibernate'in en
popüler ve sevilen özellikleri!  Kullanıcılar bana gelmişler ve
demişler ki, "aslında ihtiyacım olan şöyle ve şöyle bir
özellik". Burada tarif etmek istediğim galiba "uzmanlar" tarafından,
tepeden indirilen bir tasarım yerine, kullanıcıların yönlendirdiği,
evrim gecirerek büyüyen bir tasarım şekli. Bazen diyorum ki, herşeyin
merkezi kontrollü olduğu bir ekonomi işlemezse, herşeyin merkezden
yönetildiği bir yazılım türü neden işlesin?

Biraz önce bahsettiğiniz bu en iyi özellikler hangileri?

Bu projeye başladığımda bakış açım bir pür Java geliştiricisininki
gibiydi. Hibernate'in sunduğu hizmet düşünülürse, başta SQL bilgimin
iyi olmaması, programcıların SQL ile yapmaya çalıştıklarını bilmemem
komik gelebilir. Ayrıca veri ile "veri kümeleri" olarak çalışmaya da
alışık değildim. Java Kalıcılık kavramı pür nesne bazlıdır, ve tekil,
tek nesne etrafında döner. Bir nesne alırsınız, onunla bazı işlemler
yaparsınız, sonra oradan başka bir nesneye atlarsınız ve sadece bu
nesne ile işlemler yapmaya devam edersiniz. Fakat öteki taraftaki
ilişkisel veri tabanları içinde apayrı bir dünya var, bu dünyada veri
kümeleri ile iş yapıyoruz. Bence bu iki temsil şekli ile de iş
yapabilmemiz lazım.  Ve kabul edeyim, başta bu problemleri veri
kümeleriyle çalışma ile alâkalı problemler olarak görmüyordum. Fakât
kullanıcılar ile konuşurken bu gündeme geldi, "biz bu şekilde çalışmak
istiyoruz" dediler. Birden bire kristâlleşen bir sonuç şu oldu ki,
nesne kümeleri ile çalışmamızı sağlayacak bir tasarım yapmamız
lazım. Yâni, soyutluk derecesi öyle olmalı ki, nesneler üzerinden hem
onların metotlari, özellikleri, kalıtımları ile uğraşabilelim, hem de
kümesel planda biraraya toplama, dışsal birleşim (outer join), iç
sorgular (nested queries), sıralama, vs. gibi özellikleri
kullanabilelim. Ve bu kümesel işlemler Java tarafında olmayacak, veri
tabanı içinde işletilen şeyler olacak.  Hibernate ile yarışan diğer
ürünlere bakarsanız, işte bu sorunu, yani iki dünyayı birleştirme
sorununu çözmeye uğraşmadıklarını göreceksiniz. Genelde nesne/tablo
eşleme sorununu bir "köprü kurma" ya da eşleme problemi olarak
duyarsınız, bence probleme böyle bakılmamalı, problem bir dünyaları
birleştirme problemi olarak görülmeli ve nesne kümeleri ile çalışmayı
öngeren çözümlerde odaklanmalıyız.  Nesne ağını, nesneden nesneye
atlayarak gezmenin nesi var? Sorgulara niye ihtiyacımız var ki?  İlk
önce, eğer arkada bir ilişkisel veri tabanı var ise, nesni ağını
göstergeç takip ederek gezmek, performans olarak çok pahalı bir işlem,
özellikle veri tabanı fiziksel olarak başka bir makina üzerinde
yaşıyorsa. Eğer bu şekilde nesne gezen kod yazmışsanız, eninde sonunda
N+1 problemi ile yüzyüze gelmek zorunda kalacaksınız. Bunu derken
oldukça "genel" bir N+1 probleminden bahsediyorum, BMP ile uğraşanlar
bunu iyi bilecektir, ufak ufak parçalar halinde veri aldığımız zaman
ortaya çıkan N+1 problemi yani. Bunu yapmak ilişkisel veri tabanları
üzerinde performansı öldürecektir.  Bir diğer karşılaştığımız problem
de, nesne/tablo eşlemesinin herkes tarafından bir acele yama (hack)
olarak görülmesi idi. İdealist yaklaşımlardan kaynaklanan fikirlere
göre, "aslında bizim arkada kullanmak istediğimiz bir nesnesel veri
tabanı (object database) idi, fakat o yok, o zaman nesne veri tabanına
uygulayacağımız API'ları ilişkisel veri tabanına uygularız olur
biter". Bu yaklaşım ne yazık ki işlemiyor, çünkü ilişkisel veri tabanı
kökten farklı bir yapıda. İlişkisel veri tabanında veriye erişmek
nesnesel veri tabanında veriye erişmekten çok daha farklı.  Bu iki
yaklaşımı Hibernate nasıl birleştirebildi?  Bizim için en önemli, ve
çabalarımızın odak noktasını teşkil eden özellik, Hibernate'in
sorgulama dili HQL'dir. Bu dilin sözdizimi, SQL'in ufak bir
uzantısıdır, çünkü insanların zaten kullanmakta olduğu birşeye
benzemesini özellikle istedik. SELECT birşey FROM başkabirşey, falan,
fişman. Herkes ilişkisel bir sorgunun neye benzediğini bilir, bu bizim
icin önemli idi.  Utanarak söylemem gerekiyor ki, Java dünyasına
programcılar çoğu zaman "ilişkisel şeylere" bakıyorlar ve biraz kirli
iş olarak görüyorlar, ve bu sebeple kalıcılık katmanına programcıyı bu
kirlilikten kurtaracak, izole edecek bir şeymiş gibi bakıyorlar. Fakat
biraz oturup bu ilişkisel model ve ilişkisel modelleme üzerinde biraz
düşünürseniz, farkedersiniz ki aslında çok zarif ve güçlü bir yapısı
var. Onu kirli yapan "uyuşmazlık problemi", "eşleme problemi", işte
bunlar. Onu kirli yapan JDBC ile çalışıp, JDBC'den gelen veriyi
çetrefil bir nesne ağına dil eşlemeye çalışmak. Kirli olan iş, SQL
yazmamak, SQL ile çalışmamak.  Bizim sorgu dilimiz SQL'i baz aldı. Bu
dilin güzel olduğunu düşünüyorum. Küçük bir dil ama güzel bir
dil. Nesnesel kavramları koyabildiğimiz kadar bu dilin içine koymaya
çalıştık ve koymaya devam edeceğiz.

ODMG'nin OSL standartına baktınız mı? Eğer baktıysanız, sizce yanlış
olan bir tarafı var mıydı?

OQL de güzel bir yaklaşım, ve bazı yönlerden HQL'in bir parçası OQL
ile aynı. Fakat OQL nesnesel veri tabanı öngörüyor, o yüzden meselâ
içinde metot çâğırımı yapabiliyor. Bu tip şeyler ilişkisel veri
tabanlarında geçersiz olan şeyler. Bir de, ilişkisel veri tabanında
null eşittir null, null sonucu verir. Nesnesel veri tabanlarında böyle
olmaz.

JDOQL hakkında ne düşünüyorsunuz?

JDOQL de birşeyler yapıyor işte. Tasarlanma amacı, CICS gibi bir takım
işlemsel (transactional) veri tabanlarına, düz dosyalara, XML
dosyalara, bir ihtimal ilişkisel veri tabanına ve kesinlikle nesnesel
veri tabanlarına erişmek için. JDOQL hiçbir veri depoloma metotunu
öngörmüyor, bu yüzden de ilişkisel veri tabanı ile eşleme yapmak için
gerekli olan fikirleri ekleyememişler. Dışsal birleştirme (outer join)
bunun en güzel örneği, biraya toplama da aynı şekilde. Biraraya
toplama, izdüşüm şeklinde kullanım olmazsa olmazlardan, bizim
kullanıcılarımız bunları sürekli kullanıyorlar.  Depolanmış İşlemler
(stored procedures) kavramını ne tür bir muamele getirdiniz?
Depolonmış işlemler temelde ilişkisel bir veri tabanının, ilişkisel
olmayan bir görüntüsüdür. Dİ kavramı, ilişkisel tabanı çâğırım
merkezli, metot merkezli bir kullanımla görür, bu sebeble, benim bu
konu hakkındaki düşüncem, ki bu düşünce ateşli tartışmalar yaratabilir
çünkü herkes benimle hemfikir değil, bir nesne/ilişkisel eşleme
aracının, nesnelerle tablolar arasında eşleme yapması gerektiğidir,
nesnelerle "başka şeyler" arasında değil.  Tabii bunu söylerken
eklemem gerekir ki, bazıları Hibernate'i depolanmış işlemler ya da
başka tür karışık/hetrojen yöntemler ile veriye erişildiği ortamlarda
kullanıyorlar.  Bu yüzden biz de, karışık yöntemlerle uygulama içine
gelen verileri desteklemek için uğraşıyoruz. Bütün bu desteği de
Hibernate arkasında bir yerlere saklayarak yapmaya uğraşmadık, çünkü
Hibernate tek bir şeyi, ve o bir şeyi çok iyi yapmalı (çevirmen - işte
Unix felsefesi!). Bu ek desteği, Hibernate'den JDBC veri tabanı
bağlantısını kullanıcıya direk vererek sağlıyoruz, böylece kullanıcı
bu bağlanti üzerinde istediklerini yapabiliyorlar.  İkinci olarak
Hibernate içine kod çengelleri takılmasına izin
veriyoruz. Hibernate'in güzel taraflarından biri birleşik model
tipleri denen kavramlar, bir nesnenin özelliklerinin (property)
tiplerinin modeli. İsteğe uyarlı tipler denen bir kavram sâğladık,
böylece kullanıcılar kendilerine göre yeni bir tip
yaratabiliyorlar. Bu yeni tip, bir nesne referansı üzerinden JDBC ile
depolanmış işlem çâğıran bir başka nesne olabilir, LDAP'e bağlanan bir
nesne olabilir, vs.vs.. Tablolar haricindeki antiteleri bu şekilde
desteklemeye uğraşıyoruz.  Siz bariz belli olduğu üzere, saydam
kalıcılık (transparent persistence) hakkında çok heyecanlı ve
tutkulusunuz, fakat bazı büyük paket program satan şirketlere göre bu
müşterilerinin istediği bir şey değil. Bu şirketler saydam kalıcılığı
önemli bir şey gibi görmüyorlar,

Sun'ın içindeki bazı guruplar da böyle düşünüyor. Sizce saydam kalıtım
niye önemli?

İlk önce yorumunuzun ilk kısmını cevaplayayım, müşterilerin ne
istediği hakkında olan bölümü. Benim bu konu hakkındaki görüşüm şu:
Müşteriyi kim olarak tanımlıyorlar? Müşteri dedikleri, bu şirketlerin
sattığı teknolojiler ile günbegün uğraşmak zorunda olan programcılar
mı? Yoksa müşteri diye tanımladıkları satış zamanında karşılaştıkları
alım görevlileri, kodla uğraşmayan kimseler mi? Bunun cevabını
bilmiyorum, soru olarak bırakıyorum. Bana gözüken o ki, Hibernate'e
olan ilginin boyutuna bakılırsa programcılar saydam kalıcılık ile çok,
çok ilgililer.  İkinci olarak, saydam kalıcılık önemli çünkü bu sâyede
iş mantığına daha fazla zaman harcıyabiliyorsunuz. Bu çok
önemli. Saydam kalıcık, bizim gibi altyapı programcılarını, size, yani
kullanıcıya ve programcıya fazla kod yazdırMAmak için uğraşması
yönünde zorluyor. Bir POJO (Plain Old Java Object/Basit Java Nesnesi)
kod satır sayısı bakımından yazabileceğiniz en basit koddur, ve saydam
kalıcılık ile, bu kelimeyi telâfuz etmek Hibernate'in programcıları
olarak bizi "bir POJO'dan daha fazla" kod yazmaya mecbur kalMAmanız
için uğraşmaya mecbur eder. Saydam Kalıtım bu seviyede önemli. Ek
olarak, taşınabilir kod seviyesinde de önemli. EJB 1.1'de yazılmış kod
EJB 2.0'a taşınamaz, çünkü altyapı parmaklarını ve burnunu iş modeli
kodlarının içine sokuyor. Bence iş modeli (business model)
Hibernate'den, JDO'ya, ya da JDO'dan Toplink'e taşınabilir
olabilmeli. Çok uç bir görüş sunuyor olabilirim, ama bana öyle geliyor
ki POJO'lardan bahsettiğimiz an, benim anladığım bütün bir iş
modelinin bir altyapıdan bir başkasına taşınabilmesi özgürlüğünün
olmasıdır. Ayrıca, bu sistem kolay test edilebilen, kolay birim
testlere tâbi tutulabilen bir sistem de olmalıdır. Bu sistem bir
toptan (batch) işlemde de kullanılabilmelidir, eşzamansız bir
dinleyicinin içinde de kullanılabilmelidir.  Ve bütün bunlar çok
önemli. Hernedense EJB'nin "avantajlarından" biri olarak
tekrarkullanılabilirlik (reusability) sayılır. Fakat aslında EJB'ler
en az tekrarkullanılır kodlara sebebiyet veriyorlar. POJO'lar
tekrarkullanılabilir şeylerdir. Kalıtım ile başka bir nesneyi uzatan
(Java extend), gerçekleştirme yapan (Java implement) nesneler,
tekrarkullanılabilir özelliklerini kaybetmeye başlarlar.  Saydam
kalıtıma çok ilgi gösterilmemesinin sebebi şu olabilir mi: Birçok
insan alan modeli (domain model) yazmıyorlar.

Çoğunluk iyi bir yöntem olan böyle bir yöntemi niye kullanmıyor acaba?

İlkin belirteyim, her uygulamanın bir alan modeline ihtiyacı
yoktur. Öyle uygulamalar var ki, alan modeli bu uygulamalar için
fuzulidir. Meselâ öyle uygulamalar var ki "veri tabanından gelen veri
kümeleri" görüşü onlar için cuk oturur. Zâten Java standart alet
kutusu içinde, ya da açık yazılım çevresi içinde bu tür uygulamaları
yazmaya yardım edecek araçlar mevcut. Spring altyapısı var, dağınık,
karmaşık JDBC kodlarından kurtulmanızı sağlıyor, başka araçlar var,
sâdece ekrana listeler dolusu veri basan, ve arada sırada tek tabloya
tek satır ekleyen uygulamalar için size yardım ediyorlar. Böyle
uygulamalar için alan modeline ihtiyacınız yok.  Öte yandan, diğer bir
sınıfa giren uygulamalar var ki aşırı miktarda işlem mantığı
işletmenizi gerektiriyor. Ama hemen ekleyeyim, bu her uygulama
değildir. Ne yazık ki revaçta olan çoğu Java kitabı, insanlara, "işlem
mantığını görsel mantığın olduğu kodlardan kesinlikle çıkarmaları"
gerektiği görüşünü dayatıyor. Fakat yeteri kadar iş mantığınız yoksa,
o seviyede bir ayırıma, o tür bir soyutlamaya ihtiyacınız
olmayacaktır.  Fakat bunu muhakkak yapması gereken uygulamalar
var. İşte bu tür uygulamalar için, düzgün işleyen bir saydam kalıcılık
yönteminin olmaması insanları alan modeli kurmaktan, POJO temelli
sınıflar yazmaktan küstürdü. Java bazlı bir alan modeli hakkındaki
ilginç bir gözlem de şudur; Bu modellerin %98'i ilişkisel bir veri
tabanına yazılacaktır. İnsanların bunu hem temiz hem de verimli bir
şekilde yapacak araçlarının olmaması, bu stilde yazılım geliştirmenin
önünü tıkamıştır diye düşünüyorum.

Bir kalıcılık altyapısını yazarken karşınıza çıkan en büyük zorluklar
nelerdi?

Teknik bağlamda aşmamız gereken problemler aslında işin kolay
tarafıydı. En büyük problemler sosyal, ve iletişimsel
problemlerdi. Algoritma olarak, orta seviye zorlukta, diğer açık
yazılımlardan alıp kullanamadığımız, onlarda da benzerini görmediğimiz
bazı algoritmalarımız var. Yani, orta karar çetrefilli algoritmalar
diyelim.  Hibernate'in iç yapısının nasıl işlediğini insanlara
anlatmak (ve bunu derken iç yapıdan bahsediyorum, anlambilimsel
(semantics) dış yapıdan, kullanıcıya bakan arayüzlerin nasıl
kullanılacağınan bahsetmiyorum) en, en çetin işlerden biriydi. Bir
kişi ile aynı odada olmadığın zaman, kelimeler, diagramlar ile
iletişim kuramıyorsunuz, ve bütün iletişim e-posta'lar gibi metin
bazlı araçlara indirgeniyor. Ben Hibernate'i hep daha fazla kişinin
duymuş olmasını, bilmesini istedim, neyse ki şimdi yeteri kadar insan
var, fakat ilk başta o kodu hakikaten anlayan insan sayısında bir
azlık vardi. Tabii ki Javadoc, kod içine konan kod yorumları yardımcı
oluyor, fakat bu tür teknik iletişim için en ideali, öteki insanın
yanına oturmak ve anlatmak, "bak, işte şu, şu, şöyle işliyor!".
Hibernate, JDO teknolojisine de karşı bir duruş olarak
gösteriliyor. Niye?  Ben bunu JDO'ya değil, JDO mentalitesine bir
karşı duruş olarak betimlemek istiyorum. Bizim şu an söylediğimiz,
şimdilik JDO tarifnâmesini gerçekleştirmeyeceğimizdir, çünkü JDO'nun
bazı eksiklikleri var, ve bu eksikler yüzünden kullanıcılar yerel
arayüzleri kullanmak zorunda kalacaklar. Kullanıcılar şu konuştuğumuz
anda, Hibernate ile JDO ile yapamadıkları şeyleri yapabiliyorlar. Eğer
ki eğer JDO 2.0 eksiklerini kapatır ise, o zaman Hibernate bu
teknolojiyi destekleyebilir. Fakat şu anda JDO 2.0 oldukça havada, ne
olacağı, ne yapacağı belli değil. (Çevirmen - bu mülakattan sonra JDO
2.0 piyasaya çıktı, ve Gaven King, ünlü başka bir beyanında bu yeni
standartın da halâ temel problemleri barındırdığını söyledi). JDO'yu
gerçekleştirmememizin diğer bir sebebi de, artık bizim için çok büyük
bir engel teşgil etmese de, JDO'nun sadece kullanıcı arayüzlerinde
değil, gerçekleştirme seviyesinde de bazı zorunluluklar
getirmesi.. TheServerSide'da bir süre önce birçok tartışma olmuştu,
JDO'nun öngördüğü gibi baytkodu işlemden geçirmek gerekli mi, iyi mi,
kötü mü, kalıtım kullanarak başka şeyler yapılabilir mi.. Bu konular
hakkında birçok karışıklık var.  Benim görüşüm şöyle ki, eğer saydam
kalıcılık yapıyorsanız, baytkod işlemeye ihtiyacınız var. Baytkod
işlemenin külfetli bir şey olduğunu imâ etmek istemiyorum, ama JDO'nun
bunu zorunlu yapmasına gerek olmadığını imâ etmek istiyorum. Yâni,
belki de JDO tarifnâmesine iki aşamalı, iki türlü bir uyum kriteri
olabilir. Biri, işlerkod seviyesinde bir uyum kriteri olur, ve
derlenmis POJO'lar bir üründen ötekine taşınabilir, bir de ikinci
seviyede bir uyum kriteri olur, buna göre kaynak kod taşınabilir hâle
gelir. Bu işlerkod taşınabilme probleminin nereden geldiğini ben de
bilmiyorum, kimin çözmeye uğraştığından da haberim yok.  Bana göre,
kullanıcılar kaynak kod seviyesinde bir taşınabilirlikten mutlu
olurlar, ve bu yeterli olur. JDO bazı gerçekleştirim üzerinde koyduğu
zorunlulukları gevşetirse de iyi olacak, mesela Java Reflection
yöntemi kalıtım kodu yazmak için hârika bir yöntem, bu teknolojiyi çok
seviyorum. JDO'nun bu yaklaşımı da içinde barındıramaması için hiçbir
sebep yok. Bence tarifnameleri değiştirerek, iki yaklaşımı da
barındırabiliriz. Tepkilerim genelde bunlar, fakat başa dönmek
gerekirse JDO ile en temel sorunum, kullanıcılarımızın bizden JDO'nun
yapamacağı şeyler istemesi.

JDO'ya neler eklenirse sizin desteğinizi alır?

Anlatayım. bir numara sorun şu: Klasik nesne/tablo eşleme dünyasında
veri tabanından birşey yüklemenin, veri almanın iki yolu
vardır. Kriter usulü sorgulama, ve sorgulama dili kullanarak
sorgulama. Bu iki model de sağlam modellerdir, ve bazı özel şartlar
için mükemmel modellerdir. Fakat öyle olmuş ki, sanki JDO'cu
arkadaşlar bu modelden hangisini seçeceklerine karar verememişler, ve
hafiften bir kriter bazlı sorgulama dilini, öteki Java sözdizim bazlı
başka bir tanım diline evlendirmişler, filan, ve bütün bunların nereye
gittiği belli değil. Bu kırma sonuç ta daha önce bahsettiğimiz
sorunlar için uygun şekilde uzatılamıyor, bu sorunlar biraraya
getirme, izdüşüm yapma, dışsal birleştirme yapma gibi şeyler. O yönde
harekete yer bırakılmamış. JDO'nun bir numaralı problemi işte bu,
sorgulama dili zarif değil, ve nasıl uzatılacağı ve yeni özellikler
ekleneceği bariz değil, ve buna yer yok.  Sizin ve Hibernate için
sırada ne var?  Şu anda Hibernate 2.0'ı sürdük, sürüm oldukça sorunsuz
geçti. Çoğu Hibernate kullanıcısı da 1.2.x'ten 2.0'a geçti. Birkaç
tane pürüz çıktı, ama genelde işler iyi gitti diyebilirim. Bu sürüm
uzun bir çalışmanın ve fikirlerin sonucuydu. Sürüm bitti, biz de şimdi
yeni fikirlerle oynuyoruz, yapacak işleri tartıyoruz, belki biraz ara
verip dinlenebiliriz.  Hemen önümüzde yapılması gereken en büyük iş
"dağıtık önbellek" işi, ve bu dağıtık önbellek ile çalışabilen bir
işlem (transaction) yapısı. Orada olan birkaç şey var. Şu JCache
JSR'ını çıkarabilseler (Sun), çok iyi olurdu, ama ondan önce de bizim
yapabileceğimiz işler var.

Baytkod üzerinde değişim yapmanın neresi kötü?

Baytkod üzerinde değişim yapmanın o kadar kötü bir tarafı yok. Baytkod
değiştirmek iyi, eğer 'ihtiyacınız varsa'. Özellikle AOP altyapısında
bazı durumlar var ki alan seviyesinde çağrı yakalama (interception)
yapıyorlar, filan, eğer bunu yapıyorsanız baytkod değişimi lazım,
kullanılması gerekir. Her derleme (build) sürecinin, işleminin bir
parçası olarak her defasında işletilen bir baytkod değiştirme bence
problemli. Bütün geliştirme araçlarınızda bunun desteklenmesi
gerekiyor, yoksa derleme sürecinize bir fazla seviye daha
katıyor. Bunlar aşılmayacak problemler değil tabii. Benim JDO
tarifinde söylenen baytkod değiştirme işlemiyle problemim, zorunlu
hale getirilmesi. Baytkod değiştirmek JDO'ya zarar vermiştir
demiyorum, ama tarifnamede bulunması için güçlü bir sebep olmadığını
söylemek istiyorum.

Hibernate'in bu konuya yaklaşımı nasıl?

Parçası olduğum ve şu anda da sürmekte teknik tartışmalardan biri, AOP
altyapılarını nasıl kuracağız sorusu üzerine idi; Burada Rod
Johnson'un yaptıkları var. Rod bazı fikirlerini kodlamak için bir
hiyerarşi kurmaya başlıyor, dinamik yer tutucu (proxy) yazıyor, ve
gidip gidip sonuç olarak ortaya AspectJ çıkıyor. Bu kodu geliştirme
evresindeyken önüne çıkan seçeneklerden biri, baytkod değiştirme
tekniği imiş, ilginçtir ki şimdiki AOP altayapısı koşma zamanında bu
işi Uygulama Sunucusunun ClassLoader'ına eklemlenerek "aradan
sıkıştırıp" yapmaya uğraşıyor. Fakat buradaki sorunlardan biri bunu
taşınabilir hâlde yapabilmek, çünkü Uygulama Servislerinin
ClassLoader'ları farklı çalışıyor, ve nasıl çalıştıkları hakkında
kontrolümüz yok. Bu çözüm için cevabını ne olduğunu bilmiyorum çünkü
probleme yakınen bakmadım, fakat birkaç kişiden farklı fikirlerin
etrafta gezindiğini biliyorum.  Fakat bu sorunun başka bir açıdan
çözümü var, ki Hibernate'in yer tutucu (proxy) nesneleri bu çözümü
kullanıyor, bu çözümün ismi CGLib... CGLib birkaç ilginç şey
yapıyor. CGLib baytkod üretimi için kullanabileceğiniz yararlı
kodların bir toplamı. Bu konudaki bazı genel ihtiyaçlara çözüm
sağlamışlar. CGLib, yeni bir .class yüklemek için baytkod üretmenizi
sağlıyor. Bunu yapmak, baykodları yüklenirken değiştirmeye çalışmaktan
biraz daha hafif ve ucuz bir çözüm. CGLib yeni bir .class'ı
yükleyebiliyor. Hibernate'in baytkod kullanımı işte böyle.  İşte bu
yer tutucu nesneleri yaratıyoruz, ve bu nesneler JDK'nin dinamik yer
tutucu nesnelerinden daha güçlü. Burada, CGLib'in yaptığı şeyin,
JDK'nin dinamik yer tutucu çözümüne bir alternatif olduğunu
düşünebilirsiniz, üstüne üstlük CGLib JDK 1.2 üzerinde de
çalışıyor. En önemlisi, CGLib ile bir başka nesneden dinamik olarak
kalıtım yapabilmeniz, ya da interface'lerini dinamik olarak
gerçekleştirebilmeniz. JDK'nin dinamik yer tutucusu bunları yapmanıza
izin vermez. CGLib sayesinde her şeyin yer tutucusunu
kurabilirsiniz. Java kütüphanelerinde çoğu zaman, JMX ve EJB'de her
zaman şu kalıp takip edilir: Bazı interface'lerin gerçekleştirimi
tarifname tarafında zorunlu tutulur ki, bu interface üzerinden
yakalama, yön değiştirme ve yer tutucu nesneler kullanarak kod işleten
kapın (container), müşteri ve servis arasına "hissettirmeden" girmesi
mümkün olsun. Bütün bunlara Hibernate'te hiç ihtiyaç yok, çünkü
herhangi bir nesnenin metot çâğrımları üzerinde istediğimiz gibi
yakalama yapabiliyoruz, bütün bunlar sâğlanmış. CGLib'in tarihçesini
merak edenler için ekleyeyim, bu kütüphane BCel ve ASM üzerinde
yazılmış.

