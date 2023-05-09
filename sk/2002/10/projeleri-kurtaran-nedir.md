# Projeleri Kurtaran Nedir?

Bilgi işlem dünyasında karşımıza çıkan iki katmanlı projelerden,
günümüzdeki çok katmanlı mimarilere kadar bir programcının ve teknik
liderin kendine sorması gereken en önemli soru şudur.  "Hangi çeşit
bilgi, projemi kurtarır ve zamanında sürüm yapmamı sağlar?"  Bu
sorulara her projenin ya da şahsın değişik cevaplar verdiğini
görebilirsiniz. Bazen de başarılı olan programcılar bile işi nasıl
becerdiklerini kelimelere koyamamakta, içgüdüsel bir halde doğru
alanlara odaklanıp projeyi başarıya kavuşturan bu arkadaşlara ne zaman
bu soruyu sorsak, şöyle bir cevap vermektedirler: "Bilmem, oluyor
işte".

Zamanla, yakından gözlem yaparak ve karşılaştırmalı olarak kendimizin
de içinde bulunduğu projelerden esinlenerek şu cevabı verebiliyoruz.
Bilgi işlem için odaklanması gereken en önemli konu, kullanılan
teknolojinin açıklarını, faydalarını hemen anlayarak, sürekli
karşımıza çıkan genel bilgi işlem problemlerine bu teknolojinin
verdiği cevapları masaya yatırabilen/kavrayabilen zihniyettir.  Bu
cevap bariz gelse de, niye cevabın programlama dilleri, proje idare
teknikleri, nesnesel tasarım, oklar/balonlar çizmemizi sağlayan CASE
araçlari olMAdığını açıklamamız gerekiyor. Çünkü bazılarının cevabı bu
paragrafta yeralan seçenekler olmuştu, ve bu cevaplar verildiğinde
mantıklı olan açıklamalar beraberinde kabul görmüşlerdi.  Niye
Programlama Dilleri Cevap Değil 1996 tarihinde milyon dolarlık bir
proje içinde, milyar dolarlık bir müşterimize CORBA bazlı bir çözüm
vermek üzere kolları sıvamıştık. Takımda her türlü alanda güçlü olan
arkadaşlar vardı; Dili (C++) iyi bilen, proje idarisinde tecrübeli,
işkolu (thread) programcılığını iyi bilen, veri tabanlarında muazzam
bilgili sahsiyetler proje takımımızın üyeleri idi. Bir arkadaşımız,
KKİ sisteminde herkesin aynı şekilde yaptığı bir kodlama hatasını 3
satırlık Perl koduyla düzeltmiş, ve geri kayıtlayarak (check in),
hepimizi bir haftalık düzeltmeden kurtarmıştı.

İnanılmaz bu performans karşısında daha projenin başı olduğu için, işi
5-6 ayda bitireceğimizden hepimiz gayet emin olmuştuk.  Fakat
yukarıdaki kurala dönersek, en önemli olan faktör C++ ya da Perl
dilindeki ustalık değildi. Çünkü projemiz vaktinde bitmedi. Cevap niye
dil değildi?  Bir proglama dili boşlukta yaşamaz. Bilgi işlem işi,
aslında bir tümleştirme (integration) eylemidir, bu yüzden C++ dilinin
'birşeyler yapabilmesi için', mutlaka XYZ kütüphanesinden 'birtakım
kodları' işletmesi gerekir. Bu kütüphane kodları (kod adacıkları),
başlıbaşına apayrı bir dünyadırlar. Neredeyse her kütüphane, aslında
kendi başına bir dil olarak adledilebilir.  Karşılaştırırsak, "Java'ya
Giriş" gibi kitapların öğretikleri dilin, soyut olarak düşünürsek,
tekrarlama, koşullar ve metin bazlı giriş çıkış evreni öngeren bir dil
olduğunu görürüz. Tabii ki bu temelin mutlaka kavranması gerekir, ama
bilgi işlem dünyası için bu kadarı yetmemektedir. Akademik bir ortamda
sadece algoritma odaklı bir çevrede iseniz, sırf dil bilgisi
yetebilir. Çünkü akademik çevrede amacınız daha işlevsel (functional)
olacaktır. Bilgi işlem dünyası tümleştirir, bilgi işlem, çetrefilli
algoritmaların dünyası değildir.

İşte bu sebepten ötürü, dil bilgisini önem sırasında en tepeye
koymuyoruz.  Niye Nesnesel Tasarım Yöntemleri Değil 80'li yıllarda
başlayan, 90'lara damga vuran nesnesel idare teknikleri, bizim
projemizde en gözde konu idi. O zamanın danışmanlık dünyası, nesnesel
tasarımı tamâmen kabullenmişti. Her teknik liderin kolunun altında bir
Gready Booch kitabı, ya da yeni çıkan Design Patterns (Tasarım
Kalıpları) kitapları ile etrafta dolaşmaktaydılar. Bir programcının
bir ötekine söyleyebileceği en ağır hakaret şu idi: "Ama bu tasarımın,
nesnesel değil!". :) Bu tip teknikleri de oldukça iyi bilen takım
arkadaşlarımız vardı. Şahsen yeni okuldan mezun olmuş şahsımız ve
birkaç arkadaş ile beraber, modelleyemiyeceğimiz bir problemin daha
doğmamış olduğunu adlediyorduk. Bunda da haklıydık. Fakat bilgi
işlemde modelleme, yukarıdaki kurala tekrar dönersek, en önemli faktör
değildi. Neden?  Nesnesel modelleyi şöyle düşünebilirsiniz. Önümüzde
gereksinimleri listelenmiş olan bir bilgi işlem problemi olduğunu
düsünelim. Bu problem için, a, b ve c işlemlerinin yapılması
gerekiyor. Dikkat edin, işlemler diyoruz. Yani komut dizisi. Bu
komutların, bir nesnesel programdan ya da LISP gibi işlevsel bir
programdan gelip gelmediği hiç önemli değildir.

Sonuçta herşey dönüp dolaşıp ASSEMBLER koduna, yani makina koduna
dönecektir, değil mi? Bu yüzden, nesnesel tasarım, bir komut dizisini
vesaire şekilde parçalara bölen, taksim eden bir yöntemdir, e tabii
vesaire_2 şeklinde kodu parçalara bölen yöntemler de vardır. Nesnesel
tasarım sihirli bir şekilde programımıza işlev katan bir yöntem
değildir. Nesnesel tasarım, kodun okunabilirliğini, bakım evresinde
rahat anlaşılmasını sağlayan bir düzenleme yöntemidir. İyi yazılmış
nesnesel kodun amacı, gereksiz şekilde tekrar eden kodu
merkezileştirerek, tek bir yere toplamaktır. Böylece yazılmış olan kod
miktarı azalır. Bu iyi bir şeydir. Fakat zil takarak oynamamıza
sebebiyet verecek bir vahşet bir numara değildir.  Cevap Niye CASE
Araçları Değil CASE araçları, balonlar ve oklar çizmemizi sağlayan,
kodumuzu kuşbakışı bakmamıza yardım eden araçlardır. Eğer sunum
(presentation) amacı, ya da kodumuzdan geri-mühendislik yaparak
şekiller üretecek şekilde kullanılırsa, yararlı araçlardır.  Fakat
eğer tasarım/geliştirme sürecinin tam ortasına konulup, kod yazmadan
önce şeklini çizmeniz lazım gibi bir kural konulacak olursa, projeniz
tehlikeye girecektir. Çünkü, CASE araçları da nihayi sonuçta bir
projenin belkemiği olamaz. CASE araçları hâla şekilden kod üretip,
sonra aynı kod metinyazar ile değiştirildikten sonra koddan geriye
düzgün şekilde yeni şekiller yaratabilecek bir halde değildir. Olsa
bile, kod bu sefer iki formatta tutulmuş olacak ve karışıklık
yaratacaktır. Eğer kodlar/figürler ikilisini tamtamına uyum halinde
otomatik olarak tutamıyorsaniz, figürler geliştirme sürecinin merkezi
olamaz.  Otomatik gitme/gelme mümkün olsa bile, kodun tanımlama gücü
(expression power) figürlerden her zaman daha yüksektir. "Bir resim,
bin kelimeye bedeldir" sözü yazılım dünyasında bazen kullanılıyor,
fakat bu söz yazılım dünyası için geçersiz bir sözdür. Yazılımda
geçerli olan şekli şöyle olmalıydı: "Üzerinde anlaşma sağlanmış olan
bir kelime (kod), bin resime bedeldir".  Cevap Niye Proje İdare
Teknikleri Değil?  Bu sorunun cevabında, nesnesel modelleme için
verdiğimiz cevaba bir paralellik göreceğiz.  Proje idare teknikleri,
yapılacak işi her nasıl taksim, işbölümü, görevlendirme ile dağıtıyor
olsa da, sonuçta iş dönüp dolaşıp tek bir şeye bâğlanır. Takım, doğru
alanlara odaklanabilen bir teknik lidere, ve yetenekli takım
elemanlarından oluşuyor mu?

Eğer oluşmuyorsa, takım XYZ projesini diyelim ki 9 ayda
bitirecektir. Eğer biliyorlar ise, 6 ayda bitireceklerdir. Bilinmezlik
seçeneğinde verilen 9 ayı, her nasıl böler, katlar, görevlendirir,
tekrar görevlendirseniz de, sonuçta toplam 9 ay olacaktır. Proje idare
yöntemleri hiç yoktan yetenek yaratamazlar.  Bu yüzden en iyi idare
yöntemi, ortada en az gözükendir. Yani programcının ayaklarına en az
dolaşan proje idare yöntemi en uygunudur. Sitemizde öğretmek için
Extreme Programcılığın seçilmesinin sebebi budur: XP'nin merkezi
bireydir, yöntemin kendisi değil.  Fakat XP bile, yeteneksiz bir gurup
programcıya gaipten teknoloji bilgisi veremez. Bu yüzden proje idare
yöntemleri en önemli faktör değillerdir.  En Önemlisi: Teknoloji
Kullanımı Evet, olmazlardan olura geldik. Niye en önemli konu,
kullanılan teknolojinin açıklarını, faydalarını hemen anlayarak,
sürekli karşımıza çıkan genel bilgi işlem problemlerine bu
teknolojinin verdiği cevapları masaya yatırabilmektir?  Çünkü,
yapılması gereken iş miktarının ne olduğuna karar veren yegane faktör,
teknolojinin nasıl kullanıldığıdır. Bir örnek ile açıklayalım.  Başta
bahsettiğimiz CORBA projesinde karşımıza, o zamana göre çetrefilli şu
problem çıktı. Kullanıcı bilgisayarlar ile servis bilgisayarı arasında
bazı sonuç listelerini (resultset) göndermemiz gerekiyor.  1)
Müşterinin kayıtlı olduğu planlar bir değil, birden fazla. Eğer plan
sayıları mesela 100 tane ise, temel bir veri yapısı (int, boolean,
String gibi) tabii ki kullanamayız, bize bir liste veri yapısı lâzım.
2) Kullanıcı bilgisayar ile servis bilgisayarı bir ağ üzerinden
haberleşiyorlar. Aynı bilgisayar üzerinde değiller, bu sebeple dil
salt seviyesinde düşünülen bir "return liste" gibi bir çözüm bizi
kurtarmıyor.

Bu problem katmanlı bilgi işlem dünyasından çok klasik bir
problemdir. Teknolojiden, idareden, modelden bağımsız düşünün: Gayet
soyut olarak bakalım. Bu problem hangi çok-katmanlı bilgi işlem
projesinde olursak olalım, kesinlikle karşımıza çıkacak.  Bu klasik
problemin klasik bir problem olduğunu bilmek öncelikle tecrübe
gerektir. Bundan sonra, atılacak adım şudur.  Projeniz için seçilmiş
olan teknoloji çorbasına bakarsınız. Hangi teknoloji bu probleme çözüm
olacak? Bizim örneğimiz için cevap CORBA.  CORBA'nın bu soruna verdiği
cevap nedir? String[] gibi bir tip çeşidi buluyoruz. Fakat bir ağ
ortamındayız, iki bilgisayar birbirinden uzakta. Ya 1000 sonuç
göndermemiz gerekiyorsa? Bir seferde 1000 tane kayıt ağ üzerinden
gönderemeyiz ya?  Kullanıcı tarafında görsel programı şöyle
değiştirelim. Listeyi parça parça göstersin, ve "Sonraki" diye bir
düğme kullanıcıya sonraki parçayı göstersin.  Peki CORBA otomatik
olarak 1000 elemanı parçalara bölüp öyle geriye gönderebiliyor mu?
Cevap hayır.  İşte kullandığımız teknolojinin zayıf tarafını
bulduk. Şimdi bu zayıflığın üstesinden nasıl geleceğiz? Bulacağımız
cevap yeni baştan bir uygulama servisi yazmayacak şekilde, kısa,
etkili, kötünün iyisi bir cevap olmalı.

Potansiyel Çözüm 1: Kullanıcı tarafı, CORBA bağlantısı üzerinden bir
servis nesnesine bağlantısını koparmasın, o servis nesnesi de son
işletilen veri tabanı sorgusundan gelen sonuç listesine olan
bağlantısını koparmasın. Kullanıcı tarafı yeni parça istediğinde, yeni
parça mevcut veri tabanı bağlantısı üzerinden verilsin.  Olmaz. Çünkü,
bir çok-kullanıcılı mimariyi ölçeklemek istiyorsak, veri taban
bağlantısını tek bir kullanıcıya bağlı tutamayız. Bâğlantılar veri
taban dünyasında en pahalı kaynaktırlar, çok tutumlu kullanılmalari
gerekir. Eğer kullanıcı-bağlantı birebir bağlanırsa, mimarimiz iki
katmanlı demektir, üç katmanlı değil! CORBA, RMI, Servlet'leri boşu
boşuna kullanmış olurduk. Veri tabanlarının bu özelliğini bilmek,
kuralımızın tekrar ilginç bir şekilde ispatladığı üzere, kullandığımız
bir teknolojinin zayıf tarafını bilmekten geçer. Veri tabanlarının
eşzamanlı şekilde servis edebildiği kullanıcı sayısı, veri tabanının
açılmasına izin verdiği "bağlantılar (database connection)" ile
orantılıdır.  Birinci çözümü attık.  Potansiyel Çözüm 2: Servis
nesnesi, liste istendiğinde listenin bir parçasını geriye göndersin
(güzel). Bu işlem yapıldıktan sonra veri taban bağlantısını koparsın
(güzel). Şimdi ilginc tarafa geldik: Listenin ikinci parçasını nasıl
bulacağız?  Şöyle yapalım: Oracle'ın ROWID denen özelliğini
kullanabiliriz. ROWID, her satıra verilen özebir (unique) kimlik
numarasıdır. İlk parçayı geri gönderdikten sonra, bu listenin ilk
parçasının en son elemanının ROWID'sini bir tarafa yazalım. İkinci
parça istendiğinde servis nesnesi aynı sorguyu tekrar işletsin, ama bu
sefer sadece son ROWID'den büyük olan satırları geri göndersin. Bu da
ikinci parça olur.

Bu çözüm iyiye benziyor. Fazladan yazılan, yani CORBA'nın zayıflığı
yüzünden yazmamız gereken kod miktarı çok değil. Kötünün iyisi bir
çözüm bulduk galiba. Hem veri tabanı bağlantısına denize düşen yılana
sarılır gibisinden sarılmadık, hem de bir sürü yeni kod yazarak
projeye fazla yük getirmedik. Her taraftan omuzumuza düşmüş olan
gereklilikleri gözettik, ve bir çözüm bulduk.  Ve işin özünü böylece
açıklamış olduk. Dikkat ederseniz, potansiyel çözümlerden bahsederken,
ne XYZ modeli dedik, ne CASE aracından şöyle balonlar çizelim dedik,
ne de "sayın arkadaşlar, Extreme Programcılık bağlamında yönteminde
birileri ABC özelliğini karta yazsın, bir tahmin yapsın ve kodlamaya
başlasın" dedik. Bunların hiçbiri bize çözüm için yardımcı
olmayacaktı.  Doğru çözümü bulduktan sonra yazılması gereken kodu,
istediğimiz şekilde keser biçer taksim edebilir ve bir temiz nesne
modeli yaratabiliriz.  Doğru çözümü bulduktan sonra harcanması gereken
zamanı Extreme Programcılık ile istediğimiz gibi taksim eder, takip
edebilirdik.  Doğru çözümü bulduktan sonra bir CASE aracı ile (ya da
Visio ile) cözümümüzü anlatan şekiller çizebilir, müşterimizi, takım
arkadaşlarımızı bilgilendirebilirdik.  Ve en son olarak, doğru çözümü
bulduktan sonra istediğimiz dilde (o dilin izin verdiği şekilde)
kodlayıp işi bitirebiliriz.

Yani: İkinci derece önemde olan eylemleri, birinci derece önemli olan
bittikten sonra istediğimiz gibi yapabiliriz. Fakat ikinci derece
önemde olan eylemler, projeyi kurtarmaz. Önce yapılması, odaklanılması
ve katiyetle öğrenilmesi gereken birinci derece önemli olan
eylemdir. Yani teknolojiyi doğru şekilde kullanabilmek, anlayabilmek,
gerekiyorsa etrafından dolaşabilmek.  Yanlış anlaşılmaması için tekrar
belirtmeyi uygun görüyoruz. Tabii ki nesnesel modelleme, mevcut diğer
kod düzenleme yöntemleri arasında en iyisidir. XP'nin esnek bir yöntem
olduğu muhakkaktır, programcıya rahat nefes aldırır, ve proje içinde
özgürlük sağlar. Evet Java rahat bir dildir, ve revaçta olan nesnesel
dillerin arasında en temiz sözdizim yapısına sahip olandır. Fakat
bunlar bilgi işlem dünyasında tabiri yerinde ise, "buzdağının görünen
kısmıdır". Bu yazımızın amacı, düşüncesel, bakış açısı olarak içinde
bulunmamız gereken zihin durumunu yansıtmaktır. İkinci derece önemli
yöntemleri gereksiz olarak etiketlemek değil.  Doğru Çözümün Bir
Tarifi Daha Doğru çözümden bahsederken, bu sonuç çözümün belli
karakteristiklerinden bahsetmeyi uygun görüyoruz. Bu
karakteristiklerin en önemlisi, çözümün basit olmasıdır.

Mühendislikte de, temel bilimde de basitliğin önemi büyüktür. Bilim
dünyasında basitliğe vurgu, Occam'ın Usturası (Occam's Razor) adlı
özdeyiş ile yapılır. Occam, şöyle demiştir: "Hiçbir şeyi gereksizce
çoğaltmayalım", bunun Latinceden, İngilizceye ve günümüze taşınması
sonucu özdeyiş şu hali almıştır: "İki hipotez arasında, daha basit
olan hipotezi seçiniz". Bilim dünyası da Occam'ın zamanından beri
böyle yapmıştır.  ABD'de programcılar arasında da yaygın bir deyiş
vardır: Çözüm basit ve aptal olsun (keep it simple, stupid - KISS)
Yâni çözümü hazırlarken, bunu elimizdeki teknolojilerin açıklarına,
güçlü taraflarına göre kurar, ve çözümün her zaman basit olmasına
dikkat ederiz. Bu basitlik, her seviyede geçerli bir basitlik
olabilir: Kodlamada basitlik, idarede (administration) basitlik, kod
miktarının az olması, az sayıda dış arayüz kullanılıyor olunması,
vs..vs..



