# Yeni Metod

Yakın zamanda "çevik metod" kategorisi altında yeni dizayn ve kodlama
tekniklerine ilgi artmaya başladı. Bazıları tarafından
bürokrasi-karşıtı, ya da dağınık kodlama olarak görülse de, bu yeni
metod yazılım dünyasında büyük hareket yarattı.

Bu yazıda hafif metodları uygulamanın sebeplerini sayacağım ve
özellikle "değişebilir" özelliginden ve insanları ön plana almasına
değineceğim. Ayrıca, eğer bu dar ve dikenli yoldan geçmek
istiyorsanız, size hangi niyet ile yola çıkmanız daha iyidir, onu
göstermeye uğraşacağım.  Yokluktan Cokluk, Sonra Hafiflik Genelde
yazılım, bir kargaşa durumudur. Kargaşa halinde yapılan bu yazılım
şekline "önce kodla, sonra tamir edersin" derler. Böyle yazılımlar
genelde plansız yapılır. Sistem dizaynı genelde kısa vadeli kararlar
ile yapılmıştır. Bu şekilde yapılan geliştirme her ne kadar ufak
sistem icin idare eder ise, sistem büyümeye başladıkça yeni özellikler
eklemek çok zor hale gelir.

Bu tür sistemlerin kokusunu uzaktan almak mümkündür: Ne zaman yazılıma
özellik eklemesi bittikten sonra, test süreci, gereğinden fazla zaman
alıyorsa bilin ki bu sistemin içinde yanlış çok, ve bilâhere dağınık
bir şekilde kodlaması yapılmış.  Yazılım hayatımız boyunca uzun bir
zaman, bu tip projeleri çok gördük ve hatta yaşadık. Alternatifimiz de
yok değildi: Kontrollu Metodlar denen metodlar yazılım isine disiplin
getirmeye uğraştılar bir süredir. Amaçları yazılım projelerini daha
"tahmin edilir" ve "iyi işler" hale getirmek oldu. Bunu yapmak için
yazılım projesine sıkı bir düzen takip ettirmeye uğraştılar, ve en
büyük önemi de planlama safhasına takdim ettiler. Aslında gizli gizli
öteki mühendislik alanlarına özendiler, mimarlık, inşaatçılık gibi.
Bu kontrollü metodlar bayağı uzun süredir aramızda. Başarılı oldukları
söylenemez ve seveni de fazla yoktur.

Bu metodlar hakkında en çok duydugumuz şikayet "bürokrasi
fazlalığıdır", çünkü izlemek için o kadar çok ekstra faaliyet
gerekiyor ki, proje birdenbire kağnı hızına düşüveriyor. O yüzden bu
metodlara "ağır" metodlar adı takılmıştır, ve ya Jim Highsmith
arkadaşımızın dedigi gibi, "Dev Metodlar".  Fakat yakın bir zaman
önce, kontrollu metodlara karşı olarak yeni bir alternatif çıktı. önce
"hafif" diye bilinen bu metodlar, şimdi "çevik" metodlar diye
biliniyor. Çevik metodları sevenler büyük bir ölçüde ağır metodların
bürokrasisinden bıkmış insanlar. Çevik metodlar, yokluk ile çokluk
arasında çok güzel bir denge buluyorlar bizce, tam kararında ve en çok
getiri verecek bir şekilde bir düzen takip ediyorlar.  Sonuç olarak
çevik metodlar, ağır metodlara kıyasla büyük işletim değişiklikleri
getiriyorlar karşımıza. Mesela çevik projeler ötekilere kıyasla kod
belgelemeye, yani döküman yazma işlemine daha az önem verirler. Çevik
metodlarin merkezi kaynak koddur, çeviklere göre bir kod parçasını
belgeleyen en iyi yer, o kodun kendisidir. Bu örnek tabii ki esas
farkları göstermek için yetmez.

Belgeleme fikrinin altında yatan iki büyük fikir vardır.

* Projeler için uyum sağlayabilmek, geleceği görmekten daha faydalı
olmalı: Ağır metodlar, yazılım işlemini ince detaylı ve uzun vadede
planlamaya uğraşırlar. İşler yolunda gider bir süre, ta ki değişim
kapı onune gelinceye kadar. Ağır metodlar bu yüzden değişimi sevmez ve
hatta durdurmaya uğraşırlar, teknoloji bakımından olsun, müşterinin
istediği özellikler listesi bakımından olsun. Çevik metodlar değişimi
bekler, ve hatta hatta 'ister'. Uyum sağlayabilen metod olarak,
kendilerini değiştirmek bile anlamına gelse, değişime ayak uydururlar.

* Çevik metodlar "insan" merkezlidir, "düzen" merkezli değil: Yani
takip edilen düzen insanların doğasına uygun olarak, onların en rahat
ve severek calışıcağı bir ortam yaratmak için uğraşır.

İlerideki bölümlerde yukarıdaki kelimelerin anlamını daha detaylı
olarak işleyeceğiz, bu sayede uyum sağlayan, insan merkezli düzen
nasıl oluyor anlayabileceksiniz.  Böylece bu şekildeki işlem düzeninin
yararlarını, eksiklerini göreceksiniz, "kullansak mı kullanmasak mı?"
sorusuna cevabı burada bulacaksınız.  Tahmin Edilir mi Olsun, Yoksa
Uyum Sağlayan mı Dizayn ve kodlamayı ayırmak Genelde bugünün yazılım
metodlari, ilham için öteki mühendislik alanlarına dönmüşlerdir,
inşaatçılık ya da makina mühendisliği gibi.

Bu tür "öteki" mühendislik alanlarında, yapımdan önce planlamak çok
önemlidir.  Öteki alandaki mühendis arkadaşlarımız, çok detaylı
çizimler ile neyin, nasıl, ne kadar, ne ölçüde yapılacağını belgelemek
için calışırlar, ve bu parçalarin nasıl birbirine bağlanacağını
önceden gösterirler. Önemli dizayn kararlari (mesela bir köpru
kuruyorsanız, köprü ağırlığı nasıl taşınacak) bu çizimler sırasında
verilir. Çizimler bittikten sonra planlar yapım için başka bir gruba
veya şirkete verilir. Yapım sırasında dizayn planının sıkı şekilde
takip edileceği bilinir, hatasız bina yapmanın başka yolu
yoktur. Yapım grubu dizaynı takip ederken bazı zorluklar ile
karşılasabilir, fakat bu durumlar genelde az meydana çıkar.  Bütün
ölçümler ve detaylar ilk dökümanda olduğu için, 'yapım planı' denen
plan ilk dizayn planını çok kullanır. Yapım planı işbölümü düzeni
kurar, yani, yapılacak işler arasında bağlantıları önceden
düzenler. Bu sayede yapım bütçesi ve süresi tahmin edilebilir. Yapım
planı, işçilerin neyi, ne zaman yapacağını sıkı şekilde tarif ettiği
için, inşaatı kurma zamanı geldiğinde planı takip edenlerin fazla kafa
calıştırması gerekmez. Bu safhâda el becerisi zihin cimnastiğinden
daha önemlidir.

Gördüğünüz gibi, inşaatçılıkta dizayn ve inşaat iki türlü, birbirinden
değişik eylemler. Bir tarafta dizayn, tahmin edilmesi zor, yaratıcı ve
daha pahalı insanlar gerektiriyor, öteki tarafta yapım, daha rahat
tahmin edilebiliyor. Eğer elimizde bir plan varsa, yapımı
planlayabiliyoruz. Elimizde bir yapım planı varsa, düzenli bir şekilde
yapmaya, kurmaya başlayabiliyoruz. Ayrıca önemli bir nokta:
İnşaatçılıkta yapıyı kurmak, planlamaktan her zaman daha pahalıdır ve
daha uzun zaman ister.  Alın size bugüne kadar izlenen yazılım
metodlarinin temeli. Bugünün yazılım projeleri de, yanlış olarak
tahmin edilir bir is düzeni, ve kodlama zamanı için ucuz programcı
kullanabilsinler diye, dizaynı, kodlamadan ayırmaya uğraşmışlardır. Ve
işte o yüzden dizayn için bir metod bulmaları gerekmiştir ki, sonradan
plana bakıp kodu yazmak mümkün olsun, aynen inşaatta olduğu gibi..
Yazılım sektörü için bu planlar ne şekilde çıkar karşımıza? Genelde
bugünün yazılımcıları planlama için dizayn demek, UML gibi ok-ve-kutu
çizdirebilen bir metod kullanmaktır; Akıllarınca, eğer bütün önemli
dizayn kararları UML ile alınabilirse, bu planı alıp kodlama planına
çevirilebilir, ve oradan yazılımcı bölüm bu planı alıp, programı
yazar.  Şimdi biraz daha zor sorular soralım: Kendini koda çevirecek
bir dizayn şekli mümkün mü, ve eğer mümkünse, hangi projenin "yapım"
kısmı plandan daha pahalı ki, böyle bir metod takip etmek finansal
bize açıdan faydalı olsun?

UML şemalarını programlara verilip kodlattırmak kolay bir iş midir? Ne
yazık ki hayır. UML şemaları kağıt üzerinde çok güzel gözükse bile,
kodlamaya gelince büyük boşluklar cikar karşımıza. İnşaat
mühendislerinin kullandığı şema şekilleri yılların verdiği birikime
dayanır, kullandıkları kodlar ona göre anlamlı ve doğruluğu
matematiksel olarak ispatlanır türdendir. UML şemalarının doğruluğunu
ispatlamak için tek yapabileceğimiz şey bir arkadaşa göz atmasını rica
etmektir. En tecrübeli dizayncılar bazen yaptıkları dizayn kodlama
sırasında başaşağı dönünce şaşkın kalıyorlar.  İkinci sorun fiyat
meselesi. Bir köprü inşa ettiğiniz zaman, planlama safhâsı genel
masrafın %10'u kadardır. Geri kalanı inşaata masrafıdır. Yazılım
mesleğinde kodlama safhâsı için harcadığımız zaman bunun tam tersidir,
yani dizayna göre çok daha azdır.

McConnell'a göre büyük bir proje için, zamanımızın %15 kadarı kodlama
ve test etmek ile geçer, yani yüzdeler köprü kurma senaryosu ile
tamamen ters bir haldedir. Test etme süresini bile kodlama ile aynı
yere koysanız, dizayn hala %50 kadar zaman alır. Bu gibi bilgiler
ışığında, dizayn işinin yazılımdaki yerinin, öteki mühendislik
alanlarındaki yerine göre değişikliği iyice ortaya çıkar.  Bu tip
sorunlar, Jack Reeves arkadaşımızı şunu söylemeye itti: "Yeni
Metodlarda dizayn dokumanı/şeması aslında kaynak kodunun ta
kendisidir. Ve yazılım için inşaat evresi, aslında derleme ve bağlantı
programları tarafından otomatik olarak zaten yapılmaktadır". Bu
düşünceye göre otomatik yapılan her şey, yazılımın inşaat evresine
tekabül eder.

Su ana kadar gördüklerimizi özetlemek gerekirse:

* Yazılımda kodlama evresi ucuzdur.

* Yazılım sektörunde büyük efor dizayn için harcanır, o yüzden akıllı
ve yaratıcı insanlar gerektirir.

* Yaratıcı işler, planlaması zor olan şeylerdir, o yüzden tahmin
edilebilir planlar bir hayal olabilir.

* Yazılım mühendisligi için, diger mühendislik alanlarından kavram
kullanmak yanlıştır.

Öteki mühendislik alanları değişik sorunlarla uğraşilar, o yüzden
değişik bir metoda ihtiyaçları vardır.  Yazılım Kapsamı Değişimi
Projelerde bazen garip şeyler duyuyorum. Programcılar bana gelip
diyorlar ki "bu projenin problemi sürekli değişen özellik listesi
(yazılım kapsamı, müşteri kontraktı yani)". Beni en cok şaşırtan ise,
insanların bu değişikliğe şaşırması. Şirketler için yazılan
yazılımlarda, değişim tek sabittir. Esas sorun bu değil, bu değişime
nasıl ayak uyduracağınız.  Bu değişim ortaya çıkınca karşı bir
reaksiyon cesidi, "müşteri ihtiyaçları kapsam grubu tarafindan yanlış
toplanmış" olabilir. Bu yanlış düşünceye göre kapsam grubu
(programcılar yada idarecilerden oluşabilir bu grup) özellik listesini
toplar, müşteri tarafından bu listeye imza attırır, yani kabul ettirip
döndürür ve sabitleştirir.

Böylece düzeni bu sabit merkez üzerine kurup yazılım sırasında
değişimi en aza indirgemeye uğraşılır. Eğer değişiklik gerekmişse, bu
demek ki kapsam grubunun yanlışıdır.  Bu yaklaşımın açık noktası
şurada: İlkinden başlayalım. Bırakın ihtiyaç/kapsam listesinin
tamamlanmasına, listesindeki birimlerin değişik variyasyonlarını bile
anlamak proje başında çok zor bir şeydir. Diğer bir zorluk, ihtiyaç
listesindeki birimlere fiyat biçme zorluğudur: Yani, öyle bir durum
düşünün ki, müşteri şirketi mesela arabasına yeni bir cant istiyor,
ama araba satış görevlisi (sizin şirketiniz) cant için $10 mi yoksa
$10,000 mi isteyeceğini söyleyemiyor. Fiyatı bilmeden müşteri cantı
isteyip istemediğini nereden bilecek?  Yazılım zaman takvimini
yapmanın zor taraflarından biri işte budur: Yazılım işlemi aslında
tamamen bir dizayn eylemi olduğundan, takvime konması zor bir
eylemdir. İkinci bir sebep de: Yazılımın "ham maddesi"
insanlardır. İnsanlar her zaman tahmin edilmesi zor birimler
olmuslardir.

Bilâhere insanların hakkındaki planlama işleminin beklenmez olması
sürpriz olmamalıdır.  Yazılımın elle tutulamayan özelliği de bir
etken: herhangi bir yazılımı kullanmadan, işinize yarayıp
yaramayacağını anlayamazsınız. Anlamak için hiç değilse projenin ilk
sürümlerinden birini kullanıp şöyle bir tartmanız gerekir, ancak o
zaman ne özellikler lazım hangisi daha az lâzım, fikir sahibi
olabilirsiniz.  Bütün bunları bir araya getirseniz görürsünüz ki,
yazılım hakkındaki genel düşünce, rahat değiştirilebilir bir şey
olduğudur. Yazılım, 'yazılan' bir şeydir, yazılan silinir de, tekrar
yazılabilir de. O yüzden, proje kapsamının değişmebilmesi de "normal"
bir şey olmalı. Müşterileriniz de bunu biliyor zaten, hele hele
kendileride bir kaç satır kod yazmışlarsa...  Daha bitmedi: Hadi
diyelim, program kapsamını değişmeyecek bir şekilde belgeleyebildiniz
(bir mucize sonucu). Bu halde bile, şirketin kendisinin düzen
değiştirmeyecegi ne malum? Bugün iyi olan bir kapsam, 6 ay sonraki
sektörde müşteriniz için iyi olmayabilir.

Bütün iş dünyasi durup onları bekleyecek değil ya! iş dünyasının çoğu
değişikligi de böyledir, aksini söyleyen ya yalan söylüyordur, ya da
borsada çoktan milyarder olmuştur.  Tahmin Edilebilirlik İmkansiz mi?
Genel de hayır. Bazen yazılım dünyasinda tahmin edilebilirlik
mümkündür. Mesela NASA'nın uzay mekiğinin yazılım planı tahmin
edilebilir bir plandır. Bir sürü insan, şa-şa lı başlangıçlar, uzun
zaman alan plan/geliştirme yaparak o işe koyulurlar... NASA benzeri
projeler, başka yerlerde de vardır. Fakat 'işyerleri' için yazılan
programlar bu kategoriye girmez. İşyeri programları için değişik bir
düzen izlemeniz gerekir.  Esas önemli olan, ne tür durum içinde
olduğunu bilmek. En kötüsü, tahmin edilemeyecek bir durumdayken,
ettiğini zannetmek. Klasik metodçu arkadaşlarımız genelde bu tür 'dış
iş planı hatlarını' çizmekte pek iyi değillerdir. Hani metodların
uygunluktan geçip uygunsuzluğa girmeye başladığı o gri alandan
bahsediyoruz. Bunun suçu biraz da, metod mucidi olan
arkadaşlarımızda.

Metodlarının 'herkes' tarafından kullanılabilmesini istedikleri için,
etrafına sınır koymaktan genelde kaçınırlar. Bu yüzden takipçileri
olan bazı zavallılar, metodu yanlış yerde kullanma hatası yaparlar,
beklenmez durumlar için 'beklenir olanı seven' metodlar kullanmak
gibi.  Böyle yanlışları yapanı aslında anlıyoruz. Sonuçta bir şeyi
tahmin edebilmek, planlayabilmek arzulanan bir özellik. Fakat tahmin
edemeyecegin şeyleri planlayabilirim sanmak, proje sırasında geri
dönülmez yanlışlara yol açıyor.  Böyle durumlarda plan işlevini
kaybedince, bu durum yeterince kollanamıyor. Yavaş yavaş plan
elinizden kaysa bile, belli bir süre hala bildigim yolu takip ederim
gibi işi 'kıvırıyorsunuz', fakat en sonunda gerçek ile plan arasındaki
uçurum o kadar çok büyüyor ki, film mecburen bitiyor. Acı sonla.  Bu
söylediklerimiz bazılarına ağır gelebilir. O kadar dikkatle kurduğunuz
müşteri idare yöntemleri, proje planları demek doğru değilmis. Fakat
tahmin edebilmek ne kadar güzeldi! Katılıyoruz. Eski alışkanlıkları
bırakmak kolay değildir. Fakat ortada bir problem var. Bu problemin
varlığını kabul etmek, çözmeden önce en gerekli aşama. Merak etmeyin:

Alternatif, kontrolsuz, anarşik planlama/kodlama metodu
değil. Alternatif, bilinmezi kontrol edebilen bir metod. Uyum
sağlamanın önemi işte burada.  Bilinmezi Kontrol Etmek Bilinmez
dünyada, kendimizin nasıl müfettişliğini yapabiliriz? En önemlisi,
projenin yolda yürüyüp yürümediğini nasil anlarız? Bize lazım olan,
bir kendini denetleme mekanizması. Öyle bir mekanizma ki, gerçekçi, ve
kendimizi aldatmamıza yer bırakmayan bir mekanizma.  İste anahtar
cevap: Sık sürüm yaparak geliştirme metodu. Bu aslında pek yeni bir
fikir değil, daha önce başka adlar altında duymuş olabilirsiniz: Azar
azar geliştirme, devrim usulü, basamaklı stil, spiral metodu, vs..vs.
Önemli olan, sık sürüm metodu ile, çok kısa aralar ile proje özellik
listesinden bir kaçı kodlanır kodlanmaz herşeyi paketleyip, bir sürüm
yapmak. Tabii ki bu ara sürümler bütün kapsamı içermiyor, ama en son
planladığınız sistemin özelliklerinden bir kaçını
içeriyor. Unutmayalım, aklımızda sürekli bitiş çizgisi var. Ona göre
kısım kısım sürüm yapıyoruz, öyle kafamıza esen özellikleri koymuyoruz
program içine. Ve, bu ara sürümü, az özellik içerse bile, sanki son
sürümmüş gibi otomatik testlere tabii tutuyoruz (yazabildiğimiz
kadarına), ve iç bağlantılarını doğru şekilde kuruyoruz. Burası çok
önemli.

Çünkü bir proje için, gerçeklerle yüzyüze gelmenin en iyi yolu, test
edilmiş ve "baglanmış" bir sistem ortaya çıkarmaktır. Güzel güzel
yazılan o proje dokumanları ve belgeleri bir sürü yanlışı
saklayabilir. Calışır program bile, test edilmemişse, yanlışlarını
saklayabilir. Fakat bir programın önüne oturup şöyle bir kullanınca,
programcılık hataları, ya da müşteri yanlış anlamak yüzünden çıkan
hatalar hemen ortaya çıkarlar.  Sık sürüm metodu, klasik planli
projeler için bile yararlıdır. Fakat "çevik" metodlar için vazgeçilmez
bir durumdur, çünkü proje kapsam değişikliği ile boğuşmak için başka
yol yoktur. Sık sürüm metodunu takip edince anlayacaksınız ki, uzun
süreli planlar bayağı gevşek, ve bütün dikkatli ve detaylı planlar
hemen bir sonra gelen sürüm için yapılıyor. Böylece temeli yavaş yavaş
büyütüyorsunuz, ve yeri geldikçe üzerine eklemeler yapıyorsunuz.
Aklınıza bir soru geliyordur, her sürüm ne kadar aralıklar ile
yapılmalı? Değişik çevik metodlarden değişik cevaplar
alabilirsiniz.

XP metodu 1 hafta ile 3 hafta arasında değişen bir süre verir. SCRUM
metodu 1 ay der, Crystal metodu daha da uzatabilir. Fakat genel bizce,
her sürümü yapabildiginiz kadar sık yapmak. Böylece sonuç sistemin bir
parçasını daha sık görerek, nerede olduğunuz anlamak daha rahat
olacak.

Uyum Sağlayabilen Müşteri

Uyum sağlayabilen metodlar kullarken müşteri ile ilişkilerin de
değişmesi gerekiyor biraz, özellikle program müşteri şirketinin
dışında yazılıyor ise. Bugünlerde müşteriler ayrı bir programcı
şirkete proje verince, fiyat kontrollu proje vermek istiyorlar. Bu
eski sisteme göre ihale başlatılıyor, fiyatlar toplanıyor, bir fiyata
göre programcı şirket seçilip, proje veriliyor, ve yazılımın bitirme
sorumluluğu bu şirketin üzerinde oluyor.  Tahmin edebileceğiniz gibi,
fiyat kontrollü projeler için bilinen kapsam gerekir, ve klasik metod
gerektirir. Uyum sağlayabilen metod kullanmak demek, kapsam bilinmiyor
demektir, ve fiyat kontrolü bu projeler için anlamsızdır. Bu tür
projelere fiyat kontrolü getirmek projeye acı sonuçlar getirebilir. En
sonda olacak depremden hem siz, hem de müşteriniz
etkilenecektir. Sonuçta müşterinizin bir şekilde bir yazılıma ihtiyacı
var ki bunu sizden istemiş. Bu yazılım eline geçmezse, şirket geleceği
kötü şekilde etkilenecektir. Yani yazılımı şirkete verdikleri para bir
yana, onlara lâzım olan programı alamamaktan ifade bulan bir zarar ile
karşı karşıya gelirler.  Demek ki iki taraf için de fiyat kontrollu
bir kontrakt imza etmek zararlı. Bu demektir ki, müşteri de, siz de
değişik bir ilişki halinde olacaksınız.  Çevik metodlar altında
müşterinizin yazılım projeniz üzerinde daha fazla etkisi olmalı. Her
sürüm sonunda, hem durum kontrolü için, hem de projenin yonunu
değiştirebilmek için katilmalari gerekiyor. Böylece yazılım takımınız
ile müşteri arasında daha sıkı bir ilişki doğuyor, tam bir takım
oluyorsunuz yani. Kabul edilyoruz: böyle bir yazılım geliştirme
yöntemi her müşteri ve her yazılım şirketi için uygun
olmayabilir. Fakat böyle olması uyum sağlayabilen metodlar için
şarttır.

Böyle bir sistemin müşteriniz için en önemli yararı şudur: Az
özellikli olsa bile kullanilabilir bir yazılım, kullanima cok önceden
açılabiliyor. Ve yeri geldikçe, ve iş düzeni değiştikçe, kendileri de
ne istediklerini daha iyi anladıkça, sistemi değiştirme şansları
ellerine geçiyor.  İnsanların, Körü Körüne Metoddan Önce Gelmesi Uyum
sağlayabilen bir metodu kullanmak kolay değil. öncelikle cok kaliteli
programcılara ihtiyaç var. Hem kişilerin kaliteli olmasi lazım, hem de
yazılım takımının birbiri ile uyum halinde olması lazım. Ve ilginç bir
gözlem yapmak gerekirse, uyum sağlayan metod için iyi programcı
gerekir, iyi programcılar da, aslında bu tür metodlar ile en rahat
çalışırlar.  Programcılar Lego Parçası mi?  Klasik metodlarin asıl
istedikleri lego parçası gibi istenen yere konabilen programcı
birimleridir. Böyle metodlara göre lazım olan, değişik tipte ve
özellikte programcı "parçalarıdır". Bir tarafta analizciler, öteki
tarafta testçiler, başka bir tarafta idareciler. İnsanlar, kişi olarak
önemli değiller, ne rol üstlendikleri önemli yani. Yani, projeye hangi
"kişi" aldığın önemli değil, belli rollerde belli sayıda "parçalar"
aldığın önemli.  Fakat gerçek hayat hakikaten böyle mi? Çevik
metodların getirdiği en büyük değişiklik iste burada.

Bu tür fikirleri bir tarafa atmamız gerekecek.  Bu gözlemi en iyi
ispatlayan bence Alistair Cockburn. Bu konu hakkındaki bir yazısında,
klasik, başı-sonu belli metodların, sıkıcı ve ne yapacağı belli
insanlar gerektirdiğinden bahsediyor. Fakat insanlar tahmin edilir ve
planlabilir birimler değil. Cockburn'un yaptığı araştırmalara göre,
vardığı sonuç, insanların yazılım projesinin en önemli parçaları
olduğudur.  Bu arkadaşımızdan bir paragraf: "Biraz önceki yazımın
başlığında, klasik metodlara atıf yaparak, insanlar lego parçası gibi
kullanılıyor demiştim. Eski metodlar insanları nedense böyle
görüyor. Fakat ötekilerin göremediği, insanların çok değişken, ve
başarı-başarısızlık dalgalarının bile çok değişik olduğudur. Bu
etkenler o kadar birinci derece de önem taşıyor ve ayrıca planlanamaz
türden oluyorlar ki, bu tür değişkenleri hesaba katmayan metod
mucidlerimiz, projeler metodlarını kullanınca hüsrana uğruyorlar, ve
bu sonuçlar başa gelince nedense şaşırıyorlar".  Bu tür "insan
öncelikli" yaklaşımlarda Cockburn arkadaşımız bayağı uç noktada olsa
bile, tanıdığımız proje lideri arkadaşlarımız da benzeri gozlemleri
bizle çok paylaştılar.

Klasik metodun en eksik tarafı, bu tür gözlemleri nedense
dinlememesidir.  Bu eğilimin işaretlerini de görmüyor değildiniz
herhalde. Mesela bütün programcılariniz lego parçası gibi gördüyseniz,
onları kişi olarak görmüyorsunuz demektir. Bunun yan etkisi takım
moralini düşürür. Kaliteli olan programcılar buna dayanamayıp, mutlaka
daha iyi olarak gördükleri yere gitmeye başlarlar, ve sizde
parça-yedek parça fikirlerinizle yanlız kalırsınız.  Evet, aksine
karar verip uygulamak çok büyük bir adımdır, uygulamaya koymak büyük
kararlılık ister. İnsanların işlemci parçası olarak görülmesi, çok
eskiden beri bilinen işletmeci kafasına dayanır, Fredrick Taylor
mesela "Bilimsel İdare" sanatında böyle bir yaklaşımdan
bahsetmiştir.

Eğer fabrika isletiyorsanız, Taylor'ın fikirleri işinize
yarayacaktır. Fakat yaratıcı bir olaydan bahsediyorsak, o zaman
Taylor'ın fikirleri bırakalım. (Modern üretici tesisleri de aslında
Taylor fikirlerini bırakmaya başladı). Yazılım işi yaratıcı bir
olgudur.  Kendinden Sorumlu Programcılar Taylor fikrinin merkez
noktası sudur: "Herhangi bir isi yapan kişi, o işin nasıl en iyi
şekilde yapılacağını bilemez". Bir fabrikada bu doğru
olabilir. İşçiler bir makina gibi kullanıldığı için çok zeki olmaları
gerekmez.  Yazılım dünyasında bunun ne kadar yanlış olduğunu
görüyoruz. Gün geçtikçe yazılım dünyasının parlak ışıklarından,
oturduğun yerden büyük şeyler yaratabilme arzusu, daha çok akıllı ve
becerikli insanları bu sektöre çekiyor. Ve hisse senedi hakları,
programcıları iyice şirketlerine bağlıyor, kader birliği getiriyor.
İşte bu şekildeki bir gruptan insanları işe alabilmek, ve aldıktan
sonra tutabilmek istiyorsanız, bu insanların profosyonel düşünceli
işini seven insanlar olduğunu kabul etmeniz lazım. O yüzden teknik
sorunları çözerken, unutmayın ki probleme en iyi çözüm getirecek olan
insanlar, bu teknik arkadaşlarınızdır. Taylor modelinin işlemesi için,
ayrı olan planlama bölümünün, işçilerin ne yaptığını onlardan iyi
anlaması gerekir. Eğer işi yapan zeki ve becerikli insanlar ise, bu
düzen ise yaramayacaktır yani.Martin Fowler





