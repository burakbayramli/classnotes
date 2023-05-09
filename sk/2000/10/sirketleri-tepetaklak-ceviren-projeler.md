# Şirketleri Tepetaklak Çeviren Projeler - İyi Anlamda

Yazılım hayatında değişik projelerle karşılaşıyor insan. Bu projelerin
bir kısmı hakikaten şirket düzenini tepetaklak çeviren türdendir
(iyiye doğru). Bazı projeler de o kadar etkili olmasa da, gene de
şirketin önemli bir bölümünü etkilerler. Bu türden yazılımlar, zaten
olan düzeni daha hızlı işletmek için yapılmış türden değildirler. Bu
yazılımlar şirkete verilince, makamları değiştiren, yeni görev
çeşitleri 'yaratan' projelerdir.  Ama bu tür projeler çok risk ile de
doludur. En güzel anında bile yazılım zor bir iştir; fakat hedef daha
önemli hale geldikçe, risk bir o kadar tavana vurur. Yetmezmiş gibi,
siz proje ile meşgulken iş planları değişebilir, bu da riskleri daha
da arttıracaktır. Sonuç olarak bu tür işyerlerini tepetaklak
çevirtebilecek projelerin kendine has püf noktaları vardır. Bu
noktaları burada paylaşmayı umuyoruz.  Benim şirketim ThoughtWorks, bu
türden bir dizi proje ile uğraştı.

Bu projelerin büyük çoğunlugu başarı ile bitti. (Bazıları da
bitmedi). İşte bu yazı altında, bu örneklerden bahsedeceğim, (şirket
isimlerini değiştirerek), ve genel konulara değinerek bazen yazılım
endüstrisi hakkında duyduğum öteki projelere referans ta yapacağım.
Proje Özetleri Aşağıda bahsettiğim bu projelerden bazılarını
özetledim. Projelerin kendisi hakkında kısa bir bilgi vereceğim,
böylece ne yapmaya uğraştığımızı anlayacaksınız. Aldığımız derslerden
ileriki bölümlerde bahsederken, bu projelerin aklınızda olması önemli,
o yüzden önceden bahsediyorum. Projelerden "kod ismi" ile
bahsedeceğiz. Ayrıca, şirket ismi için mesela Efes adı altındaki
proje, Efes Holding adlı şirket için yapılmış olacak.  Proje Efes Efes
Holding ünlü ve pahalı mal üreticilerinden biridir. Fakat son
zamanlarda kazançları, pahalı olan bu malları 'kiralamaktan'
geliyor. Kiralama bilindiği gibi eski ve bayağı karışık bir iş
modelidir. Herneyse, bu şirketin kiralama işleri büyüdükçe, bilgisayar
sistemlerinin de buna ayak uydurması gerekti. İşte biz tam burada
olaya dahil olduk. Bizim sistemizin yapması gereken, arka planda olan
kiralama sistemi için bir tür ön perde yaratmaktı.

Yani, kirayi imzalamadan yapılması gereken her şeyi bizim sistemin
yapması gerekiyor idi. Mesela kiracının kredi limitini kontrol etmek,
kira için bir fiyat belirlemek ve lazım olan dokümanları basmak. Biz
işe başlamadan önce bu işlemlerin hiç biri otomize edilmiş değildi,
yarı-otomize edilmişlerdi demek daha iyi olur.  Yazğımız program çoğu
teknoloji hayranının düşlerini tatmin edecek türden oldu: Butun
Enterprise Java parçalarını kullandık, EJB, JSP ve diğer üç harfli
kısaltmalar. Kullandığımız teknolojiden çok tabii ki, esas önemli olan
bu teknolojik çözümün Efes Holding üzerindeki etkileri oldu. Efes'in
bizim bu sistem sayesinde, değişik coğrafi yerlerde ofis açmasına
gerek kalmadı. Bütün ofisler tek bir merkezde toplandı. Bir kirâ
talebinden, kira imzasına kadar geçen vakit kısaldı: Bazen uygun bir
fiyat vermek 2 gün alıyordu. Şimdi bu iş 45 dakikada haloluyor. Efes
satış elemanları bu sistemi cok sevdi, çünkü İnternet'ten
erişilebildiği için her yerden ulaşıp, müşteriye anında fiyat
verebiliyorlardı.  Proje Tuborg Tuborg Holding, Amerika'nın büyük
satış mağaza zincirlerinden biri, hani her can sıkıcı alışveriş
merkezinde olan türden. Her satış mağazası gibi, satış zincirleri
(Supply Chain) bu şirket için çok önemli. Yapması gereken işi
düşünürseniz, anlamak zor değil, mallar belli satıcılardan,
müşterilerin eline gecmesi lazim.

Arada olan bütün olayları Holding'in idare etmesi gerekiyor. Veri
ambarları o kadar çetrefilli olmuş ki, idare edenlerin korkulu
ruyalarına giriyor. Her mal kodu sabit olsa da, bur sürü değişkeni
mevcut. (Mesela bir elbise tipinde değisik renk, ölçü olması gibi)
Tuborg projesinden tek bir proje gibi bahsedeceğim, ama aslında bu
proje 9 aylık seriler halinde 4 sene boyunca süren mini-projelerden
oluştu. Bu projelerinden birisi, Noel alışveriş zamanından once ambara
gelen onlarca malın, müşterilerin istedikleri mallara olan ilişkisini
araştırdı. Sonuç: Fazla bir iliski yok, demek ki yanlış mallar yanlış
miktarda ısmarlanmış. Eyvah...Bu sistem Forte teknolojisi
kullanılarak, çok kullanılan bir veri tabanı markası üzerine kuruldu.
Proje Pilsen Pilsen, mal taşınması için koli satisi yapıyor. Yanlız bu
koliler tip ya da şekil olarak çok değişik olabiliyor bazen, metaldan,
içinde portatif buzdolabı olana kadar, her çesit kolileri
var.

Taşıyıcı şirketler kolileri Pilsen Holding'ten kiralıyor genelde; bu
koliler dünyada oradan oraya biraz taşındıktan sonra, getirip geri
veriliyorlar. Bilgisayarların yapacağı önemli bir görev, mesela bir
kolinin nerede olduğu, ve ona en yakın geri verme istasyonu. Pilsen
Holding'in o zamanki sistemi 2000 yılına hazır değildi. Akıllıca bir
iş yaparak, eski sistemi düzeltmek yerine, yerine başka sistem
yazdırmayı seçtiler. Fakat birden bire bu proje Holding'i tepetaklak
çevirecek projelerden birine dönüştü. Sistem Forte üzerinde
yazılmıştı.

Proje Heiniken Heiniken projesi biraz proje Efes'e benzedi. İkisi de
büyük şirket, on perde sistemi, Enterprise Java sistemi ve üzerine
bizim eklediklerimiz, vs. (Hatta iki proje arasinda kod bile
paylastirdik). Farklar da var tabii. Mesela Heiniken Holding o kadar
teknolojinin uç tarafında mallar üretiyordu ki, bu malların bazıları
daha adamlar malı üretmeye başlarken, teknolojinin modası geçmiş
olabiliyordu. O yüzden bu malların stok durumlarını iyi idare etmek bu
şirket için çok önemliydi. Bu işi Heiniken Holding'in ERP sistemine
bağlantı kurarak hallettik. Efes'in iş tarihi, Heiniken'e göre daha
yeniydi, o yüzden fiyat belirleme işi daha önemliydi mesela, çünkü
satış elemanları eskiden aynı işi Excel üzerinden takip edilmesi zor
bir halde yapıyorlardı. Bizim yaptığımız sistem o yüzden Efes için
büyük gelişme idi.  Aldığımız Dersler Sık Sürüm Yap Geçmişte duyduğum,
gördüğüm projeleri şöyle bir kıyaslayınca, başarı ile biten ve
başarısız olan arasındaki en bariz fark, sürüm sıklığıdır. Yani ne
kadar sık sekilde sürüm yaparsaniz, o kadar iyidir. Örnek vereyim,
bazen şirket tepetaklak ceviren projede calışan arkadaşlar ile
karşılaşıyorum. Bana diyorlar ki "İşler çok iyi gidiyor, 5 senelik
projenin 2. senesindeyiz". Peki hiç programı kullanmaya actınız mı,
kısmen olarak yani? (sürüm yapmak buna denir). Cevap: Hayır. Vallahi
çok insanla karşılaştım hayat boyunca ama, projenin 5. senesinde bu
kadar heyecanlı olacak kimseyi tanımıyorum. Yani sonuç görmeden uzun
süre gitmek, biraz zor. O yüzden çok sürüm yapip, bir sonuç görmek çok
önemli.

Başarılı projelerde gördüğüm ortak bir nokta yani, sürekli yapilan, ve
hep "calışan halde" olan yazılım sürümleri. Zaman geçtikçe daha da
sıkı frekansla sürüm yapar olduk. Mesela en eski projemiz Tuborg için
her 6 yada 9 ayda bir sürüm yaptık. Her ne kadar büyük planımız "her
şeyi" değiştirmek olsa bile, büyük projeyi ufak parçalara böldük, ve
her proje belli bir noktaya odakladık. Bu projelerin her biri ve kendi
başına büyük yarar sağlayacak türdendi. Herkese o zaman anlattığımız
hikaye şoyledi: Bu projelenin sonuçları, 2 sene içinde kendi masrafını
karşılayacak. Efes projesi en yeni örneklerden biri, bu proje için her
2 ayda bir sürüm yapildi.  Sürüm frekansı niye bu kadar önemli? Güven
kurmak için. Holdingler büyük para koydukları işlerden, sonuç
beklerler. Mesela şirketi tepetaklak çevirecek projelerden biri için 5
senelik planı kabul ettirdiniz, ve hatta şirket içinde sampiyonluğunu
yapacak birini buldunuz.

Bu en iyi şartlar içinde bile, o sizin şampiyon adamınız şirket içinde
5 sene dayanmayabilir. Eğer o zamana kadar hiç sonuç göstermemişseniz,
ne yapacaksınız?  Ayrıca güven sağlamak ile beraber, inanın ya da
inanmayın, sık yapılan sürümler, projeyi zamanında bitirmenize
yardimcı olacaktır. Çünkü sürekli calışır halde olan,
gösterebileceğiniz, bir sistem oldugu için, kimsenin proje ne halde
diye merak etmesine, ve morali aşağı indirmesine gerek kalmaz. Sürümü
yapılmış yazılım, gozle gorulur, elle tutulur bir mesafe
göstergesidir. Tabii eğer proje gerektiginden daha yavas gidiyorsa,
sürekli calisan yazılım göstermek iyi bir sey olmayabilir! Fakat bu
durumda bile, tavsiyem yuksek frekansli sürümden
vazgecmemeniz. Sonucta proje gerceklerini saklamak zordur. Eninde
sonunda nasil olsa ortaya cikar. Fakat müşteriniz, yavas bile giden
projeyi "anında" izleyebiliyorlarsa, o zaman iptal konusunda daha
anlayışlı olacaklardır. Erken bile iptal etseler, bu sizin için daha
iyi olabilir.  Ayrica sık sürümler, öğrenmenize yardımcı olur. Bu
garip bir lâf gibi gelebilir, tecrübemiz ışığında belirtmem gerekirse,
bu tip tepetaklakçı projelerin başında hiç kimse sonuç vizyonunu tam
göremez. Eğer aynı yoldan gecmiş iseniz, ne geleceğini bilirsiniz,
tepetaklakçı projelerde her sey yenidir, hem sizin, hem de müşteriniz
için. Heiniken projesinde her ne kadar bildiğimiz teknolojiyi
kullanmış olsak bile, bu risk seviyesinde ufak değişiklikler bile
projeyi çok beklenmez hale getirebilir.

Bu sürprizler içinde yeni teknolojinin büyük payı vardır. (Niye böyle
olduğunu ilerde anlatacağım). Nedense tepetaklakçı projeler en
bilinmez teknolojiyi kullanırlar. Bu gibi durumlarda, projenin başlar
başlamaz teknoloji uzmanları belli püf noktaları hakkıinda "teknoloji
etütü" yaparlarsa çok iyi olur. Bu etütlerin bazıları başarı ile,
bazıları başarısızlık ile sonuçlanabilir, önemli degil. Sonuçta bu
hataların proje "başında" yapılmış olması daha önemlidir. Bu sayede
ileriye dönük planlarınızı daha bilgilenmiş halde yapabilirsiniz.
Yeni teknoloji sürprizinden kaçınmak isteyenler için: baskalarının
aynı yoldan geçmesini bekleyebilirsiniz. Fakat o zaman da güneş
batmış, şezlonglar kaldırılmış olabilir. Yani, zamanın gerisinde
kalmışsınız demektir.

Seçim sizin.  Sürüm frekansından bahsederken, müşterinize
gösterilebilir sürümden bahsettik buraya kadar. Fakat sizin yazılım
grubunuz, kendi içinde daha bile sık sürümler yapabilir. Mesela
haftada bir. Bunun yazılım entegrasyonu için faydaları büyüktur. Son
zamanlarda biz bu tür 'içsel' sürümün çok faydasini gördük, artık daha
çok yapar olduk.  Sürprizlere Hazır Olun Daha önce bahsettiğim gibi,
şirket değiştiren projeler, önceden kestirmesi zor olan
projelerdir. İş düzeni değişmesi demek, sizin yazılımınız için önceden
görülmeyecek sürprizler çıkaracaktır. Bu sürprizlerin kimisi iyi,
kimisi kötü olabilir.

Sadece doğru önlemler alarak, iyi olanların etkisini arttırıp, kötü
olanlarınkini azaltabilirsiniz.  Tuborg projesinde sürprizlerin iyisi
de, kötüsü de basımıza geldi. Proje başında, bizim beklentimize göre,
kolilerde yapılan tamirleri izlemek sistem için önemli değil
zannediyorduk. Proje ilerledikce, neredeyse işin yarısının tamirleri
izlemek olduğunu anladık. Yazılım buna göre değişeceği için, projenin
süresini ve bilâhere fiyatını artırmak gerekti. Bu durumda yapacagınız
tek şey, probleme açıkça bakmak, ve planları ona göre değiştirmektir.
Bunu yapmayanları şahsen gördüm; Proje sürprizinden sonra, yeni
öğrendikleri bilgiye aldırmadan, sanki eski plan hala geçerliymiş gibi
ona yapışanlar oldu. Bunu yapma sebepleri, plandan ayrılmayı
başarısizlık gibi gören bir düşünce yapısıdır. Esneklik
eksiğidir. Zaten, sürprizlere aldırmazsaniz, çığ gibi büyüyüp önünüze
gene geri geleceklerdir. Sonuçları projeniz için acı olur.  Güzel
sürprizler de olur. Mesela bizim proje müdürü Greg, Tuborg sistemini
Holding görevlilerine gösterirken, beraber bir farketmisler ki, ufak
bir SQL kodu ekliyerek, tamirlerin parasını ödemekte geç kalan
müşterilerin listesini dökmek mumkun. Bu veriyi vermek bizim sistem
için zor değildi, fakat Tuborg Holding için cok önemliymiş bu bilgi,
ve şirket zararını indirmek için çok işe yaradı.  Gene aynı proje
içinde, bunun daha bile büyük bir örneğini yaşadık.

Turborg holding için koli kiralama işi, şirketin sadece bir bölümü
idi. Yeni gelen bir yönetim kurulu, koli kiralamanın onlar için
masraflı olan bölümlerini baska bir şirkete satmaya karar verdi. Bizim
sistemiz sayesinde, kiralanan malların değişmesi, genel sistemi
etkilemedigi için, şirketin bir bölümünü satmak iş düzenini
aksatmadı. Hattâ ve hattâ, bu satılan bölüm için, bilgisayar desteğini
Tuborg, sattıklara şirkete para karşılığı verdi, böylece hem satıştan,
hem de satılan bölümun işletmesinden para kazanır hale geldi! Ne kadar
analiz, tasarım yapsanız bile bu değişikligi göremezdiniz, çünkü
bunlar iş düzeni değişimi idi.

Bütün bu sürprizlere bakarak, "o zaman plana ne gerek var"
diyebilirsiniz. Yani proje ortasında planlar değişmesi mümkün ise,
plan yapmaya ne gerek var? Aslında esas bu çok degisken projeler için
plan hala çok önemli, sadece değişik bir şekilde yaklasmak lazım. Bu
tip projeler içinde planlama, proje süresince bile devam eden bir olay
haline geliyor. Bu projeler için uzun süreli planlar biraz değişebilir
türden olacaktır, ama genede genel bir yön cizmek için yararlı
olurlar. Kısa süreli planler daha az değişecektir, değişmesi gerektigi
zaman da 'rahat' değişecektir'.  Efes projesinde bunun da örneğini
yaşadık. Yazılım takımının yarısı iki aylık proje parçasını düzenlemek
ve yazmak ile meşgul iken, öteki yarısı (analiz takımı) ondan sonraki
iki ay sonrası için ne yapılacağını araştırmak ile uğraştı. Efes proje
müdürüne bir gün sordum, "altı ay sonra ne yapacağınız belli
mi?". Müdür dedi ki 'vallahi elimde bir liste var, ama ne anlama
geldiklerini pek bilmiyoruz, listeyi bize veren Pilsen elemanlarının
bile anladiklarını söyleyemem".

Fakat bu proje bizim ve Efes Holding için bir başarı oldu.  Yuksek
Yerden Destek Alın Bu başlık sürpriz gelmese gerek. Eğer iş düzeninde
değişiklik gerekiyorsa, üst düzey yoneticileri projenizin arkasında
olmalı. Eğer yoksa, hiç bir seyin olmasıni beklemeyin. İnsanlar
değişmeyi genelde sevmez, bu direnişi aşmak için bol bol kendinden
emin liderlik gerekir.  Yüksek seviyeli yönetici projenize vizyon
sağlamak için de gerekecek. Bu vizyonun çok detaylı olması gerekmez,
zaten bu tip projelerin ne kadar değişken olduğundan
bahsetmiştik. Gereken sadece, yetkisi olan birinin projenize bazen bir
nevi rehberlik yapmasıdır. Mesela yapılması gereken işlerin arasında
öncelik saptamak, yazılımda hangi özelliklerin olacağına dair zor
kararlar almak için bu yönetici size çok lazım olacaktır. Özellik
olması/olmaması derken aklımıza geldi; Sık frekanslı sürümleri yapmak
için kararlı olmanız çok önemli. İçinizden sürüm zamanını biraz daha
geri atarak, birkaç özellik daha eklemek çekici gelebilir. Fakat bir
özellik ötekini izler, bir bakarsınız projeniz arkada kalmış, ve
müşteriniz karşısında kötü duruma düşmüşsünüz.

Eğer Efes projesinde, herhangi bir sürüm zamanı eğer bir özellik
katmazak, bu ozellik hemen takibindeki sürümde mutlaka olacaktır, iki
ay sonra.  Destek konusuna dönelim: Eğer destek almamışsanız, fakat
proje gene de bir şekilde işliyor ise ne yapacaksınız? çok üzerine
basarak söylüyorum, bu projeden vazgeçin. Bu biraz acı ve ağır bir
tavsiye gibi geliyor muhakkak. Hatta projenin müşteriniz için iyi
olduğuna dahi karar kılmışsanız, tavsiyemiz daha da düş kırıcı
gelecek. Fakat eğer üst düzey yetkiliden destek yoksa, şirketin
tepetaklak değişmesi hayal.  Hayatın acı gerçeklerinden biri daha:
Bazen yüksek seviye desteği olsa bile, o desteğin hep orada duracağı
garanti değil. Tuborg Holding'teki yöneticiler bizim projemiz
başladıktan sonra başka şirketlere gittiler, başarılı insanların daha
değişik işlere yönelmesi normal. Yeni gelen yönetici, özel yazılmış
programlardan cok, ERP programlarına ağırlık vererek cevaplar almayı
uygun gördü. Bu da normal, her yöneticinin değişik stili ve yaklaşımı
olabilir, işler doğru yürüyor hâlde olsa bile. Sonuç olarak bizim
Tuborg projemiz, planlanandan önce bitti.

Fakat sık sürüm ne kadar etkili bakın: Bütün projenin meyvalarını
toplamasak bile, en son yaptığımız sürüm, yakın zamanda ve
kullanılabilir halde olduğu için, o zamana kadar yazılmış program
kullanıldı ve Tuborg Holding'e kazanç sağladı.  Hafif Programcılık
Stili Bunlar bizim şirketimiz ThoughtWorks'un tecrübeleri. Tuborg ve
Pilsen projeleri bizi daha çok hafif programcilik stillerini
kullanmaya doğru itti. Bizim şirketimiz dışında, başka programcılık
şirketleri de aynı fikre gelmeye başladılar. Bu yazılımcıların bu tip
eğilimleri, son zamanlarda moda olan Hafif Programcılık Stiline yol
açtı.  Bu Hafif stiller ThoughWorks şirketini çok etkiledi. Heiniken
projesi özellikle bu eğilimlerden çok etkilendi, ve hatta sonuna kadar
takip etti. XP (Extreme Programming) denen stil, kod geliştirme
sırasında büyük miktarda test programı yazılmasını şart
kılıyor. Heiniken projesi için yazılan bu test programları, hem
kodumuzun kalitesini arttırdı, hem de, inanın ya da inanmayın,
programci takımının hızını arttırdı, çünkü artık daha az yazılım
yanlışı arar oldular.  ThoughtWorks şirketi ve şahsen ben de, su
sonuca vardım: Hafif Programcılık yöntemleri, bizim ve müşterilerizin
başarısı için şart.

Martin Fowler



