# JSTL Yaratıcısı İle Mülakat

(JSL yaratıcısı Shawn Bayer ile mülâkattan alınmıştır).

Bize kendinizden ve şu anda uğraşmakta olduğunuz projeden
bahsedermisiniz?  Yale üniversitesinde araştırmacı programcı olarak
çalışıyorum. Bir şekilde Apaçe Yazılım Vakfı ve Java Birlik Süreci'ne
dahil oldum. Şu anda JSP standart etiketleri (JSTL) teknolojisinin
referans yazılım lideriyim. Bu yazılımı Java Birlik Süreci altında
gerçekleştiriyoruz. Manning kitabevi altında 2 kitap yazdım; biri Web
Development with JavaServer Pages, öteki JSP Standard Tag Library adlı
kitap, yani JSTL.

JSTL Nedir?

JSTL, standart etiket kütüphanesidir. Bildiğiniz gibi, JSP altında
anlık değişen türden içerik konabiliyor. Mesela programcık (scriplets)
kullanabilirsiniz, ama herkesin bildiği gibi programcık ile sayfa
yazdığımız zaman, içeriği işlem mantığından ayırmak zor
oluyor. Sayfaya bakıyoruz, bir metin, bir Java kodu, tekrar HTML,
tekrar Java, yani tam bir keşmekeş. Alternatif yöntem etiket
kütüphaneleri, yani ihtiyaca özel (custom) etiketler idi. Etiket
kütüphaneleri uzun süredir aramızdalar, değişik şirketler değişik
kütüphaneler çıkardılar, Apaçe'den de bazı kütüphaneler geldi. JSTL
atılımı bu bütün dağınık olan kütüphanelerin ortak bir temsilini
çıkartıp standart bir etiket dilini ortaya koymak idi. Böylece, mesela
içerik yazan görsel tasarımcılar tek bir etiket dili öğrenebilirler,
ve her J2EE uyumlu servis ortamında kullanabilirler. Dil standart
olunca, standarta uyan Uygulama Servisleri arasında bir rekabet
çıkabilir, bunun sonucunda daha etkin yazılımlar ortaya çıkacaktır. Ya
da, benim yazdığım gibi genel etiket konularında artık bir kitap
yazılabilir, çünkü üzerinde mutabakat kurulmuş olan tek bir dil
vardır.

JSTL JSR (Java Tarifname İsteği) ile Apaçe arasındaki ilişkiyi
anlatabilir misiniz?

JSR süreci Java Birlik Süreci altındadır. Bütün öteki JSR'lar gibi
(mesela J2ME, J2EE) birlik altındaki şirketler, uzmanlardan aldığı
yorumlar ile ilerler. Benim yönettiğim Referans Yazılımı, Apaçe
altında sürmekte. Yani JSTL tarifnamesinin 'gerçekleştirimi' Apaçe
altında yapılıyor, aynen JSP/Servlet tarifnamesinin kodlamasının Apaçe
altında Tomcat ile yapıldığı gibi. Yani ortada herkesin kodlama
yapabileceği bir standart var, biz bu kodlamalardan sadece
biriyiz. Ama referans olarak ilkiyiz. Standart kavramının güzel tarafı
burada: Yazılımın gerçekleştirme safhası değişik şirketler tarafından
yapılabileceği için, rekabet sayesinde daha çok iyi ürünler ortaya
çıkabiliyor.  Apaçe'nin Sun ile özel bir ilişkisi var gibi
gözüküyor...

Bu ilişkiden biraz daha bahsedebilir misiniz?

Evet, hakikaten prensip olarak düşünürseniz Sun'ın JSTL için niye IBM
ya da BEA gibi büyük 'ticari' şirketleri seçmediği biraz garip
gelebilir. Fakat Apaçe'nin şimdiye kadar gerçekleştirmiş olduğu
serbest yazılımlara bakarsanız, Apaçe Web Sunucusu ya da Tomcat gibi
büyük, ve çok başarılı olmuş olan projeler görürsünüz, normal olarak
bu yazılımlar etrafında büyük bir kullanıcı gurubu oluşuyor. Böyle bir
kullanıcı gurubu, JSTL için uygun bir ortamdı zannediyorum. Ben bu
kararın yapıldığı seviyede olmadığı için sadece tahmin edebiliyorum,
fakat Apaçe etrafındaki kullanıcı gurubuna bir örnek vermem gerekirse;
Şahsen Apaçe etiket kütüphanesi daha beta seviyesinde iken, etrafında
birçok kullanıcı vardı. Ben de bu ilk kullanıcıların sorularını
İnternet üzerinde cevaplıyordum, vs.. Tabii bu ileride yazacağım kitap
için birçok soru/cevap topladım, yani böyle bir şeyi yapabilmem bile
Apaçe etrafında nasıl büyük bir topluluk olduğunu gösteriyor.

JSTL amacı nedir, ve hangi problemleri çözmek için ortaya çıkmıştır?

Ee evet, daha önce belirttiğim gibi JSTL etiketlere bir standart
getirmek görevini güdüyor, yani oluşum olarak şu an JSP teknolojisi
ile alâkadar olan şirketlerin ve kullanıcıların ortak mutabakatını
temsil ediyor. JSTL'in hedef gurubu sayfa içerik yaratıcısı, arka plan
Java kodlayıcısi değil. İçerik yaratıcısı derken, belki de program
yazmayı bilmeyen, ya da sayfa yaratırken program yazması gerekmemesi
gereken kişilerden bahsediyorum. Ayrıca içerik yaratıcısı derken bir
projede oynanan bir rolden bahsediyorum, özel bir kişiden değil. Genel
hatları ile belirtmek gerekirse, JSTL, JSP'yi yepyeni bir guruba
tanıştırıyor. Kitabımda örneklerini tekrar tekrar gösterdiğim gibi,
aslında 'tamamen' JSTL kullanarak bütün bir uygulamayı baştan sona
yazabilirsiniz. Tabii büyük bir yazılım ile uğraşıyorsanız bu kod
bakımı açısından iyi bir mimari seçim olmayabilir, herhalde bu şekilde
yazılım mimarları JSP içinde fazla işlem mantığı istemezler. Fakat
teknolojinin bunu mümkün kılacak silahları 'sağladığını' belirtmek
için bunu söyledim. Mesela kitabımda örnek olarak verdiğim bir portal
uygulaması var. Ayrıca bir ileti sistemi, takvim, gibi örnekler
var. JSTL çok esnek bir teknoloji, ve programcı olmayanlar için de
yararlı. Programcı olanlar için bile yararlı bazı soyut temsiller
bulunuyor. Hala arada sırada, bana içinde çalıştığım üniversiteden
sayfa yazmam için istekler geliyor, o yüzden JSTL içindeki bu
özellikler benim çok işime yarıyor.

JSTL'in ilginç bazı özellikleri nelerdir?  Tipik bir JSP uygulaması
için lazım olan sorunların cevabı JSTL'de bulunabilir. Mesela, şartsal
mantık için etiketler var. Ya da bir liste üzerinde döngü ile
elemanları ziyaret edebilen sözdizim mevcut. JSTL, Java Collection
arayüzünün sağladığı her nesne ile konuşup, elemanlarını ziyaret
edebilir. JSTL'i metin formatlama için, ya da milletlerarası
kişiselleştirme için kullanabilirsiniz. Veri tabanına direk erişebilen
etiketler bile var; tabii bu özellik bazı ortamlarda tepki yarattı
(sayfanın veri tabanına erişmesi kötü olarak bilindiğinden) fakat ufak
ve orta ölçekli mimarilerde, ya da, içerik için gereken bazı
bilgilerin bazen veri tabanında olması sebebiyle bu özelliğin olması
bana göre iyi oldu. Mesela kullanıcının sayfasını kişiselleştirmesi
sonucunda, ne bileyim, renk tercihi mesela veri tabanında olabilir. Bu
gibi bir bilgiyi almak için bazıları koskoca bir katmandan geçmeden bu
bilgiyi almayı tercih edebilirler.  Ayrıca JSTL, XML işlememiz için
bile bazı etiketler içeriyor. Bu açıdan XSLT'ye rakip bile
sayılabilir. Tabii JSTL, XSLT ile birbirini destekler halde de
çalışabiliyor, mesela JSTL sayfası içinden XSLT değişimlerini
tetiklemek gibi. Fakat buna ek olarak, XML içinden aranan metni çekip
çıkartabilecek ve içerik üzerinde kullanabilecek JSTL etiketleri
var. Böyle olunca, JSTL ile bir şablon oluşturuyorsunuz, sonra işte
şuraya XML'den gelen bilgi döngü ile açılarak yazılabilir
diyebiliyorsunuz, vs. JSTL üzerinde bütün bunları yapmamızı sağlayan
bir deyim dili var. Kıyasla, JSP'yi programcı olmayanların kullanması
çok zordu. Birazcık Java bilmeyenlerin JSP kullanması imkansıza
yakındı. Mesela, oturum sınırı nedir, ya da istek nesnesi üzerinden
bilgiyi nasıl alabilirim gibi.. JSTL deyim dili ile, Java bilmeden,
sayfaya gelen bilgiye erişmeniz mümkün olabiliyor. İstek nesnesine
erişmek için basit bir sözdizim yaterli oluyor. Deyim dili bütün
etiketleri birarada tutan tutkaldır: Mesela bir etiket ile bir
numarayı tarayıp, sonucu sınırsal değişken üzerine koyabilirsiniz,
sonra başka bir yerde bu değere erişip başka bir işlem yapabilirsiniz.

XSLT'nin problemleri sizce nedir?

Şimdi belirteceğim görüş herkes tarafından ne kadar benimseniyor
bilmiyorum ama, hafiften benim 'ufak' bir isyan çığlığım haline
dönüştü. Çoğu kullanıcı için XSLT'nin kabul edilmez durumda. Zaten
genelde işlevsel dilleri sevdiğimi söyleyemem, (XSLT bir işlevsel
dildir). Akademik ortamda olmam yüzünden bu garip gelebilir, çünkü
LISP ya da Scheme yerine gördüğünüz gibi Java'yı tercih ediyorum. Ama
Java'nın başarısını bakacak olursak öteki insanlarda bu tercihi
paylaşıyor demektir. Eğer bilgisayar dillerinin mimarisini
biliyorsanız, şöyle bir tanım yapılabilir. Java ve JSP mecburi
işlemsel bir dildir. "Şu, şurada, şu şekilde olacak" gibi tanımlar
yapar. Işlevsel diller der ki "şu ve şu gibi kalıplar için, şöyle
davran". Fakat problem şurada: İçerik üreten kullanıcılara bu
kavramlar çok yabancı geliyor. Ayrıca, döngüler, koşulsal ibareler
kullanamamak veriyi göstermemizi zorlaştırıyor.

Deyim dilinden biraz bahsedebilir misiniz?

Deyim dili, Java kullanmadan veriye ulaşmanın bir yoludur. İçerik
üreticisi, Java'dan daha basit olan bu dili kullanarak sayfa
yaratabiliyor. Javascript'ten ve alternatiflerinden bile daha
basit. Her ortamın değişik bir deyim dili gerektirebileceğinden
hareketle, bize bu deyim dilini nasıl seçtiğimizi soruyorlar. Bu
soruya cevap, JSP'nin zaten kullanıldığı ortama en çok lazım olan
deyim dilini bulduğumuz olacaktır. Yani, şu anda veriyi içerik olarak
göstermesi gereken, ama Java programlamayı bilmesi gerekmeyen
kimseler... Her lazım olan şey için yeni etiket deyim diline
eklemedik, yeterli derecede bir alt sözdizim ile yetindik. Daha
fazlası için zaten Servlet/JavaBean üzerinde birçok şey yapmak
mümkün. Ya da özel etiket yazılabilir. Burada anahtar, mesela
başkalarının yazdığı sayfaya bakım yapmak için çağırılabileceğiniz,
böyle bir durumda herhangi bir dilin %20'sini bilmeniz
yetmezdi. JSTL'in tamamı, bazı dillerin %20'sinden azdır. Mesela,
oturum üzerinde olan kullanıcı nesnesinin adres değerine şöyle
erişebilirsiniz: session.kullanici.adres ve etrafına $ işareti
koyarsınız olur biter. Java ile session.getAttribute diyecektik, vs,
tabii yanlış yapma ihtimali var, ve bir Java hatası olsa, içerikçinin
hatayı düzeltecek Java'sı yok.

Ama sonuçta bir dili, başkası ile değiştirmiyor musunuz? Fark nerede?

Sonuçta bütün diller ortak kavramları paylaşır. Fakat deyim dillerinin
avantajı, işlem çağırma, Java tipleri, istisna hallerini bilmemize
gerek bırakmaması. Örnek olarak, bütün istek (request) bildirgeçleri
JSTL sayfasına String (metin) olarak gelir, eğer bu bilginin üst
sınırlarını falan gösterecek olsak, bir sayı gerekir, vs, bütün bu
dönüşümleri JSTL otomatik olarak yapar.

Bir şirket yazılımı içinde JSTL, genel mimari içinde nasıl yeralır?

JSTL, Java Collection arayüzlerini kullanmak için yazıldığı için, arka
planda ne olduğu farketmez. Arkada Servlet, Bean'ler, ya da EJB
olabilir, JSTL için bunlar önemli değildir. Model 2 MVC'ye bile JSTL
rahatça uyabilir; Struts yakında JSTL'i içine dahil edecek. Herhangi
ölçekte bir uygulama için JSTL kullanılabilir. Fakat belirteyim:
JSTL'in özellikle küçük ve orta ölçek için kabul edilir olması için de
özen gösterdik, bence Web uygulamalarında kronik gördüğüm bir sorun,
üzerlerinde 'aşırı mühendislik' yapılmış olması.. Yani basit şeyler
için muazzam çetrefilli tasarımların getirilmiş olması..

J2EE projelerinin en çok yaptığı hata nedir?

Benim özel görüşüm aşırı mühendislik büyük bir sorun. Çoğu internet ve
şirket yazılımları, insanların anlaması için çok zor bir halde, ve bu
büyük bir problem. Aşırı mühendisliğin en bariz kaynağı, vaktinden
önce yapılmış eniyileştirme, yani, daha bir problem ortaya bile
çıkmamışken o probleme göre tasarımlar yapmış olmak. Nesne havuzları
(object pool) harika bir örnek. Her nasılsa, herhalde çok kötü olan
örneklere bakarak, insanlar çok fazla nesne yaratıldığını görmüşler ve
güya çöp toplama (garbage collecting) dayanılmaz bir problem. Ve bu
sözde soruna çözüm olarak öteki metodlara bakmışlar, mesela veri
tabanı bağlantısının havuzlanması gibi. Fakat bu öteki alanlardaki
özkaynak gerçekten değerli, yani, veri tabanının az bir sayıda olan
tavan bâglantı sayısı var canım! Bu nesne havuzcuları, gittiler
nesneleri havuzladılar, bilmiyorlardı ki Sun'ın anında derleme
teknolojisi yeni nesne yaratmakta çok hızlı. Sonuçta ne oldu, elimizde
'olmayan bir sorun' için eniyileştirilmiş bir kod var. Belki normalden
'azıcık' daha iyi performans gösteriyor, ama işte o kadar. O arada,
kaynak kodun anlaşılması muazzam zorlaşmış, ve tabii ki bakımı bir o
kadar zorlaşmış.

JSTL bir proje içinde işbölümü için nasıl faydalar getiriyor?

Genelde etiket kütüpleri, ve özellikle JSTL'in getirdiği fayda soyut
kavramlarla oynayabilmektir. JSTL'in programcıklara olan üstünlüğü
budur. Tabii bizim sektörün en önemli özelliği, soyutluk seviyesini
kaldırarak işimizi azaltmak! (İdareciler de işi başkasının üzerine
yıkarlar :) ). Sonuçta JSTL detayları, bu detayları görmesi gerekmeyen
içerikçilerden saklayarak işlerini kolaylaştırır. Soyutlamanın ana
fikri budur. Böylece proje içinde bazıları iş mantığı yazabilir, başka
birisi bu mantığı nesne modeli üzerine koyabilir, ve bu model
üzerinden gerekli bilgiyi Iterator nesnesi ile JSTL sayfasına afişe
edebilir.

![](BayernBig.jpg)

