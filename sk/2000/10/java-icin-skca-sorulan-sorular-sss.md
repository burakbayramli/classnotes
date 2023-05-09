# Java İçin Sıkça Sorulan Sorular (SSS)

Java ile yazilan uygulamalar yavas mi?

Günümüzde ünlü olan derleyiciler, ya makina kodu çikartirlar, ya da
arakod (bytekod) denilen ve yorumlayici gerektiren isler kod
yaratirlar. Eger isler kod makina koduysa (mesela .exe dosyalari), bu
dosyayi hemen isletmek mümkündür. Java ise arakod yarattigi için,
isler kod için yorumlayici lazimdir. Java programlarini isletmek için
komut satirindan "java dosya_ismi" tabirinin kullanilmasinin sebebi
budur.  Yorumlayici, adindan da belli oldugu üzere, her arakodu teker
teker alip makina koduna program islerken 'anlik' çeviri yapar. Böyle
olunca, mikroislemci hizinin belli bir yüzdesi bu çeviri islemine
gider.  Fakat, "Java daha yavas mi" sorusu, ortama göre degisik
cevaplar getirebilir. Mesela, is sistemleri denen, veri tabanina sik
sekilde erisen sistemlerde Java kullanilirsa, hiz farki pek
farkedilmeyecektir. Çünkü is sistemleri zaten, zamaninin %80 kadarini
veri tabani üzerinde geçirir. Geri kalan %20'nin (örnegin) %10'u az
bir rakam olacaktir.  Diger ortamlarda, mesela matematik formülü çözen
ve hesap yapan sistemlerde, C++ ile fark görülebilir. Yüzde
veremiyoruz, çünkü aradaki fark her gecen gün kapaniyor. Ayrica,
Java'cilar bazi "agir" islemleri yerli "native" olarak C dilinde
kodlayabiliyorlar. Java içinden C islemleri çagirmak JNI (Java Native
Interface) araciligiyla mümkündür.  Böyle olunca aradaki seçimi çok
açidan ele almak gerekir. Fakat rahatlikla söyleyebiliriz ki, is
sistemleri için hiz farki önemli bir faktör degildir.

CLASSPATH nedir? java.lang.NoClassDefNotFound hatasi neden geliyor?

Java programciliginda karsimiza çikan ilk hata belki de
budur. Sebebini anlamak için, Java'nin paket kavramini anlamak
gerekir.  Her Java sinifi bir paket içine konabilir (tavsiye edilen
yöntem de budur). Beraber çalisan siniflar genelde ayni pakete
konur. Örnegin /usr/local/work altinda bir sinif tanimlanmis
olsun. Paket tanimlamasi söyle olabilir: package
com.sirket.vs;...public class TestSinifi { ...} Derledikten sonra,
çikan TestSinifi.class dosyasi /usr/local/work/com/sirket/vs dizini
altina gitmelidir. (Ant bu dizini otomatik olarak yaratir). Yani Java
Paket düzeni ile, dizin düzeni arasinda direk baglanti vardir.
com.sirket.vs dizini com/sirket/vs dizinine denk gelir. Bu yüzden
TestSinifi'ni isletmek için /usr/local/work altina gidip java
com.sirket.vs.TestSinifi komutunu isletmek gerekir.  Yarattigimgz
TestSinifi sinifinin gerçek ismi ise com.sirket.vs.TestSinifi olarak
geçer.

En uygun veri taban yönetim yazilimi hangisi?

Son zamanin en gözde veri tabani tartismasiz Oracle. SQL diline en
erken gecen veri tabanlarindan biridir. Her ortamda bulunabilir
(Linux, Windows, Solaris, vs.). ABD finans merkezlerinde sikça
kullanilir (ayakta uzun sure kalmasi gereken sistemler için).  Orta ve
büyük ölçekli uygulamalar için Oracle uygundur. Daha büyük (devasa)
veri tabanlari için IBM DB2, NEC'den Terradata gibi yazilimlar vardir.
Bedava veri tabanlari arasinda MySQL uygun programlardan
birisidir. InnoDB eklemesi ile hizli veri tabanlarina yetisti diye
duyumlar aldik.

J2EE ile .NET karsilastirmasi yapabilir misiniz?  Hangisi daha iyidir?

Bu iki teknoloji arasindaki seçim, su üç noktada baglanir: * Hangi
ortama baglanmak istiyorsunuz, tek isletim sistemine mi, yoksa tek
programlama diline mi?  * Sistemin uzun süre ayakta kalmasi ne kadar
önemli?  * Sistemin idare edilebilmesi ne kadar önemli?  Eger
proglamla teknolojisi olarak COM, VB bilgi dagarcigi sirketinizde
varsa, .NET uygun olabilir. Eger Java'yi yeteri kadar güçlü bir
yazilim dili olarak görüyorsaniz, her ortamda programinizi
çalistirabilmek açisindan Java uygun olabilir. Biz, gelecegin degisik
donanim ortamlarinda ve degisik boyutlardaki bilgisayarlarda,
elektronik aletlerde olacagini düsünüyoruz. Cep telefonu, ufak
devreler, dizüstü ya da sunucu ortamlarinda Java'yi artik bulmak
mümkün.  Ayrica, .NET kodlari Windows'a göbekten bagli olduklari için,
kodlarin ayakta çökmeden kalabilmesi isletim sisteminin istikrarli
islemesine baglidir. Windows ailesi nisbeten yeni bir sistem oldugu
için, hala büyümekte ve hatalari onarilmaktadir. Bu yüzden hala kritik
sistemler denen, ayakta surekli kalmasi gereken ortamlarda tercih
edilmiyor. Bu ileride degisebilir ama simdilik durum budur.  Ayrica
sistemlerin bakimini ve idaresini yapan uzmanlar, tekrar edilen isleri
genelde betikler ile (script) tekrar tekrar isletebilmeyi tercih
ederler. Windows'un geçmisi görsel agirlikli oldugu için komut
satirina fazla agirlik verilmemistir. Sistem idarecileri o yüzden Perl
gibi Unix dunyasindan gelen kavramlari Windows'da kullanmak zorunda
kaliyorlar. Fakat gene de Windows'un her tarafi betiklemeye açik
degildir.

JSP-Tomcat-Java kullanirken, Türkçe karakter sorunu oluyor. Ne yapmak
lazim?

JSP ya da Servlet'in `javax.servlet.http.HttpServletResponse`
nesnesinin setContentType(java.lang.String type) metodunu da
kullanabilirsiniz: `request.setContentType("text/html;charset=ISO-8859-9");`

Daha güzel bir yöntem ayni nesnenin `setLocale()` metodunu kullanmaktir:
`request.setLocale(new java.util.Locale("tr", "TR"));`

Eger HTML (ve HTML içeren JSP) dosyalarinin kodunu degistirerek ayni
sonucu yaratmak isterseniz, baslarina su ibareyi eklemek gerekir:

`<head><meta content="text/html;charset=ISO-8859-9" http-equiv="Content-Type"></head>`

