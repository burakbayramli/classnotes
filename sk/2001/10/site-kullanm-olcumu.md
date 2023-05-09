# Site Kullanım Ölçümü

Bir e-ticaret ya da içerik sağlayan bir site kullanıma açıldıktan
belli bir süre sonra kullanıcılarınızın yeni ürünlere, yeni yazılara
nasıl tepki verdiğini ölçmek isteyeceksiniz. İnternet'in en büyük
avantajlarından biri, bu ölçümü yapma yöntemi, aynen yaptığınız
yayınlar gibi Java kodu ile hallolacak!  Tabii Apaçe gibi internet
sunucu programları her sayfaya erişim olduğunda dosya kütüğü üzerine
bir satır yazabilirler. Erişilen sayfa ismi, isteğin nereden geldiği,
çerez içeriği, vs. gibi şeyler olabilir. Fakat bu tür internet sunucu
bazlı tutulan kütükler, istediğiniz kadar zengin bilgi
içermecektir.

Bunun sebeplerine gelelim.  Artık İnternet yayıncılığı ve uygulama
sunucuları oldukça ilerlediği için, değişken içerikli türden sayfalar,
"bir tek sayfa" kodu kullanarak "birçok" sayfa içeriğini
gösterebiliyor. Mesela sitemizden örnek verirsek, article.jsp sayfası
her yazıya tekabül eden XML'den üretilen HTML'i dinamik olarak bulup,
otomatik olarak cevap nesnesine diğer içerik ile beraber
birleştirip/ekleyip tarayıcınıza gönderiyor.  Fakat internet sunucusu
takip için dosya isminden başka bir şey anlamadıgı için kütüğunde
görünen erişim izi sürekli article.jsp oluyor! Fakat article.jsp
sayfasını kullanarak 10 çeşit yazı servis etmiştik. İnternet sunucu
programı ne yazıkki bu türden önemli bir detayı kaçırdı.  Çözüm olarak
erişim izlemeyi, içerik kodlamasının yapıldığı aynı yere koyacağız,
JSP sayfasının içine.  Tasarım İz bırakma işlemini iki şekilde
yapabiliriz. Ya veri tabanına, ya da dosyaya yazacağız. Dosyaya
yazmanın bir zararı yok.

Fakat, uygulama sunucusundan (Tomcat gibi) çıkan daha zengin kullanıcı
izleme bilgisini, sonradan sorgulama yöntemi kullanarak en rahat veri
tabanında analiz edebilirsiniz.  Fakat bir uyarı vermeden
geçemeyeceğim. Eğer, mesela her sayfaya erişimde veri tabanına bir
satır yazacaksak, bu işlem sayfa yüklenmesini yavaşlatacak. Çünkü
tecrübeye dayanarak söylemek gerekir ki, bilgi işlem dünyasında veri
tabanına erişmek "pahalı" bir işlem diye bilinir, bir sorgu işletip
cevabını almak öteki işlemlere oranla her zaman daha yavaştır.  Yani,
şöyle düşünün; sitenizi ölçeklemek için 2 tane daha sunucu eklediniz,
sayfaları değişik bilgisayarlara paylaştırdınız, umudunuz site
trafiğini bölerek her bilgisayara düşen yükü biraz olsun
hafifletmek. Fakat eğer her sayfaya erişimi içinde veri taban erişimi
var ise, bütün paylaştırma bir işe yaramadı, çünkü hala tek merkezi
noktaya odaklanmış muazzam bir yük sözkonusu, yani veri tabanı
üzerinde.  Tabii veri tabını ölçekleyerek, bazı bilgileri önbelleğe
alarak tekrar hızlandırma yapmanız mümkün. Fakat, bahsettiğimiz türden
kullanıcı izleme kodları, bir çizelge üzerine sürekli EKLE (INSERT)
işlemi yapıyor olacak. Bu türden bir işlem sitenizin diğer dinamik
kısımlarının gerektirdiğinden ayrı bir SQL kodlaması ve ölçekleme
planı gerektirebilir. Aklınızda olsun.

Bu işe en rahat çözüm, aslında, JSP ile EKLE sorgusu arasına bir ileti
kuyruğu koymak. Mesela Java JMS standard arayüzünü kullanarak, bir
ileti kuyruğu üzerine izleme bilgisini sürekli olarak atabiliriz. JMS
ortamı salla-gitsin (fire and forget) tipi çağırım kullandığından,
sayfa yüklenmesi veri tabanına bağlanmamış olur. Bir taraf kuyruğa
ekler, öteki tarafta kuyruktan bilgiyi boşaltıp veri tabanına
yazabiliriz.  Bu da aklınızda bulunsun.  Hangi Bilgileri Kütükleyelim
Sitenizin hangi kullanımını ölçmek istiyorsanız, o bilgiyi izleyin ve
kaydedin. Mesela, bir elektronik gazete için hangi makalenin, saat
kaçta, hangi makaleden sonra okunduğu önemli bir bilgi olabilir. Veri
tasarımı olarak şöyle yapabiliriz.

```
public static final String YAZI_OKUNDU = "1" ;public static final
String KATEGORI_LISTE = "2" ;protected String olcumTipi;protected
String oturumNo;protected String yaziIsim;protected Date
zaman;protected String kategoriIsim;protected String
referans;protected String uzakMakinaIp;protected String
uzakMakinaIsim;..
```

Veri yapısıni Java nesnesi olarak gösterdik, çünkü sitemizde JDO
kullandığımız için .class dosyasından geri-işlem yaparak veri tabanı
çizelgesi yaratmamız mümkün oluyor. EKLE, SEÇ gibi veri taban
işlemlerini JDO otomatik olarak çıkartabiliyor, programcı sadece bir
Java bean verip, üzerinde JDO komut satırından bir program işletip
SQL, JDBC kodlarını .class dosyasına eklettirebiliyor. JDO ile veri
taban bağlaşımı yapmak çok rahat.  İzleme konusuna dönelim: Bean'imiz
birden fazla izleme 'çeşidini' temsil edebilecek. Yani, kategori
listesine tıklamak ile yazı detayına tıklamak aynı bean üzerinde
olacak, bu yüzden mesela yazı izlerken kategori kolon değerleri boş
kalabilir. Bu o kadar önemli değil.  Java tasarımı olarak, JSP
sayfalarının içinde ikidebir bean'ler ile uğraşmamak için, ikinci bir
arayüz yaratıp, bu (daha geniş) arayüze bean yaratma, yazma gibi
görevleri verecebiliriz. Bu daha geniş arayüz şöyle olabilir.

```
public static void sayfaZiyaretKaydet(String oturumNo,String
yaziIsim,String referans,String uzakMakinaIp){Olcum olc = new
Olcum();olc.setOturumNo(oturumNo);olc.setYaziIsim(yaziIsim);olc.setZaman(new
Date());....  ..
```

ve JSP içinde kodu şöyle olacak.

```
<%OlcumKapisi.sayfaZiyaretKaydet(request.getSession().getId(), file,
request.getHeader("Referrer"), request.getRemoteAddr());%>
```

Analiz Bu yazıda, izleme bilgisinin nasıl toplanacağını
işledik. Alınan bilginin işlenmesi çok daha geniş bir konu. Sık
erişilen bir siteden çıkacak bilgilerin hacmi tartışılmaz. Bu
bilgileri analiz etmek için istatistiki, olasılıksal yöntemler
kullanılıyor, hatta yapay zeka yaklaşımlarını kullananlar bile
var. Veri madenciliği denen, "veriden bilgi" çıkartmak başlı başına
ayrı bir dal, bu konularda ileride değişik yazılarımız olacak. Bu
konularda lazım olan Java bilgisi değil, SQL, bir metin-yazar ve bol
bol zaman olacak.





