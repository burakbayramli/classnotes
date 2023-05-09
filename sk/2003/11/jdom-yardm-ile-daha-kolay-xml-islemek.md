# JDOM yardımı ile daha kolay XML işlemek

DOM, Java için yaratılmış bir araç olarak, XML işlenmesini çok
kolaylaştırıyor. JDOM tasarımı Java dilinin özelliklerinden
faydalanabilecek gibi yapılmış. Ama acaba öteki standart arayüzlerin
yerini alabilecek kadar etkili mi? Bu yazıda beraber göreceğiz.

Programcı olarak herhalde 80-20 kuralını duymuşsunuzdur (Pareto'nun
kuralı diye de bilinir). 80-20 kuralı derki "Metod (tasarım) düzenleri
önümüze çıkabilecek şartların 100'ünden 80'inine çözüm
getirebilir. Geri kalan 20 durum için metodun dışına çıkıp, o şarta
özel çözüm bulmak gerekir". Bu sözün programcılar için önemli dersi
şudur: Herhangi bir teknolojiyi ele aldığımızda, bu teknolojinin
önümüzdeki problemlerin yüzde seksenini muazzam rahatlıkta
çözebilmemize yardım etmesi gerekir.

Ne yazık ki yazılım ürünleri ve standartları bazen bu şartlara göre
gelişmiyor. Özellikle Java/XML teknolojilerine baktığımızda 80/20
kuralının birçok kere kırıldığı yerleri görüyoruz. Java dünyasında XML
arayüzleri ve ürünleri enflasyonu vardır; Kimi ürün büyük bir şirket
tarafından desteklenmiştir, kimisi tek bir XML işlemi etrafına özel
yazılmıştır, kimisi de bütün çözüme gayet çetrefilli çözümler
getirmeye uğraşmıştır. Fakat 10 koddan 8'ini çözecek, sürekli üst üste
yaptığımız işlerde yardımcı olacak kütüphane nerede? Hani o XML ağaç
yapısını basitçe işleyebileceğimiz, değiştirebileceğimiz Java diline
uygun bir yazılımdan bahsediyoruz.

JDOM işte tam bu sorunlara çözüm olarak yazılmıştır.

Java ve XML

Birçok yönden Java, XML işlemek için en uygun dil haline geldi. Apache
Vakfı ve IBM DeveloperWorks tarafından baslatılan büyük atılımlar
sayesinde şu anda Java için XML işleyebilen, değiştirebilen esnek kod
bazları mevcut.

Biraz da tarih. Java 2 standardı XML ünlü olmaya başlamadan önce
piyasaya çıktığı için, Java 2 dahilinde XML yönünden birçok eksik
var. Bu yüzden Sun, hem XML eklentileri yapabilmek hem de Sun dışında
yaratılmış olan teknolojilere 'evet' diyebilmek için JSR sürecini
kullanıyor. Bu sayede eklenen JAXP standardı akılda en çok kalan
yenilerden biri. JAXP dahilinde 3 paket eklendi.


*	org.w3c.dom: W3C standard arayüzlerinin Java'da gerçekleştirilmiş hali.

*	org.xml.sax: olaya dayalı (event driven) XML işleyici basit arayüz

*	java.xml.parsers

Bu paketlerin eklenmesi iyi bir şey olsa da, sonuçta yapılan 'zaten
revaçta olan' standardlara evet demektir. Hala eksik olan Java'nın dil
yapısına uygun esnek bir XML işleyici kütüphane idi.

Hoşgeldin JDOM. Ünlü iki Java yazarı olan Brett McLaughlin ve Jason
Hunter tarafından yazılan JDOM, 2000 yılında serbest kod lisansı ile
Apaçe projesine dahil edildi. Dünyanın her tarafındaki Java
kullanıcılarında aldığı tavsiyeler/hata onarımları ile büyüyerek
günümüze geldi. Amacı özetle, Java'ya uygun, basit ve esnek XML
işleyici kütüphanesini oluşturmaktır.

JDOM Dosyalarını Yaratmak ve Kullanmak

JDOM alıştığımız Java kodlama tekniklerini kullanıyor. Lazım olan her yerde Java new kelimesi kullanmak mümkün; çetrefilli Factory (Fabrika) nesnelerine gitmemize gerek yok. Mesela, sıfırdan bir XML dosyası yaratmak için ne yapmamız gerektiğine bakalım.

```
<?xml version="1.0" encoding="UTF-8"?>
<araba no="123fhg5869705iop90">
<!--Araba tarifi-->
<marka>Toyota</make>
<model>Celica</model>
<sene>1997</year>
<rank>yesil</renk>
<plaka il="34">HC-176</plaka>
</araba>
```

Hemen önceden bir kök (üst) elemanı yaratalım.

```
Element arabaEleman = new Element("araba");
Document benimBelge = new Document(arabaEleman);
```

Bu basamak yeni bir org.jdom yaratmış oldu, ve bu elemanı
org.jdom.Document benimBelge'nin üst düğümü haline getirdi. Her XML
dosyasına mutlaka bir tane kök gerektiği için kurucu işleme Element
nesnesini bildirgeç verildiğini görmüş olduk.

Şimdi no özelliğini ekleyeceğiz.

```
arabaEleman.addAttribute(new Attribute("no", "123fhg5869705iop90"));
``

Başka yeni elemanlar eklemek zaten basit. Marka elemanını ekleyelim:

```
Element marka = new Element("marka");
marka.addContent("Toyota");
arabaElemani.addContent(marka);
```

Not geçelim: Element nesnesinin işlemi addContent geriye Element
döndürdüğü için, aslında aynı satırı şöyle de yazabilirdik:

```
arabaEleman.addContent(new Element("marka").addContent("Toyota"));
```

..dersek aynı işi görmüş oluyoruz. Bazıları 'ilk satır daha okunaklı'
diyebilir, fakat aynı anda birden fazla Element yaratıyorsanız ikinci
satır daha okunaklı olabilir. Devam edelim.

```
arabaEleman.addContent(new Element("model").addContent("Celica"));
arabaEleman.addContent(new Element("sene").addContent("1997"));
arabaEleman.addContent(new Element("renk").addContent("yesil"));
arabaEleman.addContent(new Element("plaka").addContent("HC-176").addAttribute("il", "34"));
```

Farkettiyseniz plaka elemanı dahilinde hem elemanın kendisini hem de
'il' dediğimiz bir özellikde ekledik. Bunun mümkün olmasının sebebi
addContent işleminin gene aynı Element nesnesini geri
döndürmesidir. Böylece zincirleme ekleme yapmak kolaylaşıyor. Bazi
kütüphaneler (!) Element yerine void döndürüyorlar.. Cık cık cık.

Yorum ekleyelim:

```
arabaEleman.addContent(new Comment("Araba tarifi"));
```

Mavcut XML dosyasını işlemekte benzeri bir halde oluyor. Mesela sene
elemanına bir göstergeç almak için Element nesnesi üzerindeki getChild
işlemini kullanmamız lazım.

```
Element seneElemani = arabaEleman.getChild("sene");
```

Bu getChild çağrısı, ilk çocuk elemanı geri getirir. Eğer sene elemanı
yok ise, o zaman null (sıfır) geriye gelir. Ayrıca dikkatinize sunmak
istedim: Geri gelen değeri üst-dönüşümden (upcast) geçirmemiz
gerekmedi (DOM'da öyle olacaktı). Element nesnesinin çocukları aynı
şekilde Element. Ne kadar kolay değil mi? Devam edelim, sene elemanını
belgeden çıkarmayı görelim.

```
boolean cikarildi = arabaEleman.removeChild("sene");
```

Son satır sadece sene elemanını çıkarır, belgenin gerisi aynı şekilde kalır.

Şimdiye kadar gördüğümüz XML dosyalarının yaratılması ve
işlenmesi. Artık biten belgeyi sabit diske yazmanın zamanı geldi.

```
try {
XMLOutputter yazici = new XMLOutputter("  ", true);
yazici.output(benimBelge, System.out);
} catch (java.io.IOException istisna) {
istisna.printStackTrace();
}
```

XMLOutputter nesnesinin bazı formatlama seçenekleri var. Üstte
gösterdiğimiz, her çocuk düğümün, bir üstündeki düğümden iki karakter
sonra geleceği. Ayrıca, her elemandan sonra yeni bir satıra geçmek
istiyoruz. XMLOutputter ya Writer nesnesine, ya da OutputStream
nesnesine yazım yapabilir. Yani normal dosyaya yazmak için aşağıdakini
yapmamız yeterli.

```
FileWriter yazan = new FileWriter("/bir/dizin/benimDosya.xml");
yazici.output(benimDosya, yazan);
yazan.close();
```

Öteki XML Arayüzleri ile Etkileşim

JDOM'un ilginç özelliklerinden biri de, öteki XML kütüphaneleri ile
olan uyumu. JDOM ile Stream ve Reader nesneleri ile konuşabilmenin
yanısıra, SAX EventStream ya da DOM Document nesneleri ile
konuşabilmeniz mümkün. Böylelikle JDOM'u çok-arayüzlü ortamlarda
kullanmanız mümkün oluyor. Ayrıca, daha sonraki örneklerde göreceğimiz
üzere JDOM'un iç yapılarına başka kütüphanelerin erişmesi de mümkün.

JDOM'u kullanabileceğimiz başka bir yer, zaten mevcut olan XML
dosyalarını işlemek. Bunu org.jdom.input paketini kullanarak
yapacağız.

```
try {
SAXBuilder yapici = new SAXBuilder();
Document baskaBelge =
yapici.build(new File("/bir/dizin/ornek.xml"));
} catch(JDOMException i) {
i.printStackTrace();
} catch(NullPointerException i) {
i.printStackTrace();
}
```

İşte SAX'tan gelen bu belgeyi önceki yöntemleri kullanarak
işleyebilirsiniz.

Ayrıca, diğer bir JDOM kullanımı gene Apaçe vakfından Xalan ile
olabiliyor. Mesela ekteki dosyalara bakarak, bir İnternet araba satış
mağazası için İnternet sayfaları yaratacağımızı düşünelim. Bu sayfalar
araba detay bilgisi verecek. Bunu için JDOM Document nesnesi ile
birlikte XSL kullanacağız ve servlet'in OutputStream'ine HTML
basacağız.

Bize lazım olan araba.xsl adlı bir dosya.

```
<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:template match="/araba">
<html>
<head>
<baslik><xsl:value-of select="marka"/> <xsl:value-of select="marka"/>
</head>
<body>
<h1><xsl:value-of select="marka"/></h1><br />
<h2><xsl:value-of select="model"/></h2><br />
<table border="0">
<tr><td>no:</td><td><xsl:value-of select="@no"/></td></tr>
<tr><td>Sene:</td><td><xsl:value-of select="sene"/></td></tr>
<tr><td>Renk:</td><td><xsl:value-of select="renk"/></td></tr>
</table>
</body>
</html>
</xsl:template>
</xsl:stylesheet>
```

Şimdi org.jdom.Document nesnesini DOM Document nesnesine çevirelim, ve
Xalan'a gönderelim. HTML'e nasıl çeviri yapılacağını anlatan XSL
dosyası, ve farz edilen bir uygulama suncusundan gelen servlet
OutputStream nesnesini de ekleyerek tabii ki.

```
TransformerFactory tFabrikasi = TransformerFactory.newInstance();

// XML ve XSLT belgeleri icin giris kaynaklarini yarat.
org.jdom.output.DOMOutputter yazici = new org.jdom.output.DOMOutputter();
org.w3c.dom.Document domBelgesi = yazici.output(benimBelge);
javax.xml.transform.Source xmlKaynagi =
new javax.xml.transform.dom.DOMSource(domBelgesi);
StreamSource xsltKaynagi =
new StreamSource(new FileInputStream("/bir/dizin/araba.xsl"));

// Cikis sonucunu HTTPResponse OutputStream kullanarak yarat.
StreamResult xmlSonucu = new StreamResult(response.getOutputStream());

// XSLT ceviricisi al
Transformer cevirici = tFabrikasi.newTransformer(xsltKaynagi);

// Ceviriyi yap
cevirici.transform(xmlKaynagi, xmlSonucu);
```

Bu örnekte çıkışı Java servlet'ten HTTPResponse OutputStream
nesnelerine verdik. Fakat, bu çıkış geyet rahat bir şekilde normal
dosya da olabilirdi. (XMLOutputter örneğinde gördüğümüz
gibi). Kullandığımız DOMOutputter ile Xalan için XML kaynağı
yarattık. Aynı kaynağı Document nesnesini XMLOutputter kullanarak
String yapmak, ve onu StreamSource'a çevirerekte
yapabilirdik. Esnekliğe bakın. JDOM çıkışı, String, SAX Event, Stream,
hatta DOM Document dosyası bile olabilir. Böylece JDOM'un başka türden
giriş bekleyen yazılımlar ile çalışabilmesi sağlanmıştır.

Birkaç satırda JDOM ile yapabileceğimiz birçok şey var. Bu yazıda
gördüklerimiz XML'i yoktan yaratmak, olanı işleyebilmek, hatta XML
üzerinden HTML sayfasına çevirmek oldu.

Sonuç olarak başlangıçtaki sorumuza gelelim. JDOM, şu anki mevcut
arayüzlerden iyi mi? Eğer rüyalarızı bile Java dilinde gören birisi
iseniz, cevap bizce 'Evet'.

Not: Bu yazının örnek kodları. JDOM, Xerces, ve Xalan'ı CLASSPATH'e
eklemeyi unutmayın.
