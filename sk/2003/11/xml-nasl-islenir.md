# XML Nasıl İşlenir

Bugünkü yazımızda, XML işlemcileri hakkında yazacağız. Önümüzde ana
iki seçenek var.  * DOM * SAX DOM, SAX'tan daha yaşlıdır. XML
dökümanlarını işlemek için ilk önce DOM kullanıyordu herkes. Fakat
DOM'un bazı dezavantajları var. Mesela, XML dökümanı DOM ile
işlenirse, DOM programı bütün XML kayıdı tamamen hafızaya
alınır. Büyük XML kayıtları için bu işlem yavaş kalabilir.  Eğer bütün
XML dosyasını nasıl olsa işlemek istiyorsanız, o zaman DOM işinize
yarayabilir. Fakat XML dosyasından, sadece "birkaç" özel bölümlerden
okuma yapmak istiyorsanız, SAX kullanmak daha hızlı olacaktır.  SAX,
"vaka" modeli ile çalışır. Vaka modeli, yani Java dilinde 'event'
modeli, normâl alışık olduğumuz, "sıralı işleyen" program modelinden
biraz farklıdır. Sıralı işleyen programda, komutlar sıra ile
işlenir. Programcı bu sırayı kod içinde belirtmiştir, önce şu olacak,
sonra bu.  Vaka modeli içinde, şöyle demek mümkündür. "Ne zaman X
olayı olursa, Y komutunu işlet".  Bu düşünce şeklinin, programlama
stilinize etkileri büyüktür. Artık, sıralı dizi gibi işleyen komutlar
yerine, artık vukuat komutları yazar hale geliyorsunuz.  SAX program
modeli vaka üzerine kurulduğu için, cok hızlı bir şekilde XML işlemi
mümkün olur. Çünkü artık işlemci, bütün dosyayı hafızada tutması
gerekmez; böylece sadece istenen yere gelince, bir "vaka"
başlatır. Sizin vaka komutlarınızda hemen o anda işleme konur. O komut
içinde ne yapacağınız size kalmış. Veri tabanına yazabilirsiniz,
e-posta gönderebilirsiniz, başka bir XML dosyasi içine
kopyalayabilirisiniz.  SAX için yazılmış örnek bir program. Bu program
title başlığında girilmiş bilgileri XML dökümanından buluyor.  class
VakaKomutu extends DefaultHandler{ protected String tag = null;
protected String content = null; public VakaKomutu () { super(); }
public String getContent() { return content; } public String
processFile(String file) throws Exception { VakaKomutu komut = null;
try { //System.out.println("processing .." + file); XMLReader xr =
XMLReaderFactory.createXMLReader(
"org.apache.xerces.parsers.SAXParser"); komut = new VakaKomutu();
xr.setContentHandler(komut); xr.setErrorHandler(komut); FileReader r =
new FileReader(file); xr.parse(new InputSource(r)); } catch (Exception
e) {e.printStackTrace(); throw e;} //System.out.println("returning
content... " + content); if (komut == null ) return "title"; else
return komut.getContent(); } public void startElement (String uri,
String name, String qName, Attributes atts) { if ("".equals (uri)){
//System.out.println("Start element: " + qName); if
("title".equals(tag) == false) { tag = qName;
//System.out.println("=====================");
//System.out.println("tag is " + tag); //System.out.println("content
is " + content); } } else { //System.out.println("Start element: {" +
uri + "}" + name); } } public void characters (char ch[], int start,
int length) { if ("title".equals(tag) && content == null) { content =
new String(ch, start, length); //System.out.println(content); } } }




