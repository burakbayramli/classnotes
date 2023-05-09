# Properties Dosyaları - Java İçin Ayar Yapmak

Genellikle Java, özellikle J2EE servis uygulamalarında, uygulama
programımıza bazı ayar değerleri (configuration) olarak iletmemiz
gerekmektedir. Bazı uygulamalar için bir dizinin 'yerini' ayar olarak
bildirmek gerekebilir. Ya da, sık değişmeyen türden bir internet adres
değerini uygulamamıza ayar olarak bildirebiliriz.  Bu tip değerleri
Java dosyalarının içine kod olarak yazmak yanlış olur. Bu şekildeki
ayarlar, makinadan makinaya değişik içerik taşıyabilirler; bu yüzden
bir metin (text) dosyası olarak tutulsalar, ve Java tarafından
'okunsalar' hem kodlama, hem sistem idaresi, hem de bakım açısından
bizim için daha iyi olacak.  Java dünyasında bu kavramın karşılığı
.properties dosyalarıdır. Bir properties (özellikler) dosyası, anahtar
ve bu anahtara tekabül eden değerler eşlemesinden ibarettir.

Anahtarlar ve değerler eşittir (=) karakteri ile birbirinden
ayrılırlar.

```
ayar1=deger
ayar2=baska_bir_deger
ayar3=gene_baska_bir_deger
dizin1=/usr/local/linux
dizin2=/usr/local/java
```

Bu dosyayı okumak için Java paketlerinden java.util altında bulunan
ResourceBundle sınıfını kullanabiliriz.  Eğer ResourceBundle sınıfını
bir J2EE uygulaması bağlamında kullanıyorsak, dizin belirtmeden sırf
isimle aranan properties dosyaları WEB-INF/classes altında
aranacaktır. Yani, CLASSPATH'e otomatikman alınmış olan bir dizin
altında.  Denemek için, Tomcat, ya da J2EE uyumlu öteki uygulama
servisiniz altında kurduğunuz site kodlarınızın WEB-INF/classes
dizinine, deneme.properties gibi bir dosya bırakın. Bu dosya içine,
yukarıda gösterdiğimiz örnek değerleri koyabilirsiniz. Sonra, test.jsp
adında bir JSP sayfası yaratın, ve şu kodları yazın.

```
<%@ page import="java.util.*" %>
<% ResourceBundle bundle = ResourceBundle.getBundle( "deneme" );
  out.print( "dizin1=" +
  bundle.getString( "dizin1" ) );
  out.print( "ayar2=" + bundle.getString( "ayar2" ) );
%>
```

Bu sayfayı işlettiğinizde, properties dosyasında girdiğiniz
değerlerden ayar2 ve dizin1'in aynen okunduğunu göreceksiniz.  Sonuç
Ortamına Gönderim Eğer uygulamanız için, geliştirme ve sonuç ortamları
arasında değişik ayarlar gerekiyorsa, aynı anahtarlar taşıyan (ama
değerleri ayrı olan) iki değişik properties dosyası tutabilirsiniz. Bu
dosyalardan birisi geliştirme sırasında yerel WEB-INF/classes altında
kullanabilir, ötekini sonuç ortamına kod itildiğinde uzaktaki
makinanın WEB-INF/classes altında kullanabilirsiniz.  Site kodları
altında bunun örneğini Sabitler.java, Sabitler.properties,
Sabitler.properties.sonuc ve icerik_gonder.sh dosyaları altında
görebilirsiniz.  Yukarıda anlatılanlara ek olarak, hızlandırmak
açısından biz site kodları içinde static tekil nesne kalıbını
Singleton kullanarak, properties dosyasını uygulama başladığında
sadece 'bir kere' yüklemeyi seçtik. Kendi uygulamanızın ihtiyaçları
ışığında bunu değişik şekilde kendi sisteminize uyarlayabilirsiniz.





