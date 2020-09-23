# İnternet Yazılımları için Örnek Mimari

İnternet bazlı program yazmaya karar verdiniz. Müşteriniz kapıda, ya
da patronunuz soruyor "Hangi teknolojileri kullanacaksın bu iş için?"
Teknolojiden sonra "hangi mimariyi kullanalım" sorusu kapımıza
gelecek. Yemek yapmak ile kıyaslamak gerekirse, teknoloji yumurta,
domates gibi ana maddelerdir, mimari ise "yemek tarifidir". Hangi
tarif daha iyidir? JSP içine JDBC'mi koyalim, yoksa JDBC kodunu Java
nesneleri icine mi koyalım? Sonra JSP->nesne->JDBC->Veri tabanı gibi
olsun.. Ne kadar çok soru!  Bu soruları her proje başında, teknik
liderler kendilerine sorarlar. Yanlız değilsiniz, merak
etmeyin. Soruları sıralayalım: * Hangi teknoloji?  * Genel Mimari
nasil olsun?  * Geliştirme ortamını nasıl kuralım, programcıların
gündelik geliştirme işlemi nasıl kurulsun * Sonuç ortamı hangi işletim
sistemi üzerinde çalışsın?  * Sonuç ortamını biz mi yönetelim, yoksa
Site Barındırma şirketine mi verelim Bu yazımızda bu sorulara cevap
vermeye calisacağız. Alt kattan baslayarak, yukarıya dogru çıkalım:
Ara Kat (Alt) Alt arakat dahilinde veri tabanına bağlanmamız lâzım. Bu
kat içinde, normal Java nesneleri ile JDO ya da Hibernate
kütüphaneleri kullanılarak veriye bağlanılabilir. Daha önceki bir
yazımızda JDO metodunu işledik. JDO nesneleri, aynen bildiğiniz Java
nesnelerine benziyor, çok basitler. Sadece veri deposu görevini gören
bu nesneler 'akıllı' olmayabilirler. Üzerlerindeki işlemler sadece
get/set işlemleri olacak. (Not: get/set Turkceye çevirmek isterdik,
fakat Java tarifnamesi buna izin vermiyor. Bazı durumlarda get/set ile
başlayan kelimeler kullanmak zorunlu).  İşte JDO ve Hibernate için
hazır bir nesne modeli..  public class Musteri{protected String
no;protected String isim;protected Portfoy portfoy;public String
getNo() { return no; }public setNo(String no) {this.no = no; }// oteki
get/set islemleri buraya...}public class Portfoy{public List
gecmisIslemler;// get/set islemleri buraya...}public class
Islem{protected Senet satilanSenet;protected Senet alinanSenet;//
get/set islemleri buraya...}public class Senet{protected String
sirketIsmi;protected long kacLotAlindi;//get/set islemleri buraya...}
Nesneler tanımlandıktan sonra, veri tabanı tablolarına bağlantıyı
kuralım. Bunu, bir XML ayartanım dosyası kullanarak yapacağız. (Sadece
müşteri icin tanımladık, tüm tanımlama değil) JDO için; <?xml
version="1.0"?><!DOCTYPE jdo SYSTEM "jdo.dtd"><jdo><package
name="sk"><class name="Musteri"> <field name="no"/> <field
name="isim"/></class></package></jdo> Hibernate için; <?xml
version="1.0"?><!DOCTYPE hibernate-mapping SYSTEM
"http://hibernate.sourceforge.net/hibernate-mapping-2.0.dtd"
><hibernate-mapping package="sk"><class name="Account" table="Tablo">
<id name="bizimId" column="bizimId" type="int"> <generator
class="increment"/> </id> <property name="no" column="no" /> <property
name="isim" column="isim" /> </class></hibernate-mapping> Bundan sonra
JDO ya da arayüzlerini çağırarak, nesneleri istediğiniz veri tabanına
yazabilirsiniz. JDO için makePersistent, Hibernate için save
işlemleri, tek başına bütün nesneyi alıp veri tabanına yazacak
güçtedir.  Ara Kat (Üst) Ara kat dahiline, JDO nesnelerini kullanan
tabaka da girer. Bu tabakayı proje büyüklüğüne göre es geçebilirsiniz,
o zaman JSP/Servlet/Struts Action'lar direk Java/JDO ile bağlantı
kurar. Yok eğer büyük bir proje ise, bir arayüz daha çekmenin mahsuru
yok.  Arakat dahilinde kullanılması uygun teknoloji EJB Session Bean
olacak. Entity Bean kullanmaya gerek yok, Entity Bean ve JDO
birbirinin rakibi teknolojilerdir, ve JDO'nun daha rahat olduğunu
sanırım ispatladık. EJB Entity Bean katiyen kullanmayın. Session Bean
metodu, sayfalar ile veri tabanı arasında bir arayüz oluşturma
bakımından önemlidir. Dış dünyaya göstereceğiniz arayüz budur
yani.. Dış dünya derken, JSP yerine Swing bile kullanabilirsiniz.
Tavsiyem, üst arayüzleri geniş tutmanızdır. Yani bir işlem, tek
çağrıda çok iş yapsın, veri geriye getirebileceği kadar veri geri
getirsin. Örnek aşağıda: public class HesapBean{public Collection
musteriPortfoyGecmisIslemleriGetir(String musteriNo) {...}}
MusteriPortfoyGecmisIslemleriGetir işlemi içinde, JDO bağlantısı
yapıp, get/set ile istediğiniz nesneyi kullanabilirsiniz.  Sayfalar
Her modern internet yazılımı, sunucu sayfa metodu denen bir metod
kullanıyor bu günlerde. Mesela PHP, JSP bunlardan sayilabilir. JSP,
yani Java Sunucu Sayfaları, kodu sunucuda işletip, tarayıcı
programınıza görsel bilgileri gönderir. Kıyas etmek gerekirse,
Javascript kodu tarayıcı içinde işletilir, sunucu ile alakası yoktur
yani.  İlk projeniz icin salt JSP işinizi gorur. JSP kullanarak
<jsp:include>, <% %> icinde Java kullanarak güçlü programlar
yazılabilir. Eğer daha kuvvetli bir sayfa dili kullanmak isterseniz,
J2EE, istediniz sayfa kütüphanesini (dilini), JSP altında kullanmaniza
izin veriyor. Mesela JSTL ve Struts bu sayfa dillerinden
bazılarıdır. Apache projesi altındaki bu diller cok kullanışlıdır.
Alt katlardan bahsettik: Buna göre, JSP/Servlet/Action icinde, EJB
baglantısı kurup musteriPortfoyGecmisIslemleriGetir işlemini
çagırmanız lazım olacak. Geri gelen Collection (liste) içinde
ihtiyacınız olan veriyi bulabilirsiniz, JSP ya da Struts komutları
kullanarak veriyi görsel bir biçimde ekrana nakledin.  Özetle...  *
Veri tabanı: Oracle, MySql, ya da Pür Java JDO'ya Hazır Veri Tabanı -
ObjectDB * Nesne/Veri Bağlaşımı: Hibernate, TJDO (Serbest Yazılım),
Kodo (Ticari) * Alt Ara Kat: WebLogic JBoss ya da herhangi bir J2EE
uyumlu sunucu programı * Üst Ara Kat: Tomcat WebLogic , ya da herhangi
bir J2EE uyumlu sunucu programı * Görsel Katman: Tomcat, WebLogic veya
Tomcat üzerinde JSTL, Struts veya JSP




