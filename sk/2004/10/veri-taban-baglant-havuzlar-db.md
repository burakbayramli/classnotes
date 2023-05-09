# Veri Tabanı Bağlantı Havuzları (DB Connection Pools)

Bilgi işlem uygulamarının yapabileceği en pahalı işlemlerden biri,
veri tabanından bir bağlantı (connection) almaktır. Özellikle Oracle
gibi bir taban mevcut, ve güvenliğe önem veren bir de admin var ise,
bağlantı alma sırasında birçok güvenlik kontrolü, hafıza hazırlama,
vs. gibi işlemler bağlantı verme anında yapılacağı için, bu, bağlantı
alma anını en pahalı bilgi işlem hareketlerinden biri hâline
getirmektedir.

Durum böyle olunca, her işlemi (transaction) hızlandırmayı hep
hedeflemiş olan Bİ programcıları, her işlem başında baglantı alıp her
işlem sonunda bağlantı kapatmaktan kaçınırlar. Bağlantılar önbellekte
(cache) tutularak sadece tüm uygulama başında açılırlar ve uygulamanın
işleyişi boyunca açık tutulurlar.

Bunu yaparken, bağlantıların "bir yerde" tutulması gerekir, ve açılıp
kapanmasalar bile bir ortak yerden alınıp geri verilmelidirler. Çünkü
her veri tabanının bir kerede destekleyebileceği/servis verebileceği
bağlantı sayısı belli bir sayıdır, ve bu sayının üstünde de servis
vermek performans açısından imkansızdır.

Havuzlama tekniği burada yardımcı olur. Veri tabanı bağlantıları bir
havuzda tutulurlar, lazım olunca havuzdan alınıp havuza geri
verilirler.
  
Piyasada Java/JDBC için yazılmış havuz kütüphaneleri
mevcuttur. Bunlardan biri Jakarta projesinden DBCP diğeri Proxool adı
verilen açık yazılım projelerdir. Bu projelerden ikisi de, veri
kalıcılık aracı olan diğer bir önemli proje Hibernate tarafından arka
planda kullanılabilmektedir. Biz, kendi ihtiyaçlarımız için pür JDBC
kullanan bir proje için bu iki havuz projesini testten
geçirdik. Sonuçları burada paylaşmak istiyoruz.

Kullanım 
    
Her iki havuz projesi de, programcının işini rahatlatmak için, JDBC
bağlantısı üzerinde çağrılan close() komutuna çengellenmeyi (hook)
seçmişler. Yâni, ta ilk başta bağlantıyı verirken java.sql.Connection
nesnesi yerine bir benzerini koyuyorlar, ve close() komutu çağrılınca
bu taklit nesne bağlantıyı kapatmak yerine, bağlantıyı havuza geri
döndürüyor. Böylece bağlantıların açık kalması sağlanıyor.
  
Ayrıca, havuz kütüphaneleri, bütün bağlantılar ellerinde olduğu için,
bu baglantıların açık olup olmadığını belli aralıklarla kontrol etme,
havuzda olandan fazla bağlantı vermeme gibi kontrolleri de
yapabiliyorlar.
  
DBCP 
  
DBCP ile başlayalım. Bu havuzu kurmak için gereken dosyaları allta, ve
jar'ları almak icin gereken internet bağlantılarını en alttaki
Kaynaklar listesinde bulabilirsiniz.

```  
classes12.jar -- oracle için
commons-collections-3.1.jar
commons-dbcp-1.2.1.jar
commons-pool-1.2.jar
mysql-connector-java-3.0.6-stable-bin.jar -- mysql için
 ```
  
DBCP'de bağlantı açmak için en basit yol, şöyledir:


```
import org.apache.commons.dbcp.*;

BasicDataSource ds = new BasicDataSource();
ds.setDriverClassName("oracle.jdbc.driver.OracleDriver");
ds.setUrl("jdbc:oracle:oci:@rcat53");
ds.setMaxActive(ustLimit);

// baglanti alirken ne kadar bekleyelim?
ds.setMaxWait(1000);

ds.setUsername("kullanici");
ds.setPassword("sifre");

// test sorgusu
ds.setValidationQuery("SELECT DUMMY FROM DUAL");

ds.setNumTestsPerEvictionRun(3);
ds.setTimeBetweenEvictionRunsMillis(1000);

// baglantiyi geri alirken canli olup olmadigini kontrol edelim mi
ds.setTestOnReturn(true);

// ilk alirken kontrol edelim mi
ds.setTestOnBorrow(true);

// baglanti bos dururken (havuzda) arada sirada
// canli olup olmadigini kontrol edelim mi
ds.setTestWhileIdle(true);

Connection baglanti = ds.getConnection();
``` 
 
Proxool 
  
Gerekli dosyalar 

```  
classes12.jar -- oracle için
kostur.bat
mysql-connector-java-3.0.6-stable-bin.jar -- mysql için
proxool-0.8.3.jar
proxool.properties
```

Proxool gerçekleştirim olarak benzer, kullanım olarak DBCP'den biraz
değişik bir yaklaşım takip etmiş. Proxool'un en rahat kullanımı bir
ayar dosyası properties dosyasından ayarları okuyarak, havuzu kurulup,
kontrollerin yapılması. Bunun için, properties dosyası şöyle oluyor.

```  
jdbc-0.proxool.alias=app
jdbc-0.proxool.driver-url=jdbc:oracle:oci:@SID
jdbc-0.proxool.driver-class=oracle.jdbc.driver.OracleDriver
jdbc-0.user=kullanici
jdbc-0.password=sifre
jdbc-0.proxool.maximum-connection-count=3
jdbc-0.proxool.house-keeping-test-sql=select '1' from DUAL
``` 

Bundan sonra Java seviyesinde bağlantı havuzunu kurmak ve bağlanti
almak icin şunlar yeterli:

```  
import org.logicalcobwebs.proxool.*;
import org.logicalcobwebs.proxool.configuration.PropertyConfigurator;
..
PropertyConfigurator.configure("proxool.properties");
..
Connection baglanti = DriverManager.getConnection("proxool.app");
```
 
Bazı Notlar 
  
Son olarak performans konusunu tartışmak gerekiyor. 10,000 tane
bağlantı arkası arkasına alıp/veren bir test üzerinde elde ettiğimiz
rakamlar, Proxool'un daha hızli olduğunu ortaya koydu (6,000 küsur
milisaniye). DBCP 20,000 milisaniye gibi bir performans ile ikinci
geldi. Bu iki rakamda web uygulamaları için kabul edilir rakamlardır,
fakat milisaniyelerin hesaplandığı ortamlarda Proxool'un tercih
edilmesi daha uygun gibi gözüküyor. Ayrıca, bilgi ağının çökebileceği
durumlar için yapılan testlerde, Proxool'un kopmayı farkederek, ağ
bağlantısı geri gelince otomatik olarak veri tabanına tekrar
bağlanabildiğini gördük.

Bilgi olarak eklemek gerekir ki, Tomcat arka planda DBCP
kullanmaktadır. Her iki uygulama da, daha önce bahsettiğimiz gibi,
Hibernate tarafından kullanılmaktadırlar.
  


