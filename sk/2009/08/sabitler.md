# Sabitler

Web uygulamasi sabit degerleri (constants) nasil almali? Bir
properties dosyasindan, muhakkak, fakat bu dosya EAR icinde mi disinda
mi olsa daha iyi?Disinda olmasi daha iyi; boylece bu dosyanin set
edilmesi tamamen derleme sistemi disinda, gelistirme ile alakali
degil, sonuca gonderme (deployment) ile ilgili. Sonuc ortaminda "ant"
olmasa bile, properties dosyasinda degisim yapabiliriz. Alttaki class
sabitleri okumak icin yazildi.

```java
import org.apache.log4j.Logger;
import java.io.IOException;import
java.io.FileInputStream;
import java.util.Properties;

public class Constants
{
    transient Logger log = Logger.getLogger("logger");
    private static Constants _instance = null;
    Properties properties = new Properties();
    public static Constants instance() {
	if (_instance == null) {
	    _instance = new Constants();
	}
	return _instance;
    }

    public Constants() {
	String userHome = System.getProperty("user.home").toLowerCase();
	try {
	    log.debug("userHome =" + userHome + "/etc/pocketbudda.properties");
	    properties.load(new FileInputStream(userHome +"/etc/app.properties"));
	} catch (IOException e) { Util.log(e); }
    }

    public String getString(String arg) {

	return properties.getProperty(arg);
    }

    public int getInt(String arg) {
	return new Integer(properties.getProperty(arg)).intValue();
    }

    public String[] getList(String arg) {
	String s = properties.getProperty(arg); return s.split(",");
    }
}
```

Uygulamamizin sabitleri, diyelim ki app.properties dosyasi, her zaman
`$HOME/etc/` altinda aranir. Mesela JBoss baslatan "ubuntu" adli bir
kullanici ise, `/home/ubuntu/etc/app.properties` okunacak. Sabit
degerleri okumak icin uygulamamizin herhangi bir yerinde
`Constants.instance().getString("anahtar")` gibi bir cagri
yeterli. Dosya

```
anahtar1=deger1
anahtar2=deger2
```

formatinda... Constants class'i listeler, sayilari "tip guvenlikli
(type safe)" bir sekilde okuyabilecek halde.Sabitleri Seam'e vermek
mumkun; uygulama baslayinca muhakkak islemesi garanti bir Action
class'i icine:@OutString showAds =
Constants.instance().getString("showAds");gibi bir tanim "showAds"
sabitini dis dunyaya de-enjekte edecektir. Artik

```
<s:div rendered="#{showAds == 'false'}">
  <img src="[imaj]"/></s:div>
```

gibi tanimlar xhtml sayfalari icinde kullanilabilir.





