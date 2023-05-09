# Log4j İle Log Etmek

Java Programlarımızın hatalarını tamir için, ne yaptığını görmemiz
gerekir. Görsel, kullanıcı bazlı, tamamı tek servis içinde işleyen
kodları hata ayıklayıcı (debugger) gözetiminde işletmek kolaydır,
fakat servis bazlı, dağıtık, birden fazla thread ile çalışan
programları ayıklayıcı içinde başlatmak ve hatasını bulmak zordur.

Çözüm olarak koda System.out.println ekleyerek program hakkındaki
bilgi üretiriz. Bu çıktıyı bir "dosyaya" yönelterek kalıcı günlük
tutabiliriz. Fakat bu basit bir çözümdür, komut satırı seviyesinde
dosyaya yöneltemeyi ayrı yazmak gerekmektedir, ve hangi mesajın
yazılıp yazılamayacaği hakkında elimize kontrol vermez.

Log4j ile bunların hepsini elde edebiliriz.

Log4J

Log4J, hata ve bilgilendirme mesajlarımızı yaratmamızı sağlayan
API'lar içeriyor. Bu arayüzleri mesaj gereken her yerden
çâğırebiliriz. Önemli hatalar için error(), bilgilendirme için
debug(), ve uyarı mesajları için warn() kullanabiliriz.

Bu çağırımlar kodumuzun içinde kalır. Hangi tür mesajların yazılıp
yazılmayacağını log4.properties dosyasından kontrol ediyoruz. Daha da
ilginci, bu kontrol sınıf seviyesinde bile yapılabiliyor, yani,
istersek "sadece A sınıfından gelen mesajları yaz, B sınıfından
gelenleri yazma" gibi bir log4j.properties ayarı bile yapmak mümkün.

Sınif seviyesinde tanım yapmak istemezsek, en üst paket seviyesinde
yapılan tanım, onun altındaki sınıflar için de
kullanılacaktır. com.test adlı paketin debug() ve daha üst (yani
error()) seviyesindeki mesajlarını görebilmek için,

log4j.logger.com.test=debug

tanımı gerekiyor, com.test paket ismi, log4j.logger öneki günlükleme fonksiyonu için kullanılmış.

Root logger, çıkış ortamının ne olduğunu belirliyor. Eğer ekrana basmak istiyorsak,

```
### günlük mesajlarını ekrana (stdout) yönlendir ###
log4j.appender.stdout=org.apache.log4j.ConsoleAppender
log4j.appender.stdout.Target=System.out
log4j.appender.stdout.layout=org.apache.log4j.PatternLayout
log4j.appender.stdout.layout.ConversionPattern=%d{ABSOLUTE} %5p %c{1}:%L - %m%n

log4j.rootLogger=warn, stdout
```

Eğer dosyaya yazman istiyorsak,

```
### günlük mesajlarını dosyaya (bizimgunluk.log) yönlendir ###
log4j.appender.file=org.apache.log4j.FileAppender
log4j.appender.file.File=bizimgunluk.log # buraya herhangi bir isim olabilir
log4j.appender.file.layout=org.apache.log4j.PatternLayout
log4j.appender.file.layout.ConversionPattern=%d{ABSOLUTE} %5p %c{1}:%L - %m%n

log4j.rootLogger=warn, file
```

Eğer tek bir dosya gereğinden fazla büyüyorsa, birden fazla dosyaya
yazmak için, aşağıdaki tanımları kullanın. Hattâ sonuç ortamında
dönüşümlü (rotating) günlük dosyaları en tercih edilir durumdur.

```
log4j.appender.R.File=gunluk.log

log4j.rootLogger=debug, stdout, R

log4j.appender.stdout=org.apache.log4j.ConsoleAppender
log4j.appender.stdout.layout=org.apache.log4j.PatternLayout

log4j.appender.stdout.layout.ConversionPattern=[%d{dd MMM yyyy HH:mm:ss}] SMSGW%8p (%F:%L) - %m%n
#log4j.appender.stdout.layout.ConversionPattern=[%d{dd MMM yyyy HH:mm:ss}] %5p (%F:%L) - %m%n

log4j.appender.R=org.apache.log4j.RollingFileAppender

log4j.appender.R.MaxFileSize=5000KB
log4j.appender.R.MaxBackupIndex=25

log4j.appender.R.layout=org.apache.log4j.PatternLayout
log4j.appender.R.layout.ConversionPattern=[%d{dd MMM yyyy HH:mm:ss}] %5p - %m%n
```

Apache Commons

Demo'dan da görebileceğiniz gibi, Log4J ile beraber, bu örnek için
Apache Commons Logging adlı bir yardımcı paketi de Log4J ile beraber
kullandık. Günlükleme fonksiyonlarını basitleştiren bu yardımcı paketi
kullanmanızı tavsiye ediyoruz. Gerekli olan kütüphaneler
commons-logging.jar, junit.jar, log4j.jar yerlerine konduktan sonra,
günlüğe yazmak isteyen her sınıf önce iki Apache Commons sınıfıni
import eder.

```
...
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

public class HerhangiBirSinif {
...
}
```

Bu sınıflar alındıktan sonra, debug mesajları için
```
    if ( log.isDebugEnabled()) log.debug("Bu bir debug mesajidir");
```

Uyarılar için

```
    log.warn ("sadece bir uyari");
```

Exception atıldıktan ve catch() içinde tutulduktan sonra önemli hata
bildirmek için

```
    ...
} catch (Exception e) {
  log.error("Burada ciddi hata cikti", e);
}     
..
```

şeklinde kullanılabilir.


