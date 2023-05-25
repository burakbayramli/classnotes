# Android ve Statik Dosyaları Okumak

Android uygulamamiz, pek cok diger program gibi, bazi referans verilerine erismek isteyebilir. Android isletim sistemi bir sqllite adinda SQL tabani iceriyor, bu taban uzerinde db, tablolar yaratip o tablolara statik veriler yuklemek mumkun. Fakat ihtiyaclarimiz o kadar sofistike SQL erisimi gerektirmiyorsa, o zaman verileri pur dosyalar olarak cep telefonunda saklayabiliriz.

Daha onceki yazida gelistirme dizininin iskeletini yaratabildigimizi gorduk. Bu dizin ile beraber res/ adli bir dizin yaratiliyor, bu dizin altinda ./raw/[dosya] olarak yaratacagimiz her dosyaya Java kodundan su sekilde erismek mumkun.

```java
import android.content.res.Resources;
import java.io.InputStreamReader;
import java.io.BufferedReader;
import java.io.InputStream;

InputStream in = getResources().openRawResource(R.raw.[dosya]);
BufferedReader br = new BufferedReader(new InputStreamReader(in));
String line = "";
while((line=br.readLine())!=null){
 ...
}
```

Bu kod Activity class'indan miras alan bir class icinde olmali -
getResources() oradan erisilebiliyor (ama getResources() ile alinan
obje parametre olarak istenen yere set edilebilir tabii).

Ustteki ornekte res/raw/[dosya]'yi actik ve her satiri bir String
icine atadik.

Uygulamamizi telefona gonderirken paketleme islemleri res/ altindaki
kaynaklarin, dosyalarin paketlenip kod ile beraber gitmesini
saglayacak. Yani "ant install" "ant reinstall" komutlari gerekli her
seyi telefona gondermekte.





