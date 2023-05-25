# Dinamik Resource Erişimi

Android'de referans bilgilerini bir dosya içinde tutabiliriz; bu
dosyalara erişmek için onları res/raw altında tutuyoruz. Eğer
erişilecek dosya ismi dinamik bir şekilde üretiliyor ise, o zaman
String tipinden bir şekilde R.java dosyası içinde static olarak
tanımlı ınt ögelere eşlemena yapmamız lazım. Bnnun için Java Reflection
kullanacağız:

```java
import java.lang.reflect.Field;
import java.io.InputStream;
Field id = R.raw.class.getDeclaredField([string tipinde dinamik oge ismi]);
R.raw rr = new R.raw();
int i = id.getInt(rr);
InputStream in = getResources().openRawResource(i);
...
```

