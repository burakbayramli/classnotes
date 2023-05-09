n# Dinamik Resource Erisimi

Android'de referans bilgilerini bir dosya icinde tutabiliriz; bu
dosyalara erismek icin onlari res/raw altinda tutuyoruz. Eger
erisilecek dosya ismi dinamik bir sekilde uretiliyor ise, o zaman
String tipinden bir sekilde R.java dosyasi icinde static olarak
tanimli int ogelere eslemena yapmamiz lazim. Bnnun icin Java Reflection
kullanacagiz:

```
import java.lang.reflect.Field;
import java.io.InputStream;
Field id = R.raw.class.getDeclaredField([string tipinde dinamik oge ismi]);
R.raw rr = new R.raw();
int i = id.getInt(rr);
InputStream in = getResources().openRawResource(i);
...
```




