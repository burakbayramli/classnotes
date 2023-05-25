# Aktiviteler Arası Geçiş

Android kodlamasinda bir ekran bir Activity objesine tekabul eder. Bir
Activity'den otekine gecis su sekilde oluyor;

```java
Intent myIntent = new Intent();
myIntent.setClassName("[paket]", "[tum paketle beraber gecis yapilan class]");

startActivity(myIntent);
```

Peki geçiş yaparken bir yandan bilgi aktarmak istersek ne yaparız? En
basit işleyecek çözüm, ya kaynak Activity'ye ya da hedef Activity'ye
bir "public static" öğe eklemek ve geçiş yapmadan önce gereken veriyi
bu öğeye statik olarak set etmek. Geçiş sonrası gidilen Activity aynı
statik erişimi yaparak gereken bilgiyi alacaktır.Static erişimin
kullanılması çok kullanıcılı bir ortam olan Web kodlaması için uygun
olmayabilirdi.

Fakat unutmayalım, cep telefonu kodlamasında tek kullanıcılı bir
ortamdayız, ve uygulamada dönen her türlü işlem, veri ataması tek
kullanıcı için yapılıyor. Bu yüzden static kullanımın tehlikesi yok.

