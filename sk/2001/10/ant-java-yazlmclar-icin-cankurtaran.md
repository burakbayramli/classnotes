# Ant: Java yazılımcıları için Cankurtaran

Butun yazılım projelerinde bir iş bölümü vardır.Programcılar, kod
yazıp diske saklar test yazıp programın iyi durumda olup olmadiğını
kontrol ederler. Çok yaratıcılık gerektiren bu evrenin önünde ne kadar
az engel varsa o kadar iyi olacaktır. Eğer testler başarı ile
işliyorsa, kodu Kaynak Kontrol Sistemine ekleriz. Böylece yeni kodu
herkes görebilir. Programcılar günün sonunda mutlu bir şekilde
evlerine gidebilirler.Aynı proje içinde, bir, ya da daha fazla
arkadaşınızın görevi, mesela kodu KKİ'dan çekip, İnternet'te kurmak
olabilir. Kurmak derken, mesela www.arabeni.com sitesini,
programcıların yazdığı kodun en son hali kod ile 'güncelleçtirmekten'
bahsediyorum.Amaçlar rahat gibi gözüküyor. Bir kaç tane Servlet
yazdım,

Java dosyalarım var (.JAVA). Hemen bu dosyaları derleyip, çıkan .CLASS
dosyaları İnternet makinesine kopyalarım... Sunucu programı
indirip/kaldırırsam, yeni program işleme girmiş olur.Kulağa basit
gelse de, aslında, yapılması gereken oldukça şey var.* Her makinanın
değişik dizin yapısı olabilir. Kimisinde Java derleyicisi
/usr/local/java altında, kimisinde /opt/java altında olabilir. Derleme
komutunu verince, derleyici her makinede aynı şekilde calışacak mı?*
Programı İnternet'e iletmek için, aynı şeyleri aynı sırada birkaç kere
yapmak gerekebilir.

Mesela program /usr/local/tomcat/webapps/arabeni.com/ altına
gidecek. Sunucu program durdurulup/başlatılacak. Veri tabanı tablo
haritası değişecek.

* Bütün bu işlemleri make programı ile yapabilirsiniz, ama make, C++
programcıları için yazılmış. Java anlamıyor be kardeşim. .CLASS
dosyalari /usr/falan/classes/ altina gitsin istiyorum, JAVA dosyalari
yerinde kalsın. MAKE beni çok uğraştırıyor.

* Symantec, Borland gibi programları aklınızdan çıkarın. Programı
İnternet'te kuracak olan arkadaşınız görsel programlarla uğrasamaz.
Zaten İnternet makinesinde büyük bir ihtimalle görsel hiç bir şey
olmayacaktır (X-windows mesela). Bu makineler daha hızlı olsun, ve
korsan (hacker) arkadaşlarin kırabileceği bir sürü program bulunmasın
diye, gereksiz olan herşey silinmiştir.

* Ama denebilir ki "Bizim Internet ortamı tamamen Windows! Her yerde
görsel programlarımız var bizim".

Fakat, sunucu ortamda Windows kullanmanın zaten başka büyük sorunları
var, ve bu satırlarin amacı bu tip problemlere yardım değil. Unix'e
geçerek script tanımlama özelliği edinmek istiyorsanız, ant gibi
programlar o zaman işinize yarayacaktır.

Çözüm Ant

Üstte listelenen türden sorunlar icin ANT programı cok
uygun. (Google'dan jakarta ve ant yazarak programi nereden
indireceginizi bulabilirsiniz).ANT kullanmak icin, proje basina sadece
bir tane build.xml yazmak yeterli. Mesela, butun Java kayitlarini
derlemek icin, asagidakini build.xml icine yazin.

```
<project name="bizim_proje" default="java" basedir="."> <property
name="classes" value="${build}/WEB-INF/classes" /> <property
name="build" value="${basedir}/build" /> <target name="java"
depends="init"> <javac destdir="${classes}" srcdir="${basedir}/java">
<classpath refid="cp" /> <include name="*.java"/> </javac>
</target>
```

Bu dosya tanimlandiktan sonra, derleme icin$ ant javademeniz yeterli
olacaktir. 'java' diye tanimladiginiz kelime, bir 'hedef
kelime'dir.Neden AntAnt programi ile Make arasinda temel olarak
benzerlikler olsa bile, aslina ant cok ustun bir yazilim. Mesela,
javac kelimesi, bir arac kelimedir. 'Arac' kelimeleri bir araya
getirerek 'hedef kelimeler' tanimlamak mumkun oluyor. Ant, bir cok
arac kelimeyi onceden tanimlamis, bu yuzden tekerlegi bastan icat
etmenize gerek kalmiyor. Mesela, javac arac kelimesi, arka planda bir
Java programi isletir. Bu program, onceden tanimladigi uzere, mesela
/usr/ dizin seviyesinden baslayip, asagiya dogru inip butun java
kayitlarini derler, ve tanimlanan baska bir yere koyar. Butun bunlari
bourne shell ya da make kullanarak yapmaya kalksaniz, script kodunuz
cok uzardi.Ant'in yararli oldugu diger islere gelelim:

* Dosya kopyalama cok rahat

* Onceden tanimli XSL arac kelimesi var.  kullanarak butun XML
dosyalarinizi, bir tek komut ile HTML'e cevirmeniz mumkun.

* Isletim sistemi komutlari isletmek cok rahat arac kelimesi ile bunu
basarabilirsiniz

* Aynen make'de oldugu gibi, hedef kelimeleri birbirine baglamak
mumkun. Mesela bir Java kayidi degistiginde, java hedefi isletilir. Bu
hedef 'baglanmis' diger bir hedef var ise, onun da isletilmesi
gerekir, ve ant bunu otomatik olarak yapacaktir.


