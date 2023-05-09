# JAD - Java İle Tersine Mühendislik

Java dilinin sözdizim yapısı diğer dillerden farklı olduğu gibi, işler
kod sistemine de getirdiği bazı farklar var. Bu farkların bir tanesi
olan arakod kavramından bahsetmiştik. Bu tekniğe göre Java
derleyicisi, Unix işler kodu ya da Windows .exe dosyası değil, .class
dosyası denen bir işler kod çıkartıyordu. Bu kodlar, Java
yorumlayıcısı tarafından işletilmek üzere üretilen bir şey idi, bu
yüzden işletim sistemi bu kodları anlayamıyordu.
    
Yorumlayıcı kullanmanın yararlarından bir tanesi: .class dosyalarını
aynen kopyalayıp başka bir işletim sisteminde değistirmeden
çalıştırabilirsiniz. Bazı modern Uygulama Servisleri, Java
arakodlarını anlık bir halde başka sisteme postalayıp orada
işlettirebiliyorlar. Bu durum, bir nevi 'gezgin kod' kavramını ortaya
çıkarmıştır. Bunlar Java'nın 'işletim sisteminden bağımsız' yapısı
sayesinde mümkün oldu.

Tabii arakod ve yorumlayıcı kavramları bilgisayar bilim dünyasına hiç
yabancı değil. Eiffel, Smalltalk ve LISP dilleri bu kavramları uzun
zamandır kullanmaktaydılar. C dünyası biraz geride kalmıstı! (Java ile
onlar da yetiştiler).
  
Bu yazıda bahsedeceğimiz konu, .class dosyalarına bakarak, .java
dosyaları üretebilmek, yani tersine mühendislik teknikleri. Bu gibi
yeteneklerin programcının dağarcığında olması çözüm yelpazesini
genişletir, ve çözüm yaratmakta esneklik sağlar. Ayrıca bir sonraki
yazımızda, ters mühendislik tekrar işimize yarayacak.
 
JAD 
  
JAD komut satırından işletebileceğiniz bir program. Tek tek ya da
bütün bir dizin altındaki .class dosyalarını Java kaynak haline
çevirebiliyor. Bunu yapabilmesinin sebebi, arakod'un Java'ya özel
tiyo'lar içermesidir; Nisbeten makine dili, mikroişlemciye özel
yazılır.

[Google'dan bulup indirin] 
    
Şimdi, mesela örnek olarak, bütün bir .class dizinini Java'ya
çevirelim. `c:\jdk1.3.1_04\jre\rt.jar` içindeki kodların kaynağını hiç
merak ettiniz mi? Görelim bari.

İlk önce, bu jar dosyasını alıp, geçici bir dizine bırakalım. Geçici
dizin ismi `c:\gecici` olsun..Şimdi

```  
> cd c:\gecici
> jar xvf rt.jar
```
    
.. komutu ile, jar dosyasını 'açıyoruz'. jar dosyası zip formatı gibi
birçok dosyayı birarada tutabilen bir ortamdır. Eğer komut satırınız
jar komutunu bulamıyorsa, PATH sistem değişkenine JDK bin dizininin
yerini söylemeyi unutmayın. java, javac gibi komutları, satırdan direk
işletebilmeniz lazım.
   
Not: Biraz önce açtığımız jar dosyası, Sun (Java'nın yaratıcısı)
firması tarafından yazılmış olan bir dosyadır. İçinde Java'nın
işlemesi için hayati önem taşıyan temel nesneler bulunuyor. Mesela
'temel tipler' denen Object, Integer, vs gibi nesneler, hep rt.jar
içinde tanımlanmıştır. Şimdi JAD kullanarak bu nesnelerin kodlarını
göreceğiz!

Komut satırında, eğer geçici dizininiz içindeyseniz, aşağıdaki komutu
işletin. Ayrıca, dosyaları düzgün şekilde birarada tutmak için,
`C:\gecici` altından mesela `c:\gecici\class` altına kopyalayın. Ve,

```  
> cd c:\gecici
> jad -o -r -sjava -dkaynak class/**/*.class
```
  
..ile JAD'i özyineli bir şekilde işletmiş olduk. `C:\gecici\kaynak`
dizini altında, Sun'ın yazdığı Java kodlarının kaynaklarını
görebilirsiniz. Mesela, şu çok kullandığımız String nesnesi,
`C:\gecici\kaynak\java\lang\String.java` altında.. Daha birçok Java
karakterinin kaynağı artık `C:\gecici\kaynak` altında.









