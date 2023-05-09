# Perl İle Kod HeykelTraşlığı

Her projeye Perl'ün eli bir kere değmelidir!  Ya da değecektir demek
daha doğru... Kod heykeltraşlığı, yani birden fazla kaynak kod dosyası
üzerinde yapılmış tekrar eden bir hatayı bulup değiştirmek, Perl'ün en
güçlü özelliklerinden biridir. Bunu yapmak için Perl'ün düzenli
ifadeler desteğini kullaniyoruz.  Düzenli ifadeler bir metin üzerinde
"düzenli ifade dili" ile tarif edilmiş aranan bir metin parçasını
bulup, yerine bir başkasını koyabilir. '*' karakteri, hepimizin
işletim sistemi komut satırından bildiği bir düzenli ifade ibaresidir,
ama düzenli ifadeler bundan çok daha çetrefilli olabilirler.  Ayrıca
Perl, dil olarak, güçlü tipleme (stronly typing) kullanmaz, ve işte bu
sebeple betikleme dilleri kategorisine girer. Yani, Perl programları
içinde kullandığınız değişkenleri önceden tanımlamanıza gerek
yoktur. Java, C, güçlü tipli dillerdir.  Değişken tanımlanmasına gerek
bırakmayan diller çabuk ve kısa program yazmaya daha elverişlidir.
Örnek Aşağıdaki örnek, bir projede yapmamız gereken bir kaynak kod
heykeltraşlığı için yazıldı.

Bu projede, kalıcılık (persistence) eşlemede kullandıgımız XML
dosyaları içinde, seri no (sequence) üreten SQL ibaresi, DB2 üzerinde
çalışmamıştı... Eskiden kullanılan ...

```
<sql-text>select max(foo.col) from (select max(id)+1 as col
from TABLOISMI union select 1 as col) as foo
</sql-text>
```

şu halde değişecekti.  ...

```
<sql-text>select (nextval for s_TABLOISMI from sysibm.sysdummy1 </sql-text>...
```

Bu değişimi elle yapmak çok vaktimizi alacaktı. O yüzden şu Perl
kodunu yazdık.

```
chdir ("/test/dizin/conf");
foreach $file(<*.xml>) {
  print ".. " . $file . "\n";
  open IN, $file;
  open OUT,">./degisen/$file";
  while (<IN>) {
  if (/select max\(foo\.col\) from\(select max\(id\)\+1 as col from (.*?)  union select 1 as col\) as foo/) {
    print OUT " select (nextval for s_$1 ) from sysibm.sysdummy1\n";}
  else {
  print OUT;
  }
}
close OUT;
close IN;}
print
"Tamam. Dosyalar Degistirildi.";
```

Satır Satır Açıklama

1. satırda, işletim sisteminde olduğumuz dizini değiştiriyoruz (komut
satırında cd yazmak ile aynıdır). Bundan sonraki Perl komutlarından
dosya, ve dizin ile alakalı her işlem, içinde olduğumuz bu yeni dizini
referans olarak alacaktır.

3. satırda, <*.xml> ifadesi ile bir Perl array geri geliyor.<>
kullanımı, o an içinde bulunduğumuz dizine bakıp, tarife uyan dosya
isimlerini toplamaya yarar. $file(..array..)  ifadesi, bir array'ın
elemanlarını teker teker ziyaret eder, ve sıradaki her elemanı $file
içine alır. Böylece { } arasındaki kod, bu yeni $file'ı kullanarak
işlemler yapabilir.

4. satırda ekrana bilgi olsun diye bazı mesajlar basıyoruz. print "xx"
. "yy" kullanımındaki nokta ilginç olabilir; Perl, metinleri nokta
karakterini kullanarak birleştirmektedir. Java'da bu + ile oluyordu.
5. sıradaki dosyayı işlemeye hazırlanıyoruz. Önce dosyayı okumaya
açmamız lazım.

6. satır, başka hemen alt başka bir dizinde ama aynı isimde değişmiş
haldeki dosya ismini yaratıyor. Bu yeni dosya, yazmaya açılmış.>
karakteri, dosyaya silbaştan yazmak için kullanılır, eğer >> kullansa
idik, dosyaya ekleme yapıyor olacaktık.

7. satırda okumaya açılan dosyanın tüm satırları üzerinde bir döngü
başlattık. Ayrıca, burada kodda gözükmeyen bir bit yeniği var. while
(<IN>) ifadesinde okunan satırın hangi değişkene gittiğini görmüyoruz
değil mi? İşte Perl'ün kodlamayı hızlandırmak için yaptığı şeylerden
biri: Bu 'görünmez' değişken $_ olarak bilinir. Döngü içerisinde
okunan satıra $_ ile erişebiliriz.

8. satırda artık düzenli ifade kullanmanın vakti geldi. İfade içinde
geri bölü işaretinin biraz bolca kullanılmış olduğu belki dikkatinizi
çekmiştir. Bunun sebebi, bazen, düzenli ifade "diline" ait olan
kelimelerin metin olarak kullanılma zorunluluğunda ortaya
çıkmaktadır. Bu gibi durumlarda, geri bölü işareti ile düzenli ifade
motoruna "bu karakter, düzenli ifade komutu değil normak bir karakter"
mesajı vermekteyiz.  Gene 8. satırda, `(.*?)`  kullanımına dikkatinizi
çekerim. Perl'de her düzenli ifade komutu, parantez içine
alınabilir. Alındığı zaman, ve bu ifade metin parçası ile uyuştuğu
zaman (matching), parantez içindeki uyan kısım, diğer Perl
satırlarında düzenli ifade komut sırasına göre `$1, $2, $3, ..` ile
erişilebilir. Örneğimizde sadece bir tane () kullanımı olduğuna göre,
sadece `$1` kullanarak uyan metin parcasına erişebiliriz.

8. satırda kullanılan düzenli ifadenin detayına inmek gerekirse, nokta
`'.'` işareti düzenli ifade dilinde 'herhangi bir karakter
demektir. Bu herhangi bir karakterden birçok kere (* işareti) görmek
istiyoruz. Peki sondaki soru işareti ne için? Onun görevi şudur:
Mesela `"ali ali veli ali veli ali"` içinde `"ali (.*)\s"` tarasak
elimize ne sonuç gelir?  `"ali veli "` diyorsanız yanıldınız. Gelecek
sonuç, "ali veli ali veli " olacaktır. Çünkü, olağan (default) olarak
düzenli ifadeler açgözlü uyum yaparlar. Verdiğiniz ifadenin
uyabileceği kadar çok kelimeyi toplarlar ve döndürürler. Soru işareti
tokgözlü uyum yap demektir, yani `"ali veli "` sonucunu görmek
istiyorsak, `"ali (.*?)\s"` düzenli ifadesini kullanmamız gerekirdi.

9. satırda, uyumun başarısı `if()` ile algılanmış olmalı ki, aradığımız
kod parçasını bu satırda bulduğumuzu farzediyoruz. Ve hemen, print OUT
ile, yazmak için açtığımız dosya içine değiştirdiğimiz SQL ibaresini
atıyoruz. $1 kullanımını işte burada görmekteyiz. Yani yaptığımız,
düzenli ifade ile tablo ismini okuduğumuz dosyadan çekip çıkarmak, ve
bu tablo ismini, yeni bir SQL metninin içine koyarak yazılan dosyaya
yazmaktır.

10. satır, uyum olmayan satırları algılamak içindir...

11. satırda, bu şekildeki "normal" satırlar için satırı aldığımız gibi
aynen yazıyoruz. Peki ama, satır verisi nerede? Aah, gene o gizli
değişken. PRINT OUT, $degisken yerine PRINT OUT kullanılırsa, Perl
kullanılmayan değişkenin $_ olduğunu farzeder. Zaten daha önce IN'den
okuduğumuz satır bu değişkende olduğuna göre, dosya satırını olduğu
gibi yazmak için PRINT OUT ifadesi yeterlidir.  15. ve 16. satırlarda,
sıradaki okunan ve yazılan dosya ile işimiz bitmiş, bu iki dosyayı da
kapatıyoruz.  20. satırda, bütün dosyalar ile işimiz bitmiş, ve
kapanış mesajı veriyoruz.  Bütün bunlardan sonra, işlem yaptığınız
`/test/dizin/conf` dizinine giderek, bir altındaki degisen/
altdizinine bakın, dosyaların değişmiş halini burada göreceksiniz.


Fakat Java gibi bir dilde çoğu zaman, "mantıki anlamda" bir satır,
metin dosyası üzerinde birçok satıra dağılmış olabilir. Aşağıdaki
gibi: import com.sirket.paket.vs;if (bilmemne)soyle boyle;soyle
boyle;if (filan)soyle boyle oyle; Bu dosya üzerinde, yapacağımız
değişim şöyle olabilir: "sadece if komutunu içeren mantıki satırlar
içinde bütün şöyle böyle ibarelerini, böyle şöyle yapalım". Bu gibi
durumlarda, eğer tek satırlı işlem yapma tekniğini kullanıyorsak, if
ve şöyle böyle kelimelerinin ayrı satırlarda olması sebebiyle, her
ikisini de kapsayan bir düzenli ifade yazmamız mümkün olmayacaktı. Bu
yüzden, Perl'ün çok satırlı işleme özelliğini kullanmamız gerekecek.
Hattâ daha da ileri giderek şu tavsiyede bulunacağız: Her türlü kaynak
kod değiştirme projeniz için çok satırlı işlemi kullanın. En basit ve
en zor şartları halledebildiği için olağan seçenek olarak çok satırlı
işlem en iyisidir.  Nasıl Bütün dosyayı aynı anda işleyebilmek için,
dosyanın tümünü bir Perl değişkenine atamamız gerekiyor.

```
open GIRDI, "girdi.txt";
undef $/;
$_ = <GIRDI>;
```

Bu ifadeler ile işlenecek dosyayı açtık. undef $/; ifadesi, Perl'e çok
satırlı işlem yapmasını söylüyor.  Şimdi, çıktı dosyasını açalım.
`open CIKTI, ">cikti.txt";` Ve düzenli ifadeyi işletelim.

```
s/\;(.*?) if (.*?) soyle boyle(.*?)\;/\;$1if $2 boyle soyle$3\;/sg;
```

Bu düzenli ifade, hem bulma, hem değiştirme işlemini aynı anda
yapıyor. İfadenin uyum kısmında en başta ve en sonda bulunan ;
işaretine dikkatinizi çekerim. Bu karakterlerin kullanılış sebebi,
satır sonu kavramı kalmadığı için (satır satır değil, tüm dosyayı bir
kereden işliyoruz), yeni satır başı ve sonu için en uygun karakterin ;
olmasıdır. Bu işlediğiniz dile göre değişik olabilir. Java dilinde her
komut ; işareti ile bittiği için, bu karakterler arasında düşen her
şeyi bir komut olarak kabul edebilmesi normâl olacaktır.  Ve bu tek
düzenli ifade, değişikliği yaptı bitirdi bile. İşlenmiş sonuç $_
içinde bulunuyor. Perl sessiz değisken olan $_ 'nın özelliklerinden
önceki yazıda da bahsetmiştik. Hiçbir değişken tanımlanmadığında o
anki işlemi Perl otomatik olarak $_ üzerinde yapıyor.  Geriye bu
işlenmiş değişkeni dosyaya yazmaktan başka birşey kalmıyor.

```
# yaz
print CIKTI;
# dosyaları kapat
close GIRDI;
close CIKTI;
```





