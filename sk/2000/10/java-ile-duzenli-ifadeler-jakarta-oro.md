# Java İle Düzenli İfadeler - Jakarta ORO

Düzenli ifadeler uzun bir süre, Perl, Awk gibi dillerin tekelinde
kaldı. Unix sistem idarecileri betiklerinde metin bazlı değiştirme
işlemlerine çok ihtiyaçları olduğu için, tercih ettikleri diller
düzenli ifadeleri (regular expression) destekleyen dillerden
oluyordu. Perl, Awk (şimdi de Rubi) bu dillerdendir.  Hatırlatmak
gerekirse, düzenli ifade işleyicileri bir metnin içindeki kelime,
karakter dizisini ifade ile eşleyerek, uyan bir kalıbı bulup metin
içinden çıkartıyordu. Daha basit olmakla beraber, işletim sistemi
komut satırınızda düzenli ifadenin bir şeklini zaten
kullanmaktayız. ls *.xml gibi bir ibare, aslında bir düzenli
ifadedir. Tabii Perl ve Rubi'de kullanılanlar kadar değişik güçlü
değildir.  Java dünyası da, düzenli ifade desteğini dağarcığına
nihayet ekledi. Jakarta kütüphanesine dahil olan ORO sayesinde artık
Java ile düzenli ifade işletebilmeniz mümkün olacak.  Örnek Sitemiz
kodu ile örnek verelim.  Mesela, sitemizde makâle olan XML
dosyalarının içindeki bütün JPG ve GIF resim referanslarını bulup,
ArrayList olarak döndürmemiz gereksin. XML içindeki referans sözdizimi
şöyledir.

```
<normalresim kaynak="/images/resim_1.gif"/>
```

Bu referans içinden "sadece resim dosyasının ismini" çıkartmak için,
şöyle bir ORO kodu lazımdır.

```java

// butun dosyayi hafizaya yukle

File f = new File("c:/dizin/yazi.xml");

FileReader oku = new FileReader(f);

char[] icerik = new char[f.length()];

oku.read(icerik);

String bellekteDosya = new String(icerik);

oku.close()

// simdi ORO kullanmaya baslayabiliriz

ArrayList sonuc = new ArrayList();

PatternMatcherInput giris = new PatternMatcherInput(bellekteDosya);

PatternCompiler derleyici = new Perl5Compiler();

PatternMatcher arayici = new Perl5Matcher();

String ara = "/images\\/(.+)\"/";

Pattern ifade = derleyici.compile(ara);

while(arayici.contains(giris, ifade)) {
  sonuc.add(arayici.getMatch().group(1));
}
```

Bu ifade, "images/" kelimesinden sonra ve "/ karakterlerinden önce
gelen kelimeyi bulup çıkartıyor.  Ek Konular Aşağıdaki işaret \\
kullanımı karışık gelmiş olabilir. Bu işaret, kaçış (escape)
işaretidir, çünkü bazen düzenli ifade diline dahil olan karakterleri
eşlememiz gerekmektedir. Bu yüzden düzenli ifade motoruna, "bu
karakterler dil olarak sana mesaj değil, eşlemen için veridir"
komutunu vermiş oluyoruz.  Ayrıca PatternMatcherInput kullanımı, bir
Metin (String) içinde birden fazla eşleme yapmak için
kullanılır. PatternMatcherInput nesnesi, okuduğu metin üzerinde nerede
kaldığını hatırlayabildiği için, birden fazla eşlemeyi
yapabiliyor. Diğer gördüğümüz ORO kullanımları satır bazlı eşleme
yapıyordu, bizim kullanımız için bu şekilde PatternMatcherInput şekli
daha rahat olmuştur. Kendi ihtiyaçlarınız için örneklere bakınız.
Group komutu Bir önemli nokta da arayici.getMatch().group(1) ifadesi
hakkında.  Perl düzenli ifade diline göre (ORO'da bu dili kullanıyor),
eşlenen değeri geri getirmenin iki yolu var. Birincisi, eşlenen
ifadeyi tamamen geri getirmek. İkincisi, düzenli ifade içinde parantez
kullanmıs ise, sadece bu parantezlerin içine düşen eşlemeyi geri
getirmektir. Perl dilinde bu özellik $1, $2, .. ile geri
getirilebiliyordu. ORO ile aynı işlevi group(1), group(2), ... ile
hallediyor.

Kelime Değiştirmek Eşlemek gibi, eşlenen değerleri "değiştirmekte"
düzenli ifadelerin yapabildiği şeylerden biri.  Mesela yukarıdaki
örnekte olan "images" kelimesini, "resimler" kelimesi ile
değiştirelim.

```java
// dosyayi tekrar hafizaya yukle ...
String degistir = "s/images/resimler/g";

Perl5Util yardimci = new Perl5Util();

bellekteDosya = yardimci.substitute(degistir, bellekteDosya);
```
